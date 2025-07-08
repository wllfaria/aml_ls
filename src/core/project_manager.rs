use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use aml_config::Config;
use aml_core::workspace::get_root_template;
use aml_semantic::{
    SemanticAnalyzer, SymbolType,
    global_scope::{GlobalScope, GlobalSymbol},
};
use aml_syntax::ast::{AstVisitor, Component, Declaration};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::*;

use crate::core::document_manager::DocumentManager;

#[derive(Debug, Default)]
pub struct Templates {
    pub registered: HashMap<String, PathBuf>,
}

impl Templates {
    pub fn register(&mut self, name: String, path: PathBuf) {
        self.registered.insert(name, path);
    }

    pub fn has_template(&self, name: &str) -> bool {
        self.registered.contains_key(name)
    }

    pub fn is_empty(&self) -> bool {
        self.registered.is_empty()
    }
}

#[derive(Debug)]
pub struct ProjectManager {
    pub config: Arc<RwLock<Config>>,
    pub templates: Arc<RwLock<Templates>>,
    pub root_uri: Arc<RwLock<Option<Url>>>,
    pub global_scope: Arc<RwLock<GlobalScope>>,
    pub document_manager: Arc<RwLock<DocumentManager>>,
}

impl ProjectManager {
    pub fn new() -> Self {
        Self {
            config: Default::default(),
            root_uri: Default::default(),
            templates: Default::default(),
            global_scope: Default::default(),
            document_manager: Default::default(),
        }
    }

    pub async fn initialize(&self, params: InitializeParams) {
        // TODO(wiru): LSP support for multiple workspace roots, but for now we assume there will
        // only ever be one. Which for now is fine since most likely there will only be one
        // anathema project per rust project.
        let root_uri = match params.root_uri {
            Some(uri) => Some(uri),
            None => params
                .workspace_folders
                .as_ref()
                .and_then(|folders| folders.first().map(|folder| folder.uri.clone())),
        };

        let root_uri_path = root_uri.as_ref().and_then(|uri| uri.to_file_path().ok());
        let config = aml_config::load_config(root_uri_path);
        *self.root_uri.write().await = root_uri.clone();
        *self.config.write().await = config;
    }

    pub async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let templates = self.templates.read().await;

        // TODO(wiru): if we have no registered template, we are opening the first file of the project, so we
        // need to discover the templates on the project in order to register them with correct
        // hierarchy
        if templates.is_empty() {
            self.initialize_workspace().await;
        }

        let mut templates = self.templates.write().await;
        let mut global_scope = self.global_scope.write().await;
        self.document_manager
            .write()
            .await
            .did_open(params, &mut global_scope, &mut templates)
            .await;
    }

    pub async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let mut global_scope = self.global_scope.write().await;
        self.document_manager
            .write()
            .await
            .did_change(params, &mut global_scope)
            .await;
    }

    pub async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.document_manager.write().await.did_close(params).await;
    }

    pub async fn get_document_manager(&self) -> Arc<RwLock<DocumentManager>> {
        self.document_manager.clone()
    }

    // TODO(wiru): what happens if we can't find the root template?
    async fn initialize_workspace(&self) {
        let config = self.config.read().await;
        let root_uri = self.root_uri.read().await;
        let mut templates = self.templates.write().await;

        let Some(Ok(root_dir)) = root_uri.as_ref().map(|uri| uri.to_file_path()) else { return };
        let Some(root_template_path) = get_root_template(&root_dir, &config) else { return };
        let Ok(content) = std::fs::read_to_string(&root_template_path) else { return };
        let tokens = aml_token::Lexer::new(&content).collect();
        let tokens = aml_token::Tokens::new(tokens, content.len());
        let ast = aml_syntax::Parser::new(tokens).parse();
        let mut global_scope = self.global_scope.write().await;

        ast.accept(&mut TemplateCollector {
            config: &config,
            content: &content,
            root_dir: &root_dir,
            templates: &mut templates,
            file_path: &root_template_path,
            global_scope: &mut global_scope,
        });
    }
}

struct TemplateCollector<'src> {
    content: &'src str,
    config: &'src Config,
    root_dir: &'src PathBuf,
    file_path: &'src PathBuf,
    templates: &'src mut Templates,
    global_scope: &'src mut GlobalScope,
}

impl AstVisitor for TemplateCollector<'_> {
    fn visit_globals(&mut self, decl: &Declaration) {
        let name = decl.name.text(self.content).into();
        let mut analyzer = SemanticAnalyzer::new(self.content, self.global_scope);
        let symbol_type = SymbolType::Variable(analyzer.analyze_expression(&decl.value));

        self.global_scope.declare_global(GlobalSymbol {
            name,
            symbol_type,
            location: decl.location,
            definition: self.file_path.clone(),
        });
    }

    fn visit_component(&mut self, component: &Component) {
        let name = component.name.text(self.content);
        let path = PathBuf::from(name).with_extension("aml");
        let file_path = self.root_dir.join(&self.config.templates_dir).join(&path);

        // TODO(wiru): this has to become a diagnostic of cyclic dependency
        if self.templates.has_template(&name) {
            return;
        }

        self.templates.register(name.into(), file_path.clone());
        let Ok(content) = std::fs::read_to_string(&file_path) else { return };
        let tokens = aml_token::Lexer::new(&content).collect();
        let tokens = aml_token::Tokens::new(tokens, content.len());
        let ast = aml_syntax::Parser::new(tokens).parse();

        ast.accept(&mut TemplateCollector {
            content: &content,
            config: self.config,
            file_path: &file_path,
            root_dir: self.root_dir,
            templates: self.templates,
            global_scope: self.global_scope,
        })
    }
}

// NOTE(wiru): this test is commented out because i'm using it currently to debug the
// initialization of the workspace locally.
//
// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[tokio::test]
//     async fn test_initialize_workspace() {
//         let mut manager = ProjectManager::new();
//         let sample_project = format!("{}/code/anathest", std::env::var("HOME").unwrap());
//         let url = Url::from_file_path(&sample_project).unwrap();
//         manager.root_uri = Arc::new(RwLock::new(Some(url)));
//         manager.initialize_workspace().await;

//         let root = PathBuf::from(&sample_project)
//             .join("templates")
//             .join("index.aml");

//         let text = std::fs::read_to_string(&root).unwrap();
//         let url = Url::from_file_path(root).unwrap();
//         let mut global_scope = manager.global_scope.write().await;
//         let mut templates = manager.templates.write().await;
//         let text_document = TextDocumentItem {
//             uri: url,
//             language_id: "aml".into(),
//             version: 1,
//             text,
//         };

//         let document_manager = manager.document_manager.write().await;
//         document_manager
//             .did_open(
//                 DidOpenTextDocumentParams { text_document },
//                 &mut global_scope,
//                 &mut templates,
//             )
//             .await;

//         println!("{:#?}", document_manager.files());

//         panic!();
//     }
// }
