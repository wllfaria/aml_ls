use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use aml_config::Config;
use aml_core::workspace::get_root_template;
use aml_semantic::global_scope::{GlobalScope, GlobalSymbol};
use aml_semantic::{SemanticAnalyzer, SymbolType};
use aml_syntax::ast::{AstVisitor, Component, Declaration};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::*;

use crate::core::document_manager::{DocumentManager, parse_content};

#[derive(Debug, Default)]
pub struct Templates {
    name_to_path_map: HashMap<String, PathBuf>,
}

impl Templates {
    pub fn register(&mut self, name: String, path: PathBuf) {
        self.name_to_path_map.insert(name, path);
    }

    pub fn has_template_by_name(&self, name: &str) -> bool {
        self.name_to_path_map.contains_key(name)
    }

    pub fn name_to_path_map(&self) -> &HashMap<String, PathBuf> {
        &self.name_to_path_map
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

        self.initialize_workspace().await;
        self.analyze_templates_eagerly().await;
    }

    pub async fn did_open(&self, params: DidOpenTextDocumentParams) {
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

        let ast = parse_content(&content);
        let mut global_scope = self.global_scope.write().await;
        let mut document_manager = self.document_manager.write().await;
        templates.register("index".into(), root_template_path.clone());

        ast.accept(&mut TemplateCollector {
            config: &config,
            content: &content,
            root_dir: &root_dir,
            templates: &mut templates,
            file_path: &root_template_path,
            global_scope: &mut global_scope,
            document_manager: &mut document_manager,
        });
    }

    /// Upon initialization, we analyze the templates we found eagerly to produce workspace
    /// diagnostics for the entire project rather than only the current file
    async fn analyze_templates_eagerly(&self) {
        let templates = self.templates.read().await;

        for path in templates.name_to_path_map().values() {
            let Ok(content) = std::fs::read_to_string(path) else { return };
            let Ok(uri) = Url::from_file_path(path) else { return };
            let ast = parse_content(&content);
            let mut global_scope = self.global_scope.write().await;

            self.document_manager
                .write()
                .await
                .add_or_update_file(&mut global_scope, uri, ast, content, 0)
                .await;
        }
    }
}

struct TemplateCollector<'src> {
    content: &'src str,
    config: &'src Config,
    root_dir: &'src PathBuf,
    file_path: &'src PathBuf,
    templates: &'src mut Templates,
    global_scope: &'src mut GlobalScope,
    document_manager: &'src mut DocumentManager,
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
        if self.templates.has_template_by_name(name) {
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
            document_manager: self.document_manager,
        });
    }
}

// NOTE(wiru): this test is commented out because i'm using it currently to debug the
// initialization of the workspace locally.
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_initialize_workspace() {
        let mut manager = ProjectManager::new();
        let sample_project = format!("{}/code/anathest", std::env::var("HOME").unwrap());
        let url = Url::from_file_path(&sample_project).unwrap();
        let params = InitializeParams {
            root_uri: Some(url),
            ..Default::default()
        };
        manager.initialize(params).await;

        // let root = PathBuf::from(&sample_project)
        //     .join("templates")
        //     .join("index.aml");

        // let text = std::fs::read_to_string(&root).unwrap();
        // let url = Url::from_file_path(root).unwrap();
        // let mut global_scope = manager.global_scope.write().await;
        // let mut templates = manager.templates.write().await;
        // let text_document = TextDocumentItem {
        //     uri: url,
        //     language_id: "aml".into(),
        //     version: 1,
        //     text,
        // };

        // document_manager
        //     .did_open(
        //         DidOpenTextDocumentParams { text_document },
        //         &mut global_scope,
        //         &mut templates,
        //     )
        //     .await;

        let document_manager = manager.document_manager.write().await;
        println!("{:#?}", manager.templates.read().await);
        println!(
            "{:#?}",
            document_manager
                .files()
                .read()
                .await
                .keys()
                .map(|k| k.path())
                .collect::<Vec<_>>()
        );

        panic!();
    }
}
