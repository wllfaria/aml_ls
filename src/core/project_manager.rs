use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use aml_config::Config;
use aml_core::workspace::get_root_template;
use aml_semantic::global_scope::{GlobalScope, GlobalSymbol};
use aml_semantic::{SemanticAnalyzer, SymbolType};
use aml_syntax::ast::*;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::*;

use crate::core::document_manager::{DocumentManager, parse_content};

#[derive(Debug)]
pub struct Template {
    pub path: PathBuf,
}

#[derive(Debug, Default)]
pub struct Templates {
    inner: Vec<Template>,
    name_map: HashMap<String, usize>,
    path_map: HashMap<PathBuf, usize>,
}

impl Templates {
    pub fn register(&mut self, name: String, path: PathBuf, _: bool) {
        let index = self.inner.len();
        let template = Template { path: path.clone() };

        self.name_map.insert(name, index);
        self.path_map.insert(path, index);
        self.inner.push(template);
    }

    pub fn has_template_by_name(&self, name: &str) -> bool {
        self.name_map.contains_key(name)
    }

    pub fn paths(&self) -> impl Iterator<Item = &Template> {
        self.inner.iter()
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
        self.preload_workspace_templates().await;
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

    pub async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> tower_lsp::jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let document_manager = self.document_manager.write().await;
        let global_scope = self.global_scope.read().await;

        document_manager
            .goto_definition(params, &global_scope)
            .await
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
        templates.register("index".into(), root_template_path.clone(), false);

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

    /// Preloads and analyzes all discovered templates to provide workspace-wide diagnostics.
    /// This runs after workspace initialization to ensure all templates are processed
    /// and available for cross-file analysis and diagnostics.
    async fn preload_workspace_templates(&self) {
        let templates = self.templates.read().await;

        for template in templates.paths() {
            let Ok(content) = std::fs::read_to_string(&template.path) else { return };
            let Ok(uri) = Url::from_file_path(&template.path) else { return };
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

/// Discovers and registers templates during workspace initialization by walking the AST of
/// templates, collecting:
///
/// - Global variable declarations (registered in `GlobalScope`)
/// - Component references (recursively processes referenced template files)
struct TemplateCollector<'src> {
    content: &'src str,
    config: &'src Config,
    root_dir: &'src PathBuf,
    file_path: &'src PathBuf,
    templates: &'src mut Templates,
    global_scope: &'src mut GlobalScope,
    document_manager: &'src mut DocumentManager,
}

impl<'src> AstVisitor<'src> for TemplateCollector<'src> {
    fn visit_globals(&mut self, decl: &Declaration, _: &AstNode) {
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

    fn visit_component(&mut self, component: &Component, _: &AstNode) {
        let name = component.name.text(self.content);
        let path = PathBuf::from(name).with_extension("aml");
        let file_path = self.root_dir.join(&self.config.templates_dir).join(&path);

        // Prevent infinite recursion by checking if template is already registered.
        // TODO(wiru): this has to become a diagnostic of cyclic dependency
        if self.templates.has_template_by_name(name) {
            return;
        }

        self.templates
            .register(name.into(), file_path.clone(), false);
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

    fn visit_container(&mut self, container: &'src ContainerNode, _: &'src AstNode) {
        for child in container.children.iter() {
            child.accept(self);
        }
    }

    fn visit_text(&mut self, text: &'src Text, _: &'src AstNode) {
        for child in text.children.iter() {
            child.accept(self);
        }
    }

    fn visit_for(&mut self, for_loop: &'src For, _: &'src AstNode) {
        for child in for_loop.children.iter() {
            child.accept(self);
        }
    }
}

// NOTE(wiru): this test is commented out because i'm using it currently to debug the
// initialization of the workspace locally.
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_initialize_workspace() {
        let manager = ProjectManager::new();
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
