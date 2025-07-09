use std::sync::Arc;

use aml_config::Config;
use aml_semantic::global_scope::GlobalScope;
use tokio::sync::RwLock;
use tower_lsp::lsp_types::*;

use crate::core::document_manager::DocumentManager;
use crate::core::template_service::TemplateService;

#[derive(Debug)]
pub struct ProjectManager {
    pub config: Arc<RwLock<Config>>,
    pub template_service: Arc<RwLock<TemplateService>>,
    pub root_uri: Arc<RwLock<Option<Url>>>,
    pub global_scope: Arc<RwLock<GlobalScope>>,
    pub document_manager: Arc<RwLock<DocumentManager>>,
}

impl ProjectManager {
    pub fn new() -> Self {
        Self {
            config: Default::default(),
            root_uri: Default::default(),
            template_service: Arc::new(RwLock::new(TemplateService::new())),
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
        let mut template_service = self.template_service.write().await;
        let mut global_scope = self.global_scope.write().await;
        self.document_manager
            .write()
            .await
            .did_open(params, &mut global_scope, template_service.templates_mut())
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
        let mut template_service = self.template_service.write().await;

        let Some(Ok(root_dir)) = root_uri.as_ref().map(|uri| uri.to_file_path()) else { return };

        let mut global_scope = self.global_scope.write().await;
        let mut document_manager = self.document_manager.write().await;

        let _ = template_service.discover_templates(
            &root_dir,
            &config,
            &mut global_scope,
            &mut document_manager,
        );
    }

    /// Preloads and analyzes all discovered templates to provide workspace-wide diagnostics.
    /// This runs after workspace initialization to ensure all templates are processed
    /// and available for cross-file analysis and diagnostics.
    async fn preload_workspace_templates(&self) {
        let template_service = self.template_service.read().await;
        let mut global_scope = self.global_scope.write().await;
        let mut document_manager = self.document_manager.write().await;

        let _ = template_service
            .preload_templates(&mut global_scope, &mut document_manager)
            .await;
    }
}
