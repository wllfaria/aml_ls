use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use aml_config::Config;
use aml_semantic::global_scope::GlobalScope;
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
    pub root_uri: Arc<RwLock<Option<Url>>>,
    pub document_manager: Arc<RwLock<DocumentManager>>,
    pub global_scope: Arc<RwLock<GlobalScope>>,
    pub templates: Arc<RwLock<Templates>>,
    pub config: Arc<RwLock<Config>>,
}

impl ProjectManager {
    pub fn new() -> Self {
        Self {
            root_uri: Default::default(),
            document_manager: Default::default(),
            global_scope: Default::default(),
            config: Default::default(),
            templates: Default::default(),
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
        let mut templates = self.templates.write().await;

        // TODO(wiru): if we have no registered template, we are opening the first file of the project, so we
        // need to discover the templates on the project in order to register them with correct
        // hierarchy

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
}
