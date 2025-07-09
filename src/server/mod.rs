pub mod capabilities;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::core::project_manager::ProjectManager;
use crate::features::diagnostics::DiagnosticProvider;
use crate::features::hover::{HoverContext, HoverProvider};

#[derive(Debug)]
pub struct Backend {
    client: Client,
    project_manager: ProjectManager,
    hover_provider: HoverProvider,
    diagnostic_provider: DiagnosticProvider,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.project_manager.initialize(params).await;

        Ok(InitializeResult {
            server_info: None,
            capabilities: capabilities::server_capabilities(),
        })
    }

    async fn initialized(&self, _: InitializedParams) {}

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        self.project_manager.did_open(params).await;
        self.publish_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        self.project_manager.did_change(params).await;
        self.publish_diagnostics(&uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        self.project_manager.did_close(params).await;
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let document_manager = self.project_manager.get_document_manager().await;
        let document_manager_guard = document_manager.read().await;
        let global_scope = self.project_manager.global_scope.read().await;

        self.hover_provider
            .hover(HoverContext {
                document_manager: &document_manager_guard,
                global_scope: &global_scope,
                params,
            })
            .await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        self.project_manager.goto_definition(params).await
    }
}

impl Backend {
    async fn publish_diagnostics(&self, uri: &Url) {
        let document_manager = self.project_manager.get_document_manager().await;
        let document_manager_guard = document_manager.read().await;

        let diagnostics = self
            .diagnostic_provider
            .get_diagnostics(&document_manager_guard, uri)
            .await;
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}

pub async fn start() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        project_manager: ProjectManager::new(),
        hover_provider: HoverProvider::new(),
        diagnostic_provider: DiagnosticProvider::new(),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}
