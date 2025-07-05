pub mod capabilities;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::core::document_manager::DocumentManager;
use crate::features::diagnostics::DiagnosticProvider;
use crate::features::hover::{HoverContext, HoverProvider};

#[derive(Debug)]
pub struct Backend {
    client: Client,
    document_manager: DocumentManager,
    hover_provider: HoverProvider,
    diagnostic_provider: DiagnosticProvider,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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
        self.document_manager.did_open(params).await;
        self.publish_diagnostics(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        self.document_manager.did_change(params).await;
        self.publish_diagnostics(&uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        self.document_manager.did_close(params).await;
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        self.hover_provider
            .hover(HoverContext {
                document_manager: &self.document_manager,
                params,
            })
            .await
    }
}

impl Backend {
    async fn publish_diagnostics(&self, uri: &Url) {
        let diagnostics = self
            .diagnostic_provider
            .get_diagnostics(&self.document_manager, uri)
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
        document_manager: DocumentManager::new(),
        hover_provider: HoverProvider::new(),
        diagnostic_provider: DiagnosticProvider::new(),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}
