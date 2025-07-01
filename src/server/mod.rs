pub mod capabilities;

use std::path::PathBuf;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing_subscriber::EnvFilter;
use tracing_subscriber::fmt::time::ChronoUtc;
use tracing_subscriber::fmt::{self};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

use crate::core::document_manager::DocumentManager;
use crate::features::hover::HoverProvider;

#[derive(Debug)]
pub struct Backend {
    client: Client,
    document_manager: DocumentManager,
    hover_provider: HoverProvider,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        tracing::info!("LSP initialize called");
        tracing::debug!("Initialize params: {:#?}", params);

        Ok(InitializeResult {
            server_info: None,
            capabilities: capabilities::server_capabilities(),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        tracing::info!("LSP client initialized successfully");
    }

    async fn shutdown(&self) -> Result<()> {
        tracing::info!("LSP shutdown requested");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.document_manager.did_open(params).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.document_manager.did_change(params).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.document_manager.did_close(params).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        self.hover_provider.hover(&self.document_manager, params).await
    }
}

fn init_tracing() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let log_dir = PathBuf::from("/Users/wiru/.local/share/aml_ls");
    std::fs::create_dir_all(&log_dir)?;

    let file_appender = tracing_appender::rolling::never(&log_dir, "aml_ls.log");
    let (non_blocking, _guard) = tracing_appender::non_blocking(file_appender);

    tracing_subscriber::registry()
        .with(
            fmt::layer()
                .with_writer(non_blocking)
                .with_timer(ChronoUtc::rfc_3339())
                .with_thread_ids(true)
                .with_file(true)
                .with_line_number(true)
                .with_target(false),
        )
        .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("debug")))
        .init();

    tracing::info!("AML Language Server starting up");

    // Leak the guard to keep the writer alive
    std::mem::forget(_guard);

    Ok(())
}

pub async fn start() {
    init_tracing().ok();
    tracing::info!("Starting AML Language Server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| {
        tracing::info!("New LSP client connected");
        Backend {
            client,
            document_manager: DocumentManager::new(),
            hover_provider: HoverProvider::new(),
        }
    });

    tracing::info!("LSP server starting to serve");
    Server::new(stdin, stdout, socket).serve(service).await;
}