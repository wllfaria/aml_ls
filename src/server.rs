use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use aml_parser::{Ast, AstNode, Lexer, Parser};
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tracing_subscriber::EnvFilter;
use tracing_subscriber::fmt::time::ChronoUtc;
use tracing_subscriber::fmt::{self};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

#[derive(Debug)]
struct FileInfo {
    content: String,
    ast: Option<Ast>,
    version: i32,
}

impl FileInfo {
    pub fn new(content: String, ast: Option<Ast>, version: i32) -> Self {
        Self {
            ast,
            content,
            version,
        }
    }
}

#[derive(Debug)]
struct Backend {
    client: Client,
    files: Arc<RwLock<HashMap<Url, FileInfo>>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        tracing::info!("LSP initialize called");
        tracing::debug!("Initialize params: {:#?}", params);

        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..ServerCapabilities::default()
            },
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
        let uri = params.text_document.uri.clone();
        let content = params.text_document.text;
        let version = params.text_document.version;

        let lexer = Lexer::new(&content);
        let parser = Parser::new(lexer);
        let ast = parser.parse().ok();

        let file_info = FileInfo::new(content, ast, version);
        self.files.write().await.insert(uri, file_info);
    }

    async fn did_change(&self, _: DidChangeTextDocumentParams) {}

    async fn did_close(&self, _: DidCloseTextDocumentParams) {}

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let files = self.files.read().await;

        let Some(file_info) = files.get(&uri) else {
            return Ok(None);
        };

        let Some(ast) = &file_info.ast else {
            return Ok(None);
        };

        let byte_offset = self.position_to_byte_offset(&file_info.content, position);

        if let Some(node_info) = self.find_node_at_position(ast, byte_offset) {
            let hover_content = self.get_hover_content(node_info);

            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_content,
                }),
                range: None,
            }));
        }

        tracing::debug!(
            "No node found at position {}:{} (byte offset: {byte_offset})",
            position.line,
            position.character,
        );
        Ok(None)
    }
}

impl Backend {
    fn position_to_byte_offset(&self, content: &str, position: Position) -> usize {
        let mut byte_offset = 0;
        let mut current_line = 0;
        let mut current_char = 0;

        for ch in content.chars() {
            if current_line == position.line && current_char == position.character {
                return byte_offset;
            }

            if ch == '\n' {
                current_line += 1;
                current_char = 0;
            } else {
                current_char += 1;
            }

            byte_offset += ch.len_utf8();
        }

        byte_offset
    }

    fn find_node_at_position<'a>(&self, ast: &'a Ast, byte_offset: usize) -> Option<&'a AstNode> {
        for node in &ast.nodes {
            if let Some(found) = find_node_in_subtree(node, byte_offset) {
                return Some(found);
            }
        }
        None
    }

    fn get_hover_content(&self, node: &AstNode) -> String {
        match node {
            AstNode::Text { .. } => [
                "# Text (`text`)",
                "",
                "Displays text.",
                "",
                r#"String literals can be wrapped in either `"` or `'`."#,
                "",
                "To add styles to text in the middle of a string use a `span` element as the ",
                "`text` element only accepts `span`s as children. Any other element will be",
                "ignored. ",
                "",
                "## Example",
                "",
                "```",
                r#"text [foreground: "red"] "I'm a little sausage""#,
                "```",
                "",
                "## Attributes",
                "",
                "### `wrap`",
                "",
                "Default is to wrap on word boundaries such as space and hyphen, and this method",
                "of wrapping will be used if no `wrap` attribute is given.",
                "",
                "Valid values:",
                r#"* `"break"`: the text will wrap once it can no longer fit"#,
                "",
                "### `text_align`",
                "",
                "Note that text align will align the text within the element.",
                "The text element will size it self according to its constraint.",
                "",
                "To right align text to the right side of the screen therefore requires the use",
                "of the alignment widget in combination with the text align attribute.",
                "",
                "Default: `left`",
                "",
                "Valid values:",
                r#"* `"left"`"#,
                r#"* `"right"`"#,
                r#"* `"centre"` | `"center"`"#,
                "",
                "Example of right aligned text",
                "```",
                "border [width: 5 + 2]",
                r#"    text [text_align: "right"] "hello you""#,
                "```",
                "",
                "```",
                "┌─────┐",
                "│hello│",
                "│  you│",
                "└─────┘",
                "```",
                "",
                "Example of centre aligned text",
                "```",
                "border [width: 5 + 2]",
                r#"    text [text_align: "centre"] "hello you""#,
                "```",
                "",
                "```",
                "┌─────┐",
                "│hello│",
                "│ you │",
                "└─────┘",
                "```",
            ]
            .join("\n"),
            AstNode::Span { .. } => {
                "**span** - A styled text span within a text element.".to_string()
            }
            AstNode::String { .. } => "String literal".to_string(),
        }
    }
}

fn find_node_in_subtree(node: &AstNode, byte_offset: usize) -> Option<&AstNode> {
    match node {
        AstNode::Text {
            value,
            children,
            location,
        } => {
            if byte_offset >= location.start_byte && byte_offset <= location.end_byte {
                return Some(node);
            }

            if let Some(value_node) = value
                && let AstNode::String { value: location } = value_node.as_ref()
                && byte_offset >= location.start_byte
                && byte_offset <= location.end_byte
            {
                return Some(node);
            }

            for child in children {
                if let Some(found) = find_node_in_subtree(child, byte_offset) {
                    return Some(found);
                }
            }
        }
        AstNode::Span { value, location } => {
            if byte_offset >= location.start_byte && byte_offset <= location.end_byte {
                return Some(node);
            }

            if let Some(location) = value
                && byte_offset >= location.start_byte
                && byte_offset <= location.end_byte
            {
                return Some(node);
            }
        }
        AstNode::String { value } => {
            if byte_offset >= value.start_byte && byte_offset <= value.end_byte {
                return Some(node);
            }
        }
    }
    None
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
            files: Arc::new(RwLock::new(HashMap::new())),
        }
    });

    tracing::info!("LSP server starting to serve");
    Server::new(stdin, stdout, socket).serve(service).await;
}
