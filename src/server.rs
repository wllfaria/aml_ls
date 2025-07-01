use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use aml_parser::{Ast, AstNode, Lexer, Parser, Tokens};
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
    docs: aml_docs::Docs,
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

        let tokens = Lexer::new(&content).map(|r| r.unwrap()).collect();
        let tokens = Tokens::new(tokens, content.len());
        let parser = Parser::new(tokens, &content);
        let ast = parser.parse().ok();

        let file_info = FileInfo::new(content, ast, version);
        self.files.write().await.insert(uri, file_info);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let mut files = self.files.write().await;
        let file = files.get_mut(&uri);

        let Some(file) = file else { return };

        for change in params.content_changes {
            if let Some(range) = change.range {
                // this is an incremental update. We apply the change to our stored content.
                let start = position_to_byte_offset(&file.content, range.start);
                let end = position_to_byte_offset(&file.content, range.end);
                file.content.replace_range(start..end, &change.text);
            } else {
                // this is a full update. The client has sent the entire document content.
                file.content = change.text;
            }
        }

        let tokens = Lexer::new(&file.content)
            .map(|t| t.unwrap())
            .collect::<Vec<_>>();
        let tokens = Tokens::new(tokens, file.content.len());
        let parser = Parser::new(tokens, &file.content);
        let ast = parser.parse().ok();

        file.ast = ast;
        file.version = params.text_document.version;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.files.write().await.remove(&params.text_document.uri);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let files = self.files.read().await;

        let Some(file_info) = files.get(&uri) else { return Ok(None) };
        let Some(ast) = &file_info.ast else { return Ok(None) };
        let byte_offset = position_to_byte_offset(&file_info.content, position);
        let Some((node_info, location)) = self.find_node_at_position(ast, byte_offset) else { return Ok(None) };
        let value = self.get_hover_content(node_info);

        let contents = HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        });

        let range = Some(Range {
            start: byte_offset_to_position(&file_info.content, location.start_byte),
            end: byte_offset_to_position(&file_info.content, location.end_byte),
        });

        Ok(Some(Hover {
            contents,
            range,
        }))
    }
}

fn position_to_byte_offset(content: &str, position: Position) -> usize {
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

fn byte_offset_to_position(content: &str, byte_offset: usize) -> Position {
    let mut current_line = 0;
    let mut current_char = 0;
    let mut current_byte = 0;

    for ch in content.chars() {
        if current_byte >= byte_offset {
            break;
        }

        if ch == '\n' {
            current_line += 1;
            current_char = 0;
        } else {
            current_char += 1;
        }

        current_byte += ch.len_utf8();
    }

    Position {
        line: current_line,
        character: current_char,
    }
}

impl Backend {
    fn find_node_at_position<'a>(&self, ast: &'a Ast, byte_offset: usize) -> Option<(&'a AstNode, aml_parser::Location)> {
        for node in &ast.nodes {
            if let Some((found_node, location)) = find_node_in_subtree_with_location(node, byte_offset) {
                return Some((found_node, location));
            }
        }
        None
    }

    fn get_hover_content(&self, node: &AstNode) -> String {
        match node {
            AstNode::Text { .. } => self.docs.text.into(),
            AstNode::Span { .. } => self.docs.span.into(),
            AstNode::String { .. } => "String literal".into(),
            AstNode::Identifier { .. } => "Identifier".into(),
            AstNode::Attribute { name, .. } => {
                if let AstNode::Identifier { .. } = name.as_ref() {
                    "Attribute".into()
                } else {
                    "Attribute".into()
                }
            }
        }
    }
}

fn find_node_in_subtree_with_location(node: &AstNode, byte_offset: usize) -> Option<(&AstNode, aml_parser::Location)> {
    match node {
        AstNode::Text {
            value,
            attributes,
            children,
            location,
            ..
        } => {
            // Check attributes first (more specific)
            for attribute in attributes {
                if let Some((found, loc)) = find_node_in_subtree_with_location(attribute, byte_offset) {
                    return Some((found, loc));
                }
            }

            // Check value node
            if let Some(value_node) = value
                && let Some((found, loc)) = find_node_in_subtree_with_location(value_node, byte_offset)
            {
                return Some((found, loc));
            }

            // Check children
            for child in children {
                if let Some((found, loc)) = find_node_in_subtree_with_location(child, byte_offset) {
                    return Some((found, loc));
                }
            }

            // Finally check if we're in the text element itself
            if byte_offset >= location.start_byte && byte_offset <= location.end_byte {
                return Some((node, *location));
            }
        }
        AstNode::Span {
            value,
            attributes, 
            location,
            ..
        } => {
            // Check attributes first (more specific)
            for attribute in attributes {
                if let Some((found, loc)) = find_node_in_subtree_with_location(attribute, byte_offset) {
                    return Some((found, loc));
                }
            }

            // Check value
            if let Some(value) = value
                && let Some((found, loc)) = find_node_in_subtree_with_location(value, byte_offset)
            {
                return Some((found, loc));
            }

            // Finally check if we're in the span element itself
            if byte_offset >= location.start_byte && byte_offset <= location.end_byte {
                return Some((node, *location));
            }
        }
        AstNode::String { value } => {
            if byte_offset >= value.start_byte && byte_offset <= value.end_byte {
                return Some((node, *value));
            }
        }
        AstNode::Identifier { value } => {
            if byte_offset >= value.start_byte && byte_offset <= value.end_byte {
                return Some((node, *value));
            }
        }
        AstNode::Attribute { name, value } => {
            // Check attribute name
            if let Some((found, loc)) = find_node_in_subtree_with_location(name, byte_offset) {
                return Some((found, loc));
            }

            // Check if position is in attribute value
            if let Some(location) = get_expr_location_at_offset(value, byte_offset) {
                return Some((node, location)); // Return the attribute node with the expression location
            }
        }
    }
    None
}

// Helper function to find location in expressions
fn get_expr_location_at_offset(expr: &aml_parser::Expr, byte_offset: usize) -> Option<aml_parser::Location> {
    use aml_parser::Expr;
    
    match expr {
        Expr::String { location } => {
            if byte_offset >= location.start_byte && byte_offset <= location.end_byte {
                Some(*location)
            } else {
                None
            }
        }
        Expr::Ident { location } => {
            if byte_offset >= location.start_byte && byte_offset <= location.end_byte {
                Some(*location)
            } else {
                None
            }
        }
        Expr::Unary { expr, .. } => get_expr_location_at_offset(expr, byte_offset),
        Expr::Binary { lhs, rhs, .. } => {
            get_expr_location_at_offset(lhs, byte_offset)
                .or_else(|| get_expr_location_at_offset(rhs, byte_offset))
        }
        Expr::Call { fun, args } => {
            get_expr_location_at_offset(fun, byte_offset)
                .or_else(|| {
                    args.iter()
                        .find_map(|arg| get_expr_location_at_offset(arg, byte_offset))
                })
        }
        Expr::ArrayIndex { lhs, index } => {
            get_expr_location_at_offset(lhs, byte_offset)
                .or_else(|| get_expr_location_at_offset(index, byte_offset))
        }
        Expr::List(exprs) => {
            exprs.iter()
                .find_map(|expr| get_expr_location_at_offset(expr, byte_offset))
        }
        Expr::Map { items } => {
            items.iter()
                .find_map(|(key, value)| {
                    get_expr_location_at_offset(key, byte_offset)
                        .or_else(|| get_expr_location_at_offset(value, byte_offset))
                })
        }
        Expr::Primitive(_) => None, // Primitives don't store location
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
            files: Arc::new(RwLock::new(HashMap::new())),
            docs: aml_docs::Docs::default(),
        }
    });

    tracing::info!("LSP server starting to serve");
    Server::new(stdin, stdout, socket).serve(service).await;
}
