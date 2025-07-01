use std::collections::HashMap;
use std::sync::Arc;

use aml_syntax::{Ast, Parser};
use aml_token::{Lexer, Tokens};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::*;

#[derive(Debug)]
pub struct FileInfo {
    pub content: String,
    pub ast: Option<Ast>,
    pub version: i32,
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
pub struct DocumentManager {
    files: Arc<RwLock<HashMap<Url, FileInfo>>>,
}

impl DocumentManager {
    pub fn new() -> Self {
        Self {
            files: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub fn files(&self) -> &Arc<RwLock<HashMap<Url, FileInfo>>> {
        &self.files
    }

    pub async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let content = params.text_document.text;
        let version = params.text_document.version;

        let ast = self.parse_content(&content);
        let file_info = FileInfo::new(content, ast, version);

        self.files.write().await.insert(uri, file_info);
    }

    pub async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let mut files = self.files.write().await;
        let file = files.get_mut(&uri);

        let Some(file) = file else { return };

        for change in params.content_changes {
            if let Some(range) = change.range {
                // this is an incremental update. We apply the change to our stored content.
                let start = self.position_to_byte_offset(&file.content, range.start);
                let end = self.position_to_byte_offset(&file.content, range.end);
                file.content.replace_range(start..end, &change.text);
            } else {
                // this is a full update. The client has sent the entire document content.
                file.content = change.text;
            }
        }

        // Reparse the updated content
        file.ast = self.parse_content(&file.content);
        file.version = params.text_document.version;
    }

    pub async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.files.write().await.remove(&params.text_document.uri);
    }

    fn parse_content(&self, content: &str) -> Option<Ast> {
        let tokens = Lexer::new(content)
            .map(|r| r.ok())
            .collect::<Option<Vec<_>>>()?;
        let tokens = Tokens::new(tokens, content.len());
        let parser = Parser::new(tokens, content);
        parser.parse().ok()
    }

    pub fn position_to_byte_offset(&self, content: &str, position: Position) -> usize {
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

    pub fn byte_offset_to_position(&self, content: &str, byte_offset: usize) -> Position {
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
}

