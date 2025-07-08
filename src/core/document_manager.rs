use std::collections::HashMap;
use std::sync::Arc;

use aml_semantic::global_scope::GlobalScope;
use aml_semantic::{SemanticAnalyzer, SemanticInfo};
use aml_syntax::{Ast, Parser};
use aml_token::{Lexer, Tokens};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::*;

use crate::core::project_manager::Templates;

pub fn parse_content(content: &str) -> Ast {
    let tokens = Lexer::new(content).collect();
    let tokens = Tokens::new(tokens, content.len());
    Parser::new(tokens).parse()
}

#[derive(Debug)]
pub struct FileInfo {
    pub content: String,
    pub ast: Ast,
    pub semantic_info: SemanticInfo,
    pub version: i32,
}

impl FileInfo {
    pub fn new(content: String, ast: Ast, semantic_info: SemanticInfo, version: i32) -> Self {
        Self {
            ast,
            content,
            semantic_info,
            version,
        }
    }
}

#[derive(Debug, Default)]
pub struct DocumentManager {
    files: Arc<RwLock<HashMap<Url, FileInfo>>>,
}

impl DocumentManager {
    pub fn files(&self) -> &Arc<RwLock<HashMap<Url, FileInfo>>> {
        &self.files
    }

    pub async fn add_or_update_file(
        &self,
        global_scope: &mut GlobalScope,
        uri: Url,
        ast: Ast,
        content: String,
        version: i32,
    ) {
        let mut analyzer = SemanticAnalyzer::new(&content, global_scope);
        let semantic_info = analyzer.analyze(&ast);
        let mut files = self.files.write().await;

        if let Some(file) = files.get_mut(&uri) {
            file.ast = ast;
            file.content = content;
            file.semantic_info = semantic_info;
            file.version = version
        } else {
            let file_info = FileInfo::new(content, ast, semantic_info, version);
            files.insert(uri, file_info);
        }
    }

    // TODO(wiru): we will already have registered any component referenced by index so we really
    // should update it here if necessary and not assume we can insert as a new one
    //
    // TODO(wiru): in the future, we will watch referenced files for changes in order to update
    // the internal state properly. The LSP specification states that once we receive a did_open
    // event, the server should never read directly from the file, but instead use synchronization
    // events to keep its state up to date. That means we will need to track when files are open
    // to remove watchers, and when they are closed to add them back
    pub async fn did_open(
        &self,
        params: DidOpenTextDocumentParams,
        globals: &mut GlobalScope,
        templates: &mut Templates,
    ) {
        let uri = params.text_document.uri.clone();
        let content = params.text_document.text;
        let version = params.text_document.version;
        let ast = parse_content(&content);
        self.add_or_update_file(globals, uri, ast, content, version)
            .await;
    }

    pub async fn did_change(
        &self,
        params: DidChangeTextDocumentParams,
        global_scope: &mut GlobalScope,
    ) {
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

        let ast = parse_content(&file.content);
        let mut analyzer = SemanticAnalyzer::new(&file.content, global_scope);
        let semantic_info = analyzer.analyze(&ast);
        file.ast = ast;
        file.semantic_info = semantic_info;
        file.version = params.text_document.version;
    }

    pub async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.files.write().await.remove(&params.text_document.uri);
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
