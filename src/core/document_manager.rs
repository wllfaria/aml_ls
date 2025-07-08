use std::collections::HashMap;
use std::sync::Arc;

use crate::core::project_manager::Templates;
use aml_semantic::global_scope::GlobalScope;
use aml_semantic::{SemanticAnalyzer, SemanticInfo};
use aml_syntax::{Ast, Parser};
use aml_token::{Lexer, Tokens};
use tokio::sync::RwLock;
use tower_lsp::lsp_types::*;

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

    pub async fn did_open(
        &self,
        params: DidOpenTextDocumentParams,
        global_scope: &mut GlobalScope,
        templates: &mut Templates,
    ) {
        let uri = params.text_document.uri.clone();
        let content = params.text_document.text;
        let version = params.text_document.version;

        let ast = self.parse_content(&content);
        let mut analyzer = SemanticAnalyzer::new(&content, global_scope);
        analyzer.collect_globals(&ast);

        let (ast, semantic_info) = self.parse_and_analyze_content(&content, global_scope);
        let file_info = FileInfo::new(content, ast, semantic_info, version);

        self.files.write().await.insert(uri, file_info);
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

        let (ast, semantic_info) = self.parse_and_analyze_content(&file.content, global_scope);
        file.ast = ast;
        file.semantic_info = semantic_info;
        file.version = params.text_document.version;
    }

    pub async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.files.write().await.remove(&params.text_document.uri);
    }

    fn parse_content(&self, content: &str) -> Ast {
        let tokens = Lexer::new(content).collect();
        let tokens = Tokens::new(tokens, content.len());
        Parser::new(tokens).parse()
    }

    fn parse_and_analyze_content(
        &self,
        content: &str,
        global_scope: &mut GlobalScope,
    ) -> (Ast, SemanticInfo) {
        let tokens = Lexer::new(content).collect();
        let tokens = Tokens::new(tokens, content.len());
        let ast = Parser::new(tokens).parse();
        let semantic_info = SemanticAnalyzer::new(content, global_scope).analyze(&ast);
        (ast, semantic_info)
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
