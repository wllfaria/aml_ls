use aml_semantic::global_scope::GlobalScope;
use aml_semantic::{SymbolType, ValueType};
use aml_syntax::ast::*;
use aml_syntax::{NodeFinder, NodeFinderResult};
use aml_token::Container;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

use crate::core::document_manager::{DocumentManager, FileInfo};

#[derive(Debug)]
pub struct HoverProvider {
    docs: aml_docs::Docs,
}

pub struct HoverContext<'ctx> {
    pub document_manager: &'ctx DocumentManager,
    pub global_scope: &'ctx GlobalScope,
    pub params: HoverParams,
}

impl HoverProvider {
    pub fn new() -> Self {
        Self {
            docs: aml_docs::Docs::default(),
        }
    }

    pub async fn hover(&self, ctx: HoverContext<'_>) -> Result<Option<Hover>> {
        let uri = ctx.params.text_document_position_params.text_document.uri;
        let position = ctx.params.text_document_position_params.position;
        let files = ctx.document_manager.files().read().await;
        let Some(file_info) = files.get(&uri) else { return Ok(None) };

        let byte_offset = DocumentManager::position_to_byte_offset(&file_info.content, position);

        let mut finder = NodeFinder {
            byte_offset,
            result: None,
        };

        file_info.ast.accept(&mut finder);
        let Some(NodeFinderResult::Node(node)) = finder.result else { return Ok(None) };
        let Some(value) = self.get_hover_content(node, &file_info, ctx.global_scope) else {
            return Ok(None);
        };

        let location = node.location();
        let contents = HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        });

        let range = Some(Range {
            start: DocumentManager::byte_offset_to_position(
                &file_info.content,
                location.start_byte,
            ),
            end: DocumentManager::byte_offset_to_position(&file_info.content, location.end_byte),
        });

        Ok(Some(Hover { contents, range }))
    }

    fn get_hover_content(
        &self,
        node: &AstNode,
        file_info: &FileInfo,
        global_scope: &GlobalScope,
    ) -> Option<String> {
        match node {
            AstNode::Primitive(primitive) => Some(format!("{:?}", primitive.value)),
            AstNode::Text(_) => Some(self.docs.text.into()),
            AstNode::Span(_) => Some(self.docs.span.into()),
            AstNode::Container(container) => match container.kind {
                Container::VStack => Some(self.docs.vstack.into()),
                Container::HStack => Some(self.docs.hstack.into()),
                Container::Border => Some(self.docs.border.into()),
                Container::Alignment => Some(self.docs.alignment.into()),
                Container::ZStack => Some(self.docs.zstack.into()),
                Container::Row => Some(self.docs.row.into()),
                Container::Column => Some(self.docs.column.into()),
                Container::Expand => Some(self.docs.expand.into()),
                Container::Position => Some(self.docs.position.into()),
                Container::Spacer => Some(self.docs.spacer.into()),
                Container::Overflow => Some(self.docs.overflow.into()),
                Container::Padding => Some(self.docs.padding.into()),
                Container::Canvas => Some(self.docs.canvas.into()),
                Container::Container => Some(self.docs.container.into()),
            },
            AstNode::Error { .. } => None,
            AstNode::String(location) => None,
            AstNode::Identifier(location) => {
                let name = node.text(&file_info.content);
                let path = &file_info.path.to_string_lossy();
                let template = &file_info.name;

                if let Some(symbol) = file_info.semantic_info.symbol_table.lookup_symbol(name) {
                    let declaration = file_info.content[symbol.location.to_range()].to_string();
                    let symbol_type = &symbol.symbol_type;

                    let content = [
                        "```aml",
                        &format!(r#"local {name}: {symbol_type}"#),
                        "```",
                        "---",
                        &format!("defined in: [{template}]({path})"),
                    ]
                    .join("\n");
                    return Some(content);
                }

                None
            }
            AstNode::Declaration(_) => None,
            AstNode::Attribute(_) => None,
            AstNode::Component(component) => None,
            AstNode::ComponentSlot(component_slot) => None,
            AstNode::For(_) => None,
        }
    }
}
