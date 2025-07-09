use aml_syntax::ast::*;
use aml_syntax::{NodeFinder, NodeFinderResult};
use aml_token::Container;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

use crate::core::document_manager::DocumentManager;

#[derive(Debug)]
pub struct HoverProvider {
    docs: aml_docs::Docs,
}

pub struct HoverContext<'ctx> {
    pub document_manager: &'ctx DocumentManager,
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

        let byte_offset = ctx
            .document_manager
            .position_to_byte_offset(&file_info.content, position);

        let mut finder = NodeFinder {
            byte_offset,
            result: None,
        };

        file_info.ast.accept(&mut finder);
        let Some(NodeFinderResult::Node(node)) = finder.result else { return Ok(None) };

        let location = node.location();
        let value = self.get_hover_content(node);

        let contents = HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        });

        let range = Some(Range {
            start: ctx
                .document_manager
                .byte_offset_to_position(&file_info.content, location.start_byte),

            end: ctx
                .document_manager
                .byte_offset_to_position(&file_info.content, location.end_byte),
        });

        Ok(Some(Hover { contents, range }))
    }

    fn get_hover_content(&self, node: &AstNode) -> String {
        match node {
            AstNode::Primitive(primitive) => format!("{:?}", primitive.value),
            AstNode::Text(_) => self.docs.text.into(),
            AstNode::Span(_) => self.docs.span.into(),
            AstNode::Container(container) => match container.kind {
                Container::VStack => self.docs.vstack.into(),
                Container::HStack => self.docs.hstack.into(),
                Container::Border => self.docs.border.into(),
                Container::Alignment => self.docs.alignment.into(),
                Container::ZStack => self.docs.zstack.into(),
                Container::Row => self.docs.row.into(),
                Container::Column => self.docs.column.into(),
                Container::Expand => self.docs.expand.into(),
                Container::Position => self.docs.position.into(),
                Container::Spacer => self.docs.spacer.into(),
                Container::Overflow => self.docs.overflow.into(),
                Container::Padding => self.docs.padding.into(),
                Container::Canvas => self.docs.canvas.into(),
                Container::Container => self.docs.container.into(),
            },
            AstNode::Error { .. } => "error".into(),
            AstNode::String { .. } => "String literal".into(),
            AstNode::Identifier { .. } => "Identifier".into(),
            AstNode::Declaration(_) => "Declaration".into(),
            AstNode::Attribute(_) => "Attribute".into(),
            _ => "Unknown".into(),
        }
    }
}
