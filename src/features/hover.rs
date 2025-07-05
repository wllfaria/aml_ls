use aml_semantic::SymbolType;
use aml_syntax::{Ast, AstNode, Expr};
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
        let ast = &file_info.ast;

        let byte_offset = ctx
            .document_manager
            .position_to_byte_offset(&file_info.content, position);

        let Some((node_info, location)) = self.find_node_at_position(ast, byte_offset) else {
            return Ok(None);
        };

        let value = self.get_semantic_hover_content(node_info, &file_info.semantic_info, location);

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

    fn find_node_at_position<'a>(
        &self,
        ast: &'a Ast,
        byte_offset: usize,
    ) -> Option<(&'a AstNode, aml_core::Location)> {
        for node in &ast.nodes {
            if let Some((found_node, location)) =
                find_node_in_subtree_with_location(node, byte_offset)
            {
                return Some((found_node, location));
            }
        }
        None
    }

    fn get_hover_content(&self, node: &AstNode) -> String {
        match node {
            AstNode::Primitive { value, .. } => format!("{value:?}"),
            AstNode::Text { .. } => self.docs.text.into(),
            AstNode::Span { .. } => self.docs.span.into(),
            AstNode::Error { .. } => "error".into(),
            AstNode::String { .. } => "String literal".into(),
            AstNode::Identifier { .. } => "Identifier".into(),
            AstNode::Declaration { .. } => "Declaration".into(),
            AstNode::Attribute { name, .. } => {
                if let AstNode::Identifier { .. } = name.as_ref() {
                    "Attribute".into()
                } else {
                    "Attribute".into()
                }
            }
        }
    }

    fn get_semantic_hover_content(
        &self,
        node: &AstNode,
        semantic_info: &aml_semantic::SemanticInfo,
        position: aml_core::Location,
    ) -> String {
        // Try to get semantic information for the node at this position
        if let Some(symbol) = semantic_info.symbol_table.find_symbol_at_position(position) {
            match &symbol.symbol_type {
                SymbolType::Variable { value_type } => {
                    format!("Variable: {} (type: {:?})", symbol.name, value_type)
                }
                SymbolType::Element => format!("Element: {}", symbol.name),
            }
        } else {
            // Fall back to basic hover content
            self.get_hover_content(node)
        }
    }
}

fn find_node_in_subtree_with_location(
    node: &AstNode,
    byte_offset: usize,
) -> Option<(&AstNode, aml_core::Location)> {
    match node {
        AstNode::Primitive { .. } => todo!(),
        AstNode::Text {
            values,
            attributes,
            children,
            location,
            ..
        } => {
            for attribute in &attributes.attributes {
                if let Some((found, loc)) =
                    find_node_in_subtree_with_location(attribute, byte_offset)
                {
                    return Some((found, loc));
                }
            }

            for value in values {
                if let Some((found, loc)) = find_node_in_subtree_with_location(value, byte_offset) {
                    return Some((found, loc));
                }
            }

            for child in children {
                if let Some((found, loc)) = find_node_in_subtree_with_location(child, byte_offset) {
                    return Some((found, loc));
                }
            }

            if byte_offset >= location.start_byte && byte_offset <= location.end_byte {
                return Some((node, *location));
            }
        }
        AstNode::Span {
            values,
            attributes,
            location,
            ..
        } => {
            for attribute in &attributes.attributes {
                if let Some((found, loc)) =
                    find_node_in_subtree_with_location(attribute, byte_offset)
                {
                    return Some((found, loc));
                }
            }

            for value in values {
                if let Some((found, loc)) = find_node_in_subtree_with_location(value, byte_offset) {
                    return Some((found, loc));
                }
            }

            if byte_offset >= location.start_byte && byte_offset <= location.end_byte {
                return Some((node, *location));
            }
        }
        AstNode::String { location } => {
            if location.contains(byte_offset) {
                return Some((node, *location));
            }
        }
        AstNode::Identifier { location } => {
            if location.contains(byte_offset) {
                return Some((node, *location));
            }
        }
        AstNode::Attribute { name, value, .. } => {
            if let Some((found, loc)) = find_node_in_subtree_with_location(name, byte_offset) {
                return Some((found, loc));
            }

            if let Some(location) = get_expr_location_at_offset(value, byte_offset) {
                return Some((node, location)); // Return the attribute node with the expression location
            }
        }
        AstNode::Declaration { .. } => {}
        AstNode::Error { .. } => {}
    }
    None
}

fn get_expr_location_at_offset(expr: &Expr, byte_offset: usize) -> Option<aml_core::Location> {
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
        Expr::Binary { lhs, rhs, .. } => get_expr_location_at_offset(lhs, byte_offset)
            .or_else(|| get_expr_location_at_offset(rhs, byte_offset)),
        Expr::Call { fun, args, .. } => {
            get_expr_location_at_offset(fun, byte_offset).or_else(|| {
                args.iter()
                    .find_map(|arg| get_expr_location_at_offset(arg, byte_offset))
            })
        }
        Expr::ArrayIndex { lhs, index, .. } => get_expr_location_at_offset(lhs, byte_offset)
            .or_else(|| get_expr_location_at_offset(index, byte_offset)),
        Expr::List { items, .. } => items
            .iter()
            .find_map(|expr| get_expr_location_at_offset(expr, byte_offset)),
        Expr::Map { items, .. } => items.iter().find_map(|(key, value)| {
            get_expr_location_at_offset(key, byte_offset)
                .or_else(|| get_expr_location_at_offset(value, byte_offset))
        }),
        Expr::Primitive { location, .. } => Some(*location),
        Expr::Error { location, .. } => Some(*location),
    }
}
