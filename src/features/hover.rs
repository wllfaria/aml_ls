use aml_semantic::SymbolType;
use aml_syntax::{Ast, AstNode, Expr};
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

    fn get_semantic_hover_content(
        &self,
        node: &AstNode,
        semantic_info: &aml_semantic::SemanticInfo,
        position: aml_core::Location,
    ) -> String {
        if let Some(symbol) = semantic_info.symbol_table.find_symbol_at_position(position) {
            match &symbol.symbol_type {
                SymbolType::Variable(value_type) => {
                    format!("Variable: {} (type: {:?})", symbol.name, value_type)
                }
                SymbolType::Element => format!("Element: {}", symbol.name),
            }
        } else {
            self.get_hover_content(node)
        }
    }
}

fn find_node_in_subtree_with_location(
    node: &AstNode,
    byte_offset: usize,
) -> Option<(&AstNode, aml_core::Location)> {
    match node {
        AstNode::Primitive(primitive) => {
            if primitive.location.contains(byte_offset) {
                return Some((node, primitive.location));
            }
        }
        AstNode::Text(text) => {
            for attribute in &text.attributes.items {
                if let Some((found, loc)) =
                    find_node_in_subtree_with_location(attribute, byte_offset)
                {
                    return Some((found, loc));
                }
            }

            for value in text.values.iter() {
                if let Some((found, loc)) = find_node_in_subtree_with_location(value, byte_offset) {
                    return Some((found, loc));
                }
            }

            for child in text.children.iter() {
                if let Some((found, loc)) = find_node_in_subtree_with_location(child, byte_offset) {
                    return Some((found, loc));
                }
            }

            if text.keyword.contains(byte_offset) {
                return Some((node, text.keyword));
            }
        }
        AstNode::Span(span) => {
            for attribute in &span.attributes.items {
                if let Some((found, loc)) =
                    find_node_in_subtree_with_location(attribute, byte_offset)
                {
                    return Some((found, loc));
                }
            }

            for value in span.values.iter() {
                if let Some((found, loc)) = find_node_in_subtree_with_location(value, byte_offset) {
                    return Some((found, loc));
                }
            }

            if span.keyword.contains(byte_offset) {
                return Some((node, span.keyword));
            }
        }
        AstNode::Container(container) => {
            for attribute in &container.attributes.items {
                if let Some((found, loc)) =
                    find_node_in_subtree_with_location(attribute, byte_offset)
                {
                    return Some((found, loc));
                }
            }

            for child in container.children.iter() {
                if let Some((found, loc)) = find_node_in_subtree_with_location(child, byte_offset) {
                    return Some((found, loc));
                }
            }

            if container.keyword.contains(byte_offset) {
                return Some((node, container.keyword));
            }
        }
        AstNode::String(location) => {
            if location.contains(byte_offset) {
                return Some((node, *location));
            }
        }
        AstNode::Identifier(location) => {
            if location.contains(byte_offset) {
                return Some((node, *location));
            }
        }
        AstNode::Attribute(attribute) => {
            if let Some((found, loc)) =
                find_node_in_subtree_with_location(&attribute.name, byte_offset)
            {
                return Some((found, loc));
            }

            if let Some(location) = get_expr_location_at_offset(&attribute.value, byte_offset) {
                return Some((node, location)); // Return the attribute node with the expression location
            }
        }
        AstNode::Declaration(_) => {}
        AstNode::Error { .. } => {}
        AstNode::Component(_) => {}
        AstNode::ComponentSlot { .. } => {}
        AstNode::For(_) => {}
    }
    None
}

fn get_expr_location_at_offset(expr: &Expr, byte_offset: usize) -> Option<aml_core::Location> {
    match expr {
        Expr::String(location) => {
            if location.contains(byte_offset) {
                Some(*location)
            } else {
                None
            }
        }
        Expr::Ident(location) => {
            if location.contains(byte_offset) {
                Some(*location)
            } else {
                None
            }
        }
        Expr::Unary(unary) => get_expr_location_at_offset(&unary.expr, byte_offset),
        Expr::Binary(binary) => get_expr_location_at_offset(&binary.lhs, byte_offset)
            .or_else(|| get_expr_location_at_offset(&binary.rhs, byte_offset)),
        Expr::Call(call) => get_expr_location_at_offset(&call.fun, byte_offset).or_else(|| {
            call.args
                .iter()
                .find_map(|arg| get_expr_location_at_offset(arg, byte_offset))
        }),
        Expr::ArrayIndex(array_index) => get_expr_location_at_offset(&array_index.lhs, byte_offset)
            .or_else(|| get_expr_location_at_offset(&array_index.index, byte_offset)),
        Expr::List(list) => list
            .items
            .iter()
            .find_map(|expr| get_expr_location_at_offset(expr, byte_offset)),
        Expr::Map(map) => map.items.iter().find_map(|(key, value)| {
            get_expr_location_at_offset(key, byte_offset)
                .or_else(|| get_expr_location_at_offset(value, byte_offset))
        }),
        Expr::Primitive(primitive) => Some(primitive.location),
        Expr::Error(error) => Some(error.location),
    }
}

#[cfg(test)]
mod tests {
    use aml_syntax::Parser;
    use aml_token::{Lexer, Tokens};

    use super::*;

    fn get_ast(template: &str) -> Ast {
        let tokens = Lexer::new(template).collect();
        let tokens = Tokens::new(tokens, template.len());
        let parser = Parser::new(tokens);
        parser.parse()
    }

    #[test]
    fn test_find_attribute_in_node() {
        let template = r#"text [foreground: #ff0000] "Hello""#;
        let provider = HoverProvider::new();
        let ast = get_ast(template);
        let node = provider.find_node_at_position(&ast, "te".len());
        println!("{node:?}");
        panic!();
    }
}
