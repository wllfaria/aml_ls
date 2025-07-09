pub mod ast;
pub mod expressions;
pub mod parser;

use ast::*;
pub use ast::{Ast, Scope};
pub use parser::Parser;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum NodeFinderResult<'src> {
    Node(&'src AstNode),
    Expr(&'src Expr),
}

#[derive(Debug)]
pub struct NodeFinder<'src> {
    pub byte_offset: usize,
    pub result: Option<NodeFinderResult<'src>>,
}

impl<'ast> AstVisitor<'ast> for NodeFinder<'ast> {
    fn visit_globals(&mut self, decl: &'ast Declaration, _: &'ast AstNode) {
        decl.name.accept(self);
        decl.value.accept(self);
        // TODO(wiru): we should store the keyword aswell
    }

    fn visit_locals(&mut self, decl: &'ast Declaration, _: &'ast AstNode) {
        decl.name.accept(self);
        decl.value.accept(self);
    }

    fn visit_string(&mut self, location: aml_core::Location, node: &'ast AstNode) {
        if location.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Node(node));
        }
    }

    fn visit_component(&mut self, component: &'ast Component, _: &'ast AstNode) {
        component.name.accept(self);

        for attr in component.attributes.items.iter() {
            attr.accept(self);
        }
    }

    fn visit_container(&mut self, container: &'ast ContainerNode, node: &'ast AstNode) {
        for attr in container.attributes.items.iter() {
            attr.accept(self);
        }

        for child in container.children.iter() {
            child.accept(self);
        }

        if container.keyword.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Node(node));
        }
    }

    fn visit_text(&mut self, text: &'ast Text, node: &'ast AstNode) {
        for attr in text.attributes.items.iter() {
            attr.accept(self);
        }

        for value in text.values.iter() {
            value.accept(self);
        }

        for child in text.children.iter() {
            child.accept(self);
        }

        if text.keyword.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Node(node));
        }
    }

    fn visit_for(&mut self, for_loop: &'ast For, node: &'ast AstNode) {
        for_loop.binding.accept(self);
        for_loop.value.accept(self);

        for child in for_loop.children.iter() {
            child.accept(self);
        }

        if for_loop.keyword.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Node(node));
        }
    }

    fn visit_component_slot(&mut self, slot: &'ast ComponentSlot, _: &'ast AstNode) {
        slot.name.accept(self);
    }

    fn visit_primitive(&mut self, prim: &'ast PrimitiveNode, node: &'ast AstNode) {
        if prim.location.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Node(node))
        }
    }

    fn visit_span(&mut self, span: &'ast Span, node: &'ast AstNode) {
        for attr in span.attributes.items.iter() {
            attr.accept(self);
        }

        for value in span.values.iter() {
            value.accept(self);
        }

        if span.keyword.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Node(node));
        }
    }

    fn visit_identifier(&mut self, ident: aml_core::Location, node: &'ast AstNode) {
        if ident.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Node(node));
        }
    }

    fn visit_attribute(&mut self, attr: &'ast Attribute, _node: &'ast AstNode) {
        attr.name.accept(self);
        attr.value.accept(self);
    }

    fn visit_error(&mut self, err: &'ast ErrorNode, node: &'ast AstNode) {
        if err.location.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Node(node));
        }
    }
}

impl<'ast> ExprVisitor<'ast> for NodeFinder<'ast> {
    fn visit_unary(&mut self, unary: &'ast Unary, _: &'ast Expr) {
        unary.expr.accept(self);
    }

    fn visit_binary(&mut self, binary: &'ast Binary, _: &'ast Expr) {
        binary.lhs.accept(self);
        binary.rhs.accept(self);
    }

    fn visit_ident(&mut self, ident: aml_core::Location, expr: &'ast Expr) {
        if ident.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Expr(expr));
        }
    }

    fn visit_string(&mut self, string: aml_core::Location, expr: &'ast Expr) {
        if string.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Expr(expr));
        }
    }

    fn visit_call(&mut self, call: &'ast Call, _expr: &'ast Expr) {
        call.fun.accept(self);

        for arg in call.args.iter() {
            arg.accept(self);
        }
    }

    fn visit_primitive(&mut self, prim: &'ast PrimitiveExpr, expr: &'ast Expr) {
        if prim.location.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Expr(expr));
        }
    }

    fn visit_array_index(&mut self, index: &'ast ArrayIndex, _: &'ast Expr) {
        index.lhs.accept(self);
        index.index.accept(self);
    }

    fn visit_list(&mut self, list: &'ast List, _: &'ast Expr) {
        for item in list.items.iter() {
            item.accept(self);
        }
    }

    fn visit_map(&mut self, map: &'ast Map, _: &'ast Expr) {
        for (key, value) in map.items.iter() {
            key.accept(self);
            value.accept(self);
        }
    }

    fn visit_error(&mut self, error: &'ast ErrorExpr, expr: &'ast Expr) {
        if error.location.contains(self.byte_offset) {
            self.result = Some(NodeFinderResult::Expr(expr));
        }
    }
}

#[cfg(test)]
mod tests {
    use aml_core::Location;
    use aml_token::{Hex, Primitive};

    use super::*;

    #[test]
    fn test_find_node() {
        let template = r#"
vstack [foreground: #ff0000]
    text [background: #00ff00] "Hello, "
        span [background: #0000ff] "World!""#;

        let tokens = aml_token::Lexer::new(template).collect();
        let tokens = aml_token::Tokens::new(tokens, template.len());
        let ast = Parser::new(tokens).parse();

        let mut finder = NodeFinder {
            byte_offset: 55,
            result: None,
        };
        ast.accept(&mut finder);

        assert!(finder.result.is_some());
        assert_eq!(
            finder.result.unwrap(),
            NodeFinderResult::Expr(&Expr::Primitive(PrimitiveExpr {
                value: Primitive::Hex(Hex::from((0, 255, 0))),
                location: Location::new(52, 59),
            }))
        );

        let mut finder = NodeFinder {
            byte_offset: usize::MAX,
            result: None,
        };
        ast.accept(&mut finder);
        assert!(finder.result.is_none());

        let mut finder = NodeFinder {
            byte_offset: 69,
            result: None,
        };
        ast.accept(&mut finder);
        assert!(finder.result.is_some());
        assert_eq!(
            finder.result.unwrap(),
            NodeFinderResult::Node(&AstNode::String(Location::new(61, 70)))
        );
    }
}
