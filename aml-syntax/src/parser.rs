use aml_token::{Element, Operator, TokenKind, Tokens};

use crate::Expr;
use crate::ast::{Ast, AstNode, Attributes, Scope};
use crate::expressions::parse_expression;

struct LocationCalculator;

impl LocationCalculator {
    fn merge_location_with_values(
        start: aml_core::Location,
        attributes: &Attributes,
        values: Option<&Vec<AstNode>>,
        children: Option<&Vec<AstNode>>,
    ) -> aml_core::Location {
        let value_location = values.and_then(|v| v.iter().last().map(|node| node.location()));
        let children_location = children.and_then(|c| c.iter().last().map(|node| node.location()));

        match (children_location, value_location, attributes.location) {
            (Some(location), _, _) => start.merge(location),
            (_, Some(location), _) => start.merge(location),
            (_, _, Some(location)) => start.merge(location),
            (None, None, None) => start,
        }
    }

    fn merge_location_without_values(
        start: aml_core::Location,
        attributes: &Attributes,
        children: &[AstNode],
    ) -> aml_core::Location {
        let children_location = children.iter().last().map(|node| node.location());

        match (children_location, attributes.location) {
            (Some(location), _) => start.merge(location),
            (_, Some(location)) => start.merge(location),
            (None, None) => start,
        }
    }
}

trait ElementParser {
    fn parse_element(&self, parser: &mut Parser, current_indent: usize) -> AstNode;
}

struct TextElementParser;
struct SpanElementParser;
struct VStackElementParser;
struct HStackElementParser;

impl ElementParser for TextElementParser {
    fn parse_element(&self, parser: &mut Parser, current_indent: usize) -> AstNode {
        let text = parser.tokens.next_token();
        assert!(text.kind() == TokenKind::Element(Element::Text));

        let start_location = text.location();
        let attributes = parser.maybe_parse_attributes();
        let values = parser.parse_values();
        let children = parser.maybe_parse_block(current_indent);

        let location = LocationCalculator::merge_location_with_values(
            start_location,
            &attributes,
            Some(&values),
            Some(&children),
        );

        AstNode::Text {
            values,
            attributes,
            children,
            location,
        }
    }
}

impl ElementParser for SpanElementParser {
    fn parse_element(&self, parser: &mut Parser, _current_indent: usize) -> AstNode {
        let span = parser.tokens.next_token();
        assert!(span.kind() == TokenKind::Element(Element::Span));

        let start_location = span.location();
        let attributes = parser.maybe_parse_attributes();
        let values = parser.parse_values();

        let location =
            LocationCalculator::merge_location_without_values(start_location, &attributes, &values);

        AstNode::Span {
            values,
            attributes,
            location,
        }
    }
}

impl ElementParser for VStackElementParser {
    fn parse_element(&self, parser: &mut Parser, current_indent: usize) -> AstNode {
        let vstack = parser.tokens.next_token();
        assert!(vstack.kind() == TokenKind::Element(Element::VStack));

        let start_location = vstack.location();
        let attributes = parser.maybe_parse_attributes();
        let children = parser.maybe_parse_block(current_indent);

        let location = LocationCalculator::merge_location_without_values(
            start_location,
            &attributes,
            &children,
        );

        AstNode::VStack {
            children,
            attributes,
            location,
        }
    }
}

impl ElementParser for HStackElementParser {
    fn parse_element(&self, parser: &mut Parser, current_indent: usize) -> AstNode {
        let hstack = parser.tokens.next_token();
        assert!(hstack.kind() == TokenKind::Element(Element::HStack));

        let start_location = hstack.location();
        let attributes = parser.maybe_parse_attributes();
        let children = parser.maybe_parse_block(current_indent);

        let location = LocationCalculator::merge_location_without_values(
            start_location,
            &attributes,
            &children,
        );

        AstNode::HStack {
            children,
            attributes,
            location,
        }
    }
}

pub struct Parser {
    scope_stack: Vec<usize>,
    tokens: Tokens,
    ast: Ast,
}

impl Parser {
    pub fn new(mut tokens: Tokens) -> Self {
        tokens.consume_newlines();

        Self {
            tokens,
            ast: Ast::default(),
            scope_stack: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Ast {
        let base_indent = match self.tokens.peek().kind() {
            TokenKind::Indent(indent) => indent,
            _ => 0,
        };

        self.ast.nodes = self.parse_block(base_indent);
        self.ast
    }

    fn parse_block(&mut self, block_indent: usize) -> Vec<AstNode> {
        self.add_scope();

        let mut nodes = vec![];

        loop {
            self.tokens.consume_newlines();

            let current_indent = match self.tokens.peek().kind() {
                TokenKind::Indent(i) => {
                    self.tokens.consume();
                    i
                }
                _ => 0,
            };

            if current_indent < block_indent {
                break;
            }

            if current_indent > block_indent {
                break;
            }

            if self.tokens.peek().kind() == TokenKind::Eof {
                break;
            }

            nodes.push(self.parse_node(current_indent));
        }

        self.pop_scope();
        nodes
    }

    fn parse_node(&mut self, current_indent: usize) -> AstNode {
        match self.tokens.peek_skip_indent().kind() {
            TokenKind::Element(element) => self.parse_element(element, current_indent),
            TokenKind::String(_) => self.parse_string(),
            TokenKind::Identifier(_) => self.parse_identifier(),
            TokenKind::Decl => self.parse_declaration(),
            t => todo!("unhandled token in parse_node: {t:?}"),
        }
    }

    fn parse_element(&mut self, element: Element, current_indent: usize) -> AstNode {
        match element {
            Element::Text => TextElementParser.parse_element(self, current_indent),
            Element::Span => SpanElementParser.parse_element(self, 0),
            Element::VStack => VStackElementParser.parse_element(self, current_indent),
            Element::HStack => HStackElementParser.parse_element(self, current_indent),
            t => todo!("unhandled element: {t:?}"),
        }
    }

    fn parse_values(&mut self) -> Vec<AstNode> {
        let mut values = vec![];
        loop {
            let next_token = self.tokens.peek_skip_indent();
            match next_token.kind() {
                TokenKind::Newline => break,
                TokenKind::Eof => break,
                TokenKind::Identifier(_) => values.push(self.parse_identifier()),
                TokenKind::Primitive(_) => values.push(self.parse_primitive()),
                TokenKind::String(_) => values.push(self.parse_string()),
                token => {
                    self.tokens.consume();
                    values.push(AstNode::Error {
                        token,
                        location: next_token.location(),
                    })
                }
            }
        }
        values
    }

    fn parse_primitive(&mut self) -> AstNode {
        let primitive = self.tokens.next_token();
        let TokenKind::Primitive(value) = primitive.kind() else { unreachable!() };
        let location = primitive.location();
        AstNode::Primitive { location, value }
    }

    fn parse_string(&mut self) -> AstNode {
        let token = self.tokens.next_token();
        let location = token.location();
        AstNode::String { location }
    }

    fn parse_identifier(&mut self) -> AstNode {
        let token = self.tokens.next_token();
        let TokenKind::Identifier(location) = token.kind() else {
            return AstNode::Error {
                location: token.location(),
                token: token.kind(),
            };
        };
        AstNode::Identifier { location }
    }

    fn parse_declaration(&mut self) -> AstNode {
        let keyword = self.tokens.next_token();
        let start_location = keyword.location();

        self.tokens.consume_indent();
        let name = self.parse_identifier();

        self.tokens.consume_indent();
        self.tokens.consume(); // consume equal sign
        let value = parse_expression(&mut self.tokens);

        let location = start_location.merge(value.location());
        AstNode::Declaration {
            name: Box::new(name),
            value,
            location,
        }
    }

    fn maybe_parse_attributes(&mut self) -> Attributes {
        self.tokens.consume_indent();
        if self.tokens.peek_skip_indent().kind() == TokenKind::Operator(Operator::LBracket) {
            let attributes = self.parse_attributes();
            self.tokens.consume_indent();
            return attributes;
        }

        Attributes::default()
    }

    fn maybe_parse_block(&mut self, current_indent: usize) -> Vec<AstNode> {
        self.tokens.consume_newlines();

        let next_indent = match self.tokens.peek().kind() {
            TokenKind::Indent(i) => i,
            _ => 0,
        };

        match next_indent > current_indent {
            true => self.parse_block(next_indent),
            false => vec![],
        }
    }

    fn parse_attributes(&mut self) -> Attributes {
        let token = self.tokens.next_token();
        let start_location = token.location();
        assert!(token.kind() == TokenKind::Operator(Operator::LBracket));

        let mut attributes = vec![];
        let end_location = loop {
            let next_token = self.tokens.peek_skip_indent();

            match next_token.kind() {
                TokenKind::Operator(Operator::RBracket) => {
                    self.tokens.consume();
                    break next_token.location();
                }
                TokenKind::Eof => break next_token.location(),
                TokenKind::Newline => {
                    self.tokens.consume();
                    continue;
                }
                _ => {}
            }

            let name = self.parse_identifier();
            self.tokens.consume_all_whitespace();

            // TODO: this is a syntax error if there is no colon
            if self.tokens.peek().kind() == TokenKind::Operator(Operator::Colon) {
                self.tokens.consume();
            }

            self.tokens.consume_all_whitespace();
            let value = parse_expression(&mut self.tokens);

            if expression_has_error(&value) {
                loop {
                    match self.tokens.peek().kind() {
                        TokenKind::Operator(Operator::RBracket) => break,
                        TokenKind::Eof => break,
                        _ => self.tokens.consume(),
                    }
                }
            }

            let location = name.location().merge(value.location());
            attributes.push(AstNode::Attribute {
                name: Box::new(name),
                value,
                location,
            });

            self.skip_optional_comma();
        };

        Attributes {
            attributes,
            location: Some(start_location.merge(end_location)),
        }
    }

    fn skip_optional_comma(&mut self) {
        if self.tokens.peek_skip_indent().kind() == TokenKind::Operator(Operator::Comma) {
            self.tokens.consume();
        }
    }

    fn add_scope(&mut self) {
        let scope_id = self.ast.scopes.len();
        let parent = self.scope_stack.last().copied();

        self.ast.scopes.push(Scope {
            variables: Vec::new(),
            parent,
        });

        self.scope_stack.push(scope_id);
    }

    fn pop_scope(&mut self) {
        self.scope_stack.pop();
    }
}

fn expression_has_error(expr: &Expr) -> bool {
    match expr {
        Expr::Error { .. } => true,
        Expr::Ident { .. } => false,
        Expr::Unary { expr, .. } => expression_has_error(expr),
        Expr::Binary { lhs, rhs, .. } => {
            let lhs_error = expression_has_error(lhs);
            let rhs_error = expression_has_error(rhs);
            lhs_error || rhs_error
        }
        Expr::String { .. } => false,
        Expr::Call { args, fun, .. } => {
            let args_error = args.iter().any(expression_has_error);
            let fun_error = expression_has_error(fun);
            args_error || fun_error
        }
        Expr::Primitive { .. } => false,
        Expr::ArrayIndex { lhs, index, .. } => {
            let lhs_error = expression_has_error(lhs);
            let index_error = expression_has_error(index);
            lhs_error || index_error
        }
        Expr::List { items, .. } => items.iter().any(expression_has_error),
        Expr::Map { items, .. } => items
            .iter()
            .any(|(key, value)| expression_has_error(key) || expression_has_error(value)),
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use aml_core::Location;
    use aml_token::{Lexer, Primitive};
    use serde::Serialize;

    use super::*;
    use crate::expressions::test::SnapshotExpr;

    #[derive(Debug, Serialize)]
    struct SnapshotAst<'ast> {
        pub nodes: Vec<SnapshotAstNode<'ast>>,
        pub variables: HashMap<String, Location>,
        pub scopes: Vec<Scope>,
    }

    impl<'ast> SnapshotAst<'ast> {
        pub fn from_ast(ast: Ast, content: &'ast str) -> Self {
            Self {
                nodes: ast
                    .nodes
                    .into_iter()
                    .map(|n| SnapshotAstNode::from_node(n, content))
                    .collect(),
                variables: ast.variables,
                scopes: ast.scopes,
            }
        }
    }

    #[derive(Debug, Serialize)]
    enum SnapshotAstNode<'ast> {
        Primitive {
            value: Primitive,
            location: Location,
            original: &'ast str,
        },
        String {
            value: &'ast str,
            location: Location,
        },
        Text {
            values: Vec<SnapshotAstNode<'ast>>,
            attributes: Vec<SnapshotAstNode<'ast>>,
            children: Vec<SnapshotAstNode<'ast>>,
            text: &'ast str,
            location: Location,
        },
        Span {
            values: Vec<SnapshotAstNode<'ast>>,
            attributes: Vec<SnapshotAstNode<'ast>>,
            value: &'ast str,
            location: Location,
        },
        VStack {
            children: Vec<SnapshotAstNode<'ast>>,
            location: Location,
            attributes: Vec<SnapshotAstNode<'ast>>,
            original: &'ast str,
        },
        HStack {
            children: Vec<SnapshotAstNode<'ast>>,
            location: Location,
            attributes: Vec<SnapshotAstNode<'ast>>,
            original: &'ast str,
        },
        Identifier {
            value: &'ast str,
            location: Location,
        },
        Attribute {
            name: Box<SnapshotAstNode<'ast>>,
            value: SnapshotExpr<'ast>,
            location: Location,
            original: &'ast str,
        },
        Declaration {
            name: Box<SnapshotAstNode<'ast>>,
            value: SnapshotExpr<'ast>,
            location: Location,
        },
        Error {
            token: TokenKind,
            location: Location,
        },
    }

    impl<'ast> SnapshotAstNode<'ast> {
        fn from_node(node: AstNode, content: &'ast str) -> Self {
            match node {
                AstNode::Primitive { location, value } => Self::Primitive {
                    location,
                    value,
                    original: &content[location.to_range()],
                },
                AstNode::String { location } => {
                    let value = &content[location.to_range()];
                    Self::String { location, value }
                }
                AstNode::Text {
                    values,
                    children,
                    location,
                    attributes,
                } => Self::Text {
                    values: values
                        .into_iter()
                        .map(|n| SnapshotAstNode::from_node(n, content))
                        .collect(),
                    text: &content[location.to_range()],
                    attributes: attributes
                        .attributes
                        .into_iter()
                        .map(|n| SnapshotAstNode::from_node(n, content))
                        .collect(),
                    children: children
                        .into_iter()
                        .map(|n| SnapshotAstNode::from_node(n, content))
                        .collect(),
                    location,
                },
                AstNode::Span {
                    values,
                    location,
                    attributes,
                } => Self::Span {
                    values: values
                        .into_iter()
                        .map(|n| SnapshotAstNode::from_node(n, content))
                        .collect(),
                    attributes: attributes
                        .attributes
                        .into_iter()
                        .map(|n| SnapshotAstNode::from_node(n, content))
                        .collect(),
                    value: &content[location.to_range()],
                    location,
                },
                AstNode::VStack {
                    children,
                    location,
                    attributes,
                } => Self::VStack {
                    location,
                    children: children
                        .into_iter()
                        .map(|n| SnapshotAstNode::from_node(n, content))
                        .collect(),
                    attributes: attributes
                        .attributes
                        .into_iter()
                        .map(|n| SnapshotAstNode::from_node(n, content))
                        .collect(),
                    original: &content[location.to_range()],
                },
                AstNode::HStack {
                    children,
                    location,
                    attributes,
                } => Self::HStack {
                    location,
                    children: children
                        .into_iter()
                        .map(|n| SnapshotAstNode::from_node(n, content))
                        .collect(),
                    attributes: attributes
                        .attributes
                        .into_iter()
                        .map(|n| SnapshotAstNode::from_node(n, content))
                        .collect(),
                    original: &content[location.to_range()],
                },
                AstNode::Identifier { location } => Self::Identifier {
                    value: &content[location.to_range()],
                    location,
                },
                AstNode::Attribute {
                    name,
                    value,
                    location,
                } => Self::Attribute {
                    location,
                    name: Box::new(SnapshotAstNode::from_node(*name, content)),
                    value: SnapshotExpr::from_expr(value, content),
                    original: &content[location.to_range()],
                },
                AstNode::Declaration {
                    name,
                    value,
                    location,
                } => Self::Declaration {
                    name: Box::new(SnapshotAstNode::from_node(*name, content)),
                    value: SnapshotExpr::from_expr(value, content),
                    location,
                },
                AstNode::Error { location, token } => Self::Error { location, token },
            }
        }
    }

    fn get_ast(template: &str) -> SnapshotAst<'_> {
        let tokens = Lexer::new(template).collect::<Vec<_>>();
        let tokens = Tokens::new(tokens, template.len());
        let parser = Parser::new(tokens);
        SnapshotAst::from_ast(parser.parse(), template)
    }

    #[test]
    fn test_simple_text_element() {
        let template = r#"text "Hello""#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_text_element_with_attributes() {
        let template = r#"text [foreground: #ff0000] "Hello""#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_text_element_with_multiple_attributes() {
        let template = r#"text [foreground: #ff0000, background: #00ff00] "Hello""#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_text_with_multiple_values() {
        let template = r#"text "Hello" world true 1"#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_simple_span_element() {
        let template = r#"span "Hello""#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_span_element_with_attributes() {
        let template = r#"span [foreground: #ff0000] "Hello""#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn parse_span_element_with_multiple_attributes() {
        let template = r#"span [foreground: #ff0000, background: #00ff00] "Hello""#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_span_with_multiple_values() {
        let template = r#"span "Hello" world true 1"#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_vstack_with_no_children() {
        let template = r#"vstack"#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_vstack_with_children() {
        let template = r#"
vstack
    text "Hello"
"#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_vstack_with_multiple_children_and_attributes() {
        let template = r#"
vstack [width: 10]
    text "Hello"
    text "World"
"#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_hstack_with_children() {
        let template = r#"
hstack
    text "Hello"
"#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_hstack_with_multiple_children_and_attributes() {
        let template = r#"
hstack [width: 10]
    text "Hello"
    text "World"
"#;
        let ast = get_ast(template);
        insta::assert_yaml_snapshot!(ast);
    }
}
