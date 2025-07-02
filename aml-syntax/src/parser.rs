use aml_token::{Element, Operator, TokenKind, Tokens};

use crate::ast::{Ast, AstNode, Scope};
use crate::expressions::parse_expression;

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
                TokenKind::Eof => break,
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
            Element::Text => self.parse_text(current_indent),
            Element::Span => self.parse_span(),
            t => todo!("unhandled element: {t:?}"),
        }
    }

    fn parse_text(&mut self, current_indent: usize) -> AstNode {
        let text = self.tokens.next_token();
        let location = text.location();

        self.tokens.consume_all_whitespace();
        let attributes = self.parse_optional_attributes();

        self.tokens.consume_all_whitespace();
        let value = match self.tokens.peek().kind() {
            TokenKind::String(_) => Some(Box::new(self.parse_string())),
            _ => None,
        };

        self.tokens.consume_newlines();

        let next_indent = match self.tokens.peek().kind() {
            TokenKind::Indent(i) => i,
            _ => 0,
        };

        let children = match next_indent > current_indent {
            true => self.parse_block(next_indent),
            false => vec![],
        };

        AstNode::Text {
            value,
            attributes,
            children,
            location,
        }
    }

    fn parse_span(&mut self) -> AstNode {
        let span = self.tokens.next_token();
        let location = span.location();

        self.tokens.consume_all_whitespace();
        let attributes = self.parse_optional_attributes();

        self.tokens.consume_all_whitespace();
        let value = match self.tokens.peek().kind() {
            TokenKind::String(_) => Some(Box::new(self.parse_string())),
            _ => None,
        };

        AstNode::Span {
            value,
            attributes,
            location,
        }
    }

    fn parse_string(&mut self) -> AstNode {
        let token = self.tokens.next_token();
        let value = token.location();
        AstNode::String { value }
    }

    fn parse_identifier(&mut self) -> AstNode {
        let identifier = self.tokens.next_token();
        let location = identifier.location();
        AstNode::Identifier { value: location }
    }

    fn parse_declaration(&mut self) -> AstNode {
        let keyword = self.tokens.next_token();
        let location = keyword.location();

        self.tokens.consume_indent();
        let name = self.parse_identifier();

        self.tokens.consume_indent();
        self.tokens.consume(); // consume equal sign
        let value = parse_expression(&mut self.tokens);

        AstNode::Declaration {
            name: Box::new(name),
            value,
            location,
        }
    }

    fn parse_optional_attributes(&mut self) -> Vec<AstNode> {
        if self.tokens.peek_skip_indent().kind() == TokenKind::Operator(Operator::LBracket) {
            return self.parse_attributes();
        }

        vec![]
    }

    fn parse_attributes(&mut self) -> Vec<AstNode> {
        self.tokens.consume(); // consume LBracket

        let mut attributes = vec![];

        loop {
            match self.tokens.peek_skip_indent().kind() {
                TokenKind::Operator(Operator::RBracket) => {
                    self.tokens.consume();
                    break;
                }
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

            attributes.push(AstNode::Attribute {
                name: Box::new(name),
                value,
            });

            self.skip_optional_comma();
        }

        attributes
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use aml_core::Location;
    use aml_token::Lexer;
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
        String {
            value: Location,
            text: &'ast str,
        },
        Text {
            value: Option<Box<SnapshotAstNode<'ast>>>,
            attributes: Vec<SnapshotAstNode<'ast>>,
            children: Vec<SnapshotAstNode<'ast>>,
            location: Location,
            text: &'ast str,
        },
        Span {
            value: Option<Box<SnapshotAstNode<'ast>>>,
            attributes: Vec<SnapshotAstNode<'ast>>,
            location: Location,
            text: &'ast str,
        },
        Identifier {
            value: Location,
            text: &'ast str,
        },
        Attribute {
            name: Box<SnapshotAstNode<'ast>>,
            value: SnapshotExpr<'ast>,
        },
        Declaration {
            name: Box<SnapshotAstNode<'ast>>,
            value: SnapshotExpr<'ast>,
            location: Location,
        },
    }

    impl<'ast> SnapshotAstNode<'ast> {
        fn from_node(node: AstNode, content: &'ast str) -> Self {
            match node {
                AstNode::String { value } => {
                    let text = &content[value.to_range()];
                    Self::String { value, text }
                }
                AstNode::Text {
                    value,
                    children,
                    location,
                    attributes,
                } => Self::Text {
                    value: value.map(|n| Box::new(SnapshotAstNode::from_node(*n, content))),
                    text: &content[location.to_range()],
                    attributes: attributes
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
                    value,
                    location,
                    attributes,
                } => Self::Span {
                    value: value.map(|n| Box::new(SnapshotAstNode::from_node(*n, content))),
                    attributes: attributes
                        .into_iter()
                        .map(|n| SnapshotAstNode::from_node(n, content))
                        .collect(),
                    text: &content[location.to_range()],
                    location,
                },
                AstNode::Identifier { value } => Self::Identifier {
                    text: &content[value.to_range()],
                    value,
                },
                AstNode::Attribute { name, value } => Self::Attribute {
                    name: Box::new(SnapshotAstNode::from_node(*name, content)),
                    value: SnapshotExpr::from_expr(value, content),
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
    fn test_parser() {
        let template = r#"

text "Hello"
    span "World""#;

        let tokens = Lexer::new(template).collect::<Vec<_>>();
        let tokens = Tokens::new(tokens, template.len());
        let parser = Parser::new(tokens);

        let ast = SnapshotAst::from_ast(parser.parse(), template);

        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_parsing_weirdly_formatted_attributes() {
        let template = r#"
text [
    foreground
    :
    "red",
    background

    :


    'green',
] "Hello"
    span [background: "yellow"] "World"
"#;

        insta::assert_yaml_snapshot!(get_ast(template));
    }

    #[test]
    fn test_parsing_attributes() {
        let template = r#"
text [foreground: "red"] "Hello"
    span [background: "yellow"] "World"
"#;

        insta::assert_yaml_snapshot!(get_ast(template));
    }
}
