use std::collections::HashMap;

use serde::Serialize;

use aml_core::Location;
use aml_token::{Element, Operator, TokenKind, Tokens};
use crate::ast::{Ast, AstNode, Expr, Scope};
use crate::expressions::{parse_expression};
use crate::error::Result;

pub struct Parser<'p> {
    scope_stack: Vec<usize>,
    content: &'p str,
    tokens: Tokens,
    ast: Ast,
}

impl<'p> Parser<'p> {
    pub fn new(mut tokens: Tokens, content: &'p str) -> Self {
        tokens.consume_newlines();

        Self {
            tokens,
            content,
            ast: Ast::default(),
            scope_stack: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<Ast> {
        let base_indent = match self.tokens.peek().kind() {
            TokenKind::Indent(indent) => indent,
            _ => 0,
        };

        self.ast.nodes = self.parse_block(base_indent)?;

        Ok(self.ast)
    }

    fn parse_block(&mut self, block_indent: usize) -> Result<Vec<AstNode>> {
        self.add_scope()?;

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

            nodes.push(self.parse_node(current_indent)?);
        }

        self.pop_scope()?;
        Ok(nodes)
    }

    fn parse_node(&mut self, current_indent: usize) -> Result<AstNode> {
        let node = match self.tokens.peek_skip_indent().kind() {
            TokenKind::Element(element) => self.parse_element(element, current_indent)?,
            TokenKind::String(_) => self.parse_string()?,
            TokenKind::Identifier(_) => self.parse_identifier()?,
            t => todo!("unhandled token in parse_node: {t:?}"),
        };

        Ok(node)
    }

    fn parse_element(&mut self, element: Element, current_indent: usize) -> Result<AstNode> {
        let node = match element {
            Element::Text => self.parse_text(current_indent)?,
            Element::Span => self.parse_span()?,
            t => todo!("unhandled element: {t:?}"),
        };

        Ok(node)
    }

    fn parse_text(&mut self, current_indent: usize) -> Result<AstNode> {
        let text = self.tokens.next_token();
        let location = text.location();

        self.tokens.consume_all_whitespace();
        let attributes = self.parse_optional_attributes()?;

        self.tokens.consume_all_whitespace();
        let value = match self.tokens.peek().kind() {
            TokenKind::String(_) => Some(Box::new(self.parse_string()?)),
            _ => None,
        };

        self.tokens.consume_newlines();

        let next_indent = match self.tokens.peek().kind() {
            TokenKind::Indent(i) => i,
            _ => 0,
        };

        let children = match next_indent > current_indent {
            true => self.parse_block(next_indent)?,
            false => vec![],
        };

        Ok(AstNode::Text {
            value,
            attributes,
            children,
            location,
        })
    }

    fn parse_span(&mut self) -> Result<AstNode> {
        let span = self.tokens.next_token();
        let location = span.location();

        self.tokens.consume_all_whitespace();
        let attributes = self.parse_optional_attributes()?;

        self.tokens.consume_all_whitespace();
        let value = match self.tokens.peek().kind() {
            TokenKind::String(_) => Some(Box::new(self.parse_string()?)),
            _ => None,
        };

        Ok(AstNode::Span {
            value,
            attributes,
            location,
        })
    }

    fn parse_string(&mut self) -> Result<AstNode> {
        let token = self.tokens.next_token();
        let value = token.location();
        Ok(AstNode::String { value })
    }

    fn parse_identifier(&mut self) -> Result<AstNode> {
        let identifier = self.tokens.next_token();
        let location = identifier.location();

        Ok(AstNode::Identifier { value: location })
    }

    fn parse_optional_attributes(&mut self) -> Result<Vec<AstNode>> {
        if self.tokens.peek_skip_indent().kind() == TokenKind::Operator(Operator::LBracket) {
            return self.parse_attributes();
        }

        Ok(vec![])
    }

    fn parse_attributes(&mut self) -> Result<Vec<AstNode>> {
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

            let name = self.parse_identifier()?;
            self.tokens.consume_all_whitespace();

            // TODO: this is a syntax error if there is no colon
            if self.tokens.peek().kind() == TokenKind::Operator(Operator::Colon) {
                self.tokens.consume();
            }

            self.tokens.consume_all_whitespace();
            let value = parse_expression(&mut self.tokens)?;

            attributes.push(AstNode::Attribute {
                name: Box::new(name),
                value: Box::new(value),
            });

            self.skip_optional_comma();
        }

        Ok(attributes)
    }

    fn skip_optional_comma(&mut self) {
        if self.tokens.peek_skip_indent().kind() == TokenKind::Operator(Operator::Comma) {
            self.tokens.consume();
        }
    }

    fn add_scope(&mut self) -> Result<()> {
        let scope_id = self.ast.scopes.len();
        let parent = self.scope_stack.last().copied();

        self.ast.scopes.push(Scope {
            variables: Vec::new(),
            parent,
        });

        self.scope_stack.push(scope_id);
        Ok(())
    }

    fn pop_scope(&mut self) -> Result<()> {
        self.scope_stack.pop();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use serde::Serialize;

    use super::*;
    use crate::expressions::test::SnapshotExpr;
    use aml_token::Lexer;

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
            value: Box<SnapshotExpr<'ast>>,
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
                    value: Box::new(SnapshotExpr::from_expr(*value, content)),
                },
            }
        }
    }

    fn get_ast(template: &str) -> SnapshotAst<'_> {
        let tokens = Lexer::new(template).collect::<aml_token::Result<Vec<_>>>().unwrap();
        let tokens = Tokens::new(tokens, template.len());
        let parser = Parser::new(tokens, template);
        SnapshotAst::from_ast(parser.parse().unwrap(), template)
    }

    #[test]
    fn test_parser() {
        let template = r#"

text "Hello"
    span "World""#;

        let tokens = Lexer::new(template).collect::<aml_token::Result<Vec<_>>>().unwrap();
        let tokens = Tokens::new(tokens, template.len());
        let parser = Parser::new(tokens, template);

        let ast = SnapshotAst::from_ast(parser.parse().unwrap(), template);

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