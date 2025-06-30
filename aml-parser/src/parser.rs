use std::collections::HashMap;

use serde::Serialize;

use crate::TokenKind;
use crate::error::Result;
use crate::expressions::{Expr, parse_expression};
use crate::token::{Element, Location, Operator, Tokens};

#[derive(Debug, Default)]
pub struct Ast {
    pub nodes: Vec<AstNode>,
    pub variables: HashMap<String, Location>,
    pub scopes: Vec<Scope>,
}

#[derive(Debug)]
pub enum AstNode {
    String {
        value: Location,
    },
    Text {
        value: Option<Box<AstNode>>,
        attributes: Vec<AstNode>,
        children: Vec<AstNode>,
        location: Location,
    },
    Span {
        value: Option<Box<AstNode>>,
        attributes: Vec<AstNode>,
        location: Location,
    },
    Identifier {
        value: Location,
    },
    Attribute {
        name: Box<AstNode>,
        value: Box<Expr>,
    },
}

#[derive(Debug, Serialize)]
pub struct Scope {
    pub variables: Vec<String>,
    pub parent: Option<usize>,
}

pub struct Parser<'p> {
    tokens: Tokens,
    content: &'p str,
    ast: Ast,
    scope_stack: Vec<usize>,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: Tokens, content: &'p str) -> Self {
        Self {
            tokens,
            content,
            ast: Ast::default(),
            scope_stack: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<Ast> {
        self.add_scope()?;

        while self.tokens.peek().kind() != TokenKind::Eof {
            let Some(node) = self.parse_node()? else { continue };
            self.ast.nodes.push(node);
        }

        Ok(self.ast)
    }

    fn parse_node(&mut self) -> Result<Option<AstNode>> {
        let node = match self.tokens.peek().kind() {
            TokenKind::Element(_) => Some(self.parse_element()?),
            TokenKind::String => Some(self.parse_string()?),
            TokenKind::Newline => {
                self.tokens.consume();
                None
            }
            t => todo!("{t:?}"),
        };

        Ok(node)
    }

    fn parse_element(&mut self) -> Result<AstNode> {
        let token = self.tokens.next_token();

        let TokenKind::Element(element) = token.kind() else {
            unreachable!();
        };

        let node = match element {
            Element::Text => self.parse_text()?,
            Element::Span => self.parse_span()?,
            t => todo!("{t:?}"),
        };

        Ok(node)
    }

    fn parse_string(&mut self) -> Result<AstNode> {
        let token = self.tokens.next_token();
        let value = token.location();
        Ok(AstNode::String { value })
    }

    fn parse_text(&mut self) -> Result<AstNode> {
        let keyword = self.tokens.next_token();
        let location = keyword.location();

        let attributes = self.parse_optional_attributes()?;

        let value = match self.tokens.peek().kind() {
            TokenKind::String => Some(Box::new(self.parse_string()?)),
            _ => None,
        };

        let children = self.parse_children()?;

        Ok(AstNode::Text {
            value,
            attributes,
            children,
            location,
        })
    }

    fn parse_identifier(&mut self) -> Result<AstNode> {
        let identifier = self.tokens.next_token();

        Ok(AstNode::Identifier {
            value: identifier.location(),
        })
    }

    fn parse_optional_attributes(&mut self) -> Result<Vec<AstNode>> {
        let has_attribute = matches!(
            self.tokens.peek().kind(),
            TokenKind::Operator(Operator::LBracket)
        );

        if has_attribute {
            return self.parse_attributes();
        }

        Ok(vec![])
    }

    fn parse_attributes(&mut self) -> Result<Vec<AstNode>> {
        self.tokens.consume();

        let mut attributes = vec![];

        loop {
            match self.tokens.peek().kind() {
                TokenKind::Operator(Operator::RBracket) => {
                    self.tokens.consume();
                    break;
                }
                _ => {}
            }

            let name = self.parse_identifier()?;
            self.tokens.consume();
            let value = parse_expression(&mut self.tokens)?;

            attributes.push(AstNode::Attribute {
                name: Box::new(name),
                value: Box::new(value),
            });

            if matches!(
                self.tokens.peek().kind(),
                TokenKind::Operator(Operator::Comma)
            ) {
                self.tokens.consume();
            }
        }

        Ok(attributes)
    }

    fn parse_children(&mut self) -> Result<Vec<AstNode>> {
        let mut children = vec![];

        if !matches!(self.tokens.peek().kind(), TokenKind::Indent(_)) {
            return Ok(children);
        }

        self.tokens.consume(); // consume indent
        self.add_scope()?;

        loop {
            if self.tokens.peek().kind() == TokenKind::Eof {
                self.pop_scope()?;
                break;
            }

            let Some(node) = self.parse_node()? else {
                continue;
            };
            children.push(node);
        }

        Ok(children)
    }

    fn parse_span(&mut self) -> Result<AstNode> {
        let keyword = self.tokens.next_token();
        let location = keyword.location();

        let attributes = self.parse_optional_attributes()?;

        let value = match self.tokens.peek().kind() {
            TokenKind::String => Some(Box::new(self.parse_string()?)),
            _ => None,
        };

        Ok(AstNode::Span {
            value,
            attributes,
            location,
        })
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
    use crate::lexer::Lexer;

    #[derive(Serialize)]
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

    #[derive(Serialize)]
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
                AstNode::Attribute { name, .. } => Self::Attribute {
                    name: Box::new(SnapshotAstNode::from_node(*name, content)),
                },
            }
        }
    }

    #[test]
    fn test_parser() {
        let template = r#"text "Hello"
    span "World""#;

        let tokens = Lexer::new(template).collect::<Result<Vec<_>>>().unwrap();
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

        let tokens = Lexer::new(template).collect::<Result<Vec<_>>>().unwrap();
        let tokens = Tokens::new(tokens, template.len());
        let parser = Parser::new(tokens, template);

        let ast = SnapshotAst::from_ast(parser.parse().unwrap(), template);

        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_parsing_attributes() {
        let template = r#"
text [foreground: "red"] "Hello"
    span [background: "yellow"] "World"
"#;

        let tokens = Lexer::new(template).collect::<Result<Vec<_>>>().unwrap();
        let tokens = Tokens::new(tokens, template.len());
        let parser = Parser::new(tokens, template);

        let ast = SnapshotAst::from_ast(parser.parse().unwrap(), template);

        insta::assert_yaml_snapshot!(ast);
    }
}
