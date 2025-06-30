use std::collections::HashMap;

use serde::Serialize;

use crate::error::Result;
use crate::lexer::{Element, Lexer, Location, Operator, TokenKind, TransposeRef};

#[macro_export]
macro_rules! expect {
    ($lexer:expr, $kind:pat) => {{
        let Some(token) = $lexer.next().transpose()? else {
            todo!();
        };

        if !matches!(token.kind, $kind) {
            todo!();
        }

        token
    }};
}

#[macro_export]
macro_rules! peek {
    ($lexer:expr) => {
        $lexer.peek().transpose().map_err(|e| e.clone())?
    };
}

#[macro_export]
macro_rules! expect_peek {
    ($lexer:expr, $kind:pat) => {{
        let token = peek!($lexer);
        if token.is_none() || !matches!(token.unwrap().kind, $kind) {
            todo!();
        }
        token.unwrap()
    }};
}

#[macro_export]
macro_rules! consume {
    ($lexer:expr) => {
        $lexer.next().transpose()?
    };
}

pub type NodeId = usize;

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
        value: Box<AstNode>,
    },
}

#[derive(Debug, Serialize)]
pub struct Scope {
    pub variables: Vec<String>,
    pub parent: Option<usize>,
}

pub struct Parser<'p> {
    lexer: Lexer<'p>,
    ast: Ast,
    scope_stack: Vec<usize>,
}

impl<'p> Parser<'p> {
    pub fn new(lexer: Lexer<'p>) -> Self {
        Self {
            lexer,
            ast: Ast::default(),
            scope_stack: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<Ast> {
        self.add_scope()?;

        while !self.lexer.is_empty() {
            let Some(node) = self.parse_node()? else { continue };
            self.ast.nodes.push(node);
        }

        Ok(self.ast)
    }

    fn parse_node(&mut self) -> Result<Option<AstNode>> {
        let node = match peek!(self.lexer) {
            Some(token) if matches!(token.kind, TokenKind::Element(_)) => {
                Some(self.parse_element()?)
            }
            Some(token) if matches!(token.kind, TokenKind::String) => Some(self.parse_string()?),
            Some(token) if matches!(token.kind, TokenKind::Newline) => {
                consume!(self.lexer);
                None
            }
            t => todo!("{t:#?}"),
        };

        Ok(node)
    }

    fn parse_string(&mut self) -> Result<AstNode> {
        let token = expect!(self.lexer, TokenKind::String);
        let value = token.location;
        Ok(AstNode::String { value })
    }

    fn parse_element(&mut self) -> Result<AstNode> {
        let token = expect_peek!(self.lexer, TokenKind::Element(_));

        let TokenKind::Element(element) = token.kind else {
            unreachable!();
        };

        let node = match element {
            Element::Text => self.parse_text()?,
            Element::Span => self.parse_span()?,
            t => todo!("{t:?}"),
        };

        Ok(node)
    }

    fn parse_text(&mut self) -> Result<AstNode> {
        let keyword = expect!(self.lexer, TokenKind::Element(Element::Text));
        let location = keyword.location;

        let attributes = match peek!(self.lexer) {
            Some(token) if matches!(token.kind, TokenKind::Operator(Operator::LBracket)) => {
                self.parse_attributes()?
            }
            _ => vec![],
        };

        let value = match peek!(self.lexer) {
            Some(token) if matches!(token.kind, TokenKind::String) => {
                Some(Box::new(self.parse_string()?))
            }
            _ => None,
        };

        let mut children = vec![];

        while let Some(token) = peek!(self.lexer) {
            match token.kind {
                TokenKind::Indent => {
                    consume!(self.lexer);
                    self.add_scope()?;
                }
                TokenKind::Dedent => {
                    consume!(self.lexer);
                    self.pop_scope()?
                }
                TokenKind::Newline => {
                    consume!(self.lexer);
                    continue;
                }
                _ => {
                    let Some(node) = self.parse_node()? else { continue };
                    children.push(node)
                }
            }
        }

        Ok(AstNode::Text {
            value,
            attributes,
            children,
            location,
        })
    }

    fn parse_identifier(&mut self) -> Result<AstNode> {
        let identifier = expect!(self.lexer, TokenKind::Identifier);

        Ok(AstNode::Identifier {
            value: identifier.location,
        })
    }

    fn parse_attributes(&mut self) -> Result<Vec<AstNode>> {
        expect!(self.lexer, TokenKind::Operator(Operator::LBracket));

        let mut attributes = vec![];

        loop {
            match peek!(self.lexer) {
                Some(token) if matches!(token.kind, TokenKind::Operator(Operator::RBracket)) => {
                    consume!(self.lexer);
                    break;
                }
                None => break,
                _ => {}
            }

            let name = self.parse_identifier()?;
            expect!(self.lexer, TokenKind::Operator(Operator::Colon));
            let value = self.parse_expression()?;

            attributes.push(AstNode::Attribute {
                name: Box::new(name),
                value: Box::new(value),
            })
        }

        Ok(attributes)
    }

    fn parse_expression(&mut self) -> Result<AstNode> {
        let lhs = match peek!(self.lexer) {
            Some(token) if matches!(token.kind, TokenKind::String) => self.parse_string()?,
            _ => todo!(),
        };

        Ok(lhs)
    }

    fn parse_span(&mut self) -> Result<AstNode> {
        let keyword = expect!(self.lexer, TokenKind::Element(Element::Span));
        let location = keyword.location;

        let attributes = match peek!(self.lexer) {
            Some(token) if matches!(token.kind, TokenKind::Operator(Operator::LBracket)) => {
                self.parse_attributes()?
            }
            _ => vec![],
        };

        let value = match peek!(self.lexer) {
            Some(token) if matches!(token.kind, TokenKind::String) => {
                Some(Box::new(self.parse_string()?))
            }
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
            value: Box<SnapshotAstNode<'ast>>,
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
                AstNode::Attribute { name, value } => Self::Attribute {
                    name: Box::new(SnapshotAstNode::from_node(*name, content)),
                    value: Box::new(SnapshotAstNode::from_node(*value, content)),
                },
            }
        }
    }

    #[test]
    fn test_parser() {
        let template = r#"text "Hello"
    span "World""#;

        let lexer = Lexer::new(template);
        let parser = Parser::new(lexer);

        let ast = SnapshotAst::from_ast(parser.parse().unwrap(), template);

        insta::assert_yaml_snapshot!(ast);
    }

    #[test]
    fn test_parsing_attributes() {
        let template = r#"
text [foreground: "red"] "Hello"
    span [background: "yellow"] "World"
"#;

        let lexer = Lexer::new(template);
        let parser = Parser::new(lexer);

        let ast = SnapshotAst::from_ast(parser.parse().unwrap(), template);

        insta::assert_yaml_snapshot!(ast);
    }
}
