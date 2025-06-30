use std::collections::HashMap;

use serde::Serialize;

use crate::error::Result;
use crate::lexer::{Element, Lexer, Location, TokenKind, TransposeRef};

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
        children: Vec<AstNode>,
        location: Location,
    },
    Span {
        value: Option<Box<AstNode>>,
        location: Location,
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
            let node = self.parse_node()?;
            self.ast.nodes.push(node);
        }

        Ok(self.ast)
    }

    fn parse_node(&mut self) -> Result<AstNode> {
        match peek!(self.lexer) {
            Some(token) if matches!(token.kind, TokenKind::Element(_)) => self.parse_element(),
            Some(token) if matches!(token.kind, TokenKind::String) => self.parse_string(),
            t => todo!("{t:#?}"),
        }
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
            _ => todo!(),
        };

        Ok(node)
    }

    fn parse_text(&mut self) -> Result<AstNode> {
        let keyword = expect!(self.lexer, TokenKind::Element(Element::Text));
        let location = keyword.location;

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
                _ => children.push(self.parse_node()?),
            }
        }

        Ok(AstNode::Text {
            value,
            children,
            location,
        })
    }

    fn parse_span(&mut self) -> Result<AstNode> {
        let keyword = expect!(self.lexer, TokenKind::Element(Element::Span));
        let location = keyword.location;

        let value = match peek!(self.lexer) {
            Some(token) if matches!(token.kind, TokenKind::String) => {
                Some(Box::new(self.parse_string()?))
            }
            _ => None,
        };

        Ok(AstNode::Span { value, location })
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
            children: Vec<SnapshotAstNode<'ast>>,
            location: Location,
            text: &'ast str,
        },
        Span {
            value: Option<Box<SnapshotAstNode<'ast>>>,
            location: Location,
            text: &'ast str,
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
                } => Self::Text {
                    value: value.map(|n| Box::new(SnapshotAstNode::from_node(*n, content))),
                    text: &content[location.to_range()],
                    children: children
                        .into_iter()
                        .map(|n| SnapshotAstNode::from_node(n, content))
                        .collect(),
                    location,
                },
                AstNode::Span { value, location } => Self::Span {
                    value: value.map(|n| Box::new(SnapshotAstNode::from_node(*n, content))),
                    text: &content[location.to_range()],
                    location,
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
}
