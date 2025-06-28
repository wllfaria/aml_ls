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
macro_rules! skip {
    ($lexer:expr) => {
        _ = $lexer.next().transpose()?
    };
}

pub type NodeId = usize;

#[derive(Debug, Serialize, Default)]
pub struct Ast {
    pub nodes: Vec<AstNode>,
    pub variables: HashMap<String, Location>,
    pub scopes: Vec<Scope>,
}

#[derive(Debug, Serialize)]
pub enum AstNode {
    String {
        value: Location,
    },
    Text {
        value: Option<Box<AstNode>>,
        children: Vec<AstNode>,
    },
    Span {
        value: Option<Location>,
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
        let token = expect!(self.lexer, TokenKind::Element(_));

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
                    skip!(self.lexer);
                    self.add_scope()?;
                }
                TokenKind::Dedent => {
                    skip!(self.lexer);
                    self.pop_scope()?
                }
                TokenKind::Newline => {
                    skip!(self.lexer);
                    continue;
                }
                _ => children.push(self.parse_node()?),
            }
        }

        Ok(AstNode::Text { value, children })
    }

    fn parse_span(&mut self) -> Result<AstNode> {
        let value = match peek!(self.lexer) {
            Some(token) if matches!(token.kind, TokenKind::String) => {
                let token = expect!(self.lexer, TokenKind::String);
                Some(token.location)
            }
            _ => None,
        };

        Ok(AstNode::Span { value })
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

    #[test]
    fn test_parser() {
        let template = r#"text "Hello"
    span "World""#;

        let lexer = Lexer::new(template);
        let parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();

        insta::assert_yaml_snapshot!(ast);
    }
}
