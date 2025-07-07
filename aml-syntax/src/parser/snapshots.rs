use std::collections::HashMap;

use aml_core::Location;
use aml_token::{Container, Primitive};
use serde::Serialize;

use crate::ast::{Ast, AstNode, Declaration, DeclarationKind, Scope};
use crate::expressions::snapshots::SnapshotExpr;

#[derive(Debug, Serialize)]
pub struct SnapshotAst<'ast> {
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
pub struct SnapshotDeclaration<'ast> {
    kind: DeclarationKind,
    name: Box<SnapshotAstNode<'ast>>,
    value: SnapshotExpr<'ast>,
    location: Location,
}

impl<'ast> SnapshotDeclaration<'ast> {
    fn from_declaration(declaration: Declaration, content: &'ast str) -> Self {
        Self {
            kind: declaration.kind,
            name: Box::new(SnapshotAstNode::from_node(*declaration.name, content)),
            value: SnapshotExpr::from_expr(declaration.value, content),
            location: declaration.location,
        }
    }
}

impl<'ast> From<SnapshotDeclaration<'ast>> for SnapshotAstNode<'ast> {
    fn from(declaration: SnapshotDeclaration<'ast>) -> Self {
        Self::Declaration(declaration)
    }
}

#[derive(Debug, Serialize)]
pub enum SnapshotAstNode<'ast> {
    Primitive {
        value: Primitive,
        location: Location,
        original: &'ast str,
    },
    String {
        value: &'ast str,
        location: Location,
    },
    Component {
        name: Box<SnapshotAstNode<'ast>>,
        location: Location,
        attributes: Vec<SnapshotAstNode<'ast>>,
        original: &'ast str,
    },
    ComponentSlot {
        name: Box<SnapshotAstNode<'ast>>,
        location: Location,
        original: &'ast str,
    },
    Text {
        values: Vec<SnapshotAstNode<'ast>>,
        attributes: Vec<SnapshotAstNode<'ast>>,
        children: Vec<SnapshotAstNode<'ast>>,
        text: &'ast str,
        location: Location,
        keyword: Location,
    },
    Span {
        values: Vec<SnapshotAstNode<'ast>>,
        attributes: Vec<SnapshotAstNode<'ast>>,
        value: &'ast str,
        location: Location,
        keyword: Location,
    },
    Container {
        kind: Container,
        children: Vec<SnapshotAstNode<'ast>>,
        location: Location,
        attributes: Vec<SnapshotAstNode<'ast>>,
        original: &'ast str,
        keyword: Location,
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
    Declaration(SnapshotDeclaration<'ast>),
    Error {
        token: aml_token::TokenKind,
        location: Location,
    },
    For {
        binding: Box<SnapshotAstNode<'ast>>,
        value: SnapshotExpr<'ast>,
        children: Vec<SnapshotAstNode<'ast>>,
        location: Location,
        keyword: Location,
        original: &'ast str,
    },
}

impl<'ast> SnapshotAstNode<'ast> {
    pub fn from_node(node: AstNode, content: &'ast str) -> Self {
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
                keyword,
            } => Self::Text {
                keyword,
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
                keyword,
            } => Self::Span {
                keyword,
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
            AstNode::Container {
                children,
                location,
                attributes,
                kind,
                keyword,
            } => Self::Container {
                keyword,
                kind,
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
            AstNode::Declaration(declaration) => {
                SnapshotDeclaration::from_declaration(declaration, content).into()
            }
            AstNode::Error { location, token } => Self::Error { location, token },
            AstNode::Component {
                name,
                attributes,
                location,
            } => Self::Component {
                location,
                original: &content[location.to_range()],
                name: Box::new(SnapshotAstNode::from_node(*name, content)),
                attributes: attributes
                    .attributes
                    .into_iter()
                    .map(|n| SnapshotAstNode::from_node(n, content))
                    .collect(),
            },
            AstNode::ComponentSlot { name, location } => Self::ComponentSlot {
                location,
                original: &content[location.to_range()],
                name: Box::new(SnapshotAstNode::from_node(*name, content)),
            },
            AstNode::For {
                binding,
                value,
                children,
                location,
                keyword,
            } => Self::For {
                location,
                original: &content[location.to_range()],
                binding: Box::new(SnapshotAstNode::from_node(*binding, content)),
                value: SnapshotExpr::from_expr(value, content),
                children: children
                    .into_iter()
                    .map(|n| SnapshotAstNode::from_node(n, content))
                    .collect(),
                keyword,
            },
        }
    }
}