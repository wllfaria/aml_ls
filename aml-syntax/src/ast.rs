use std::collections::HashMap;

use aml_core::Location;
use aml_token::{Primitive, TokenKind};
use serde::Serialize;

#[derive(Debug, Default)]
pub struct Ast {
    pub nodes: Vec<AstNode>,
    pub variables: HashMap<String, Location>,
    pub scopes: Vec<Scope>,
}

#[derive(Debug, Default)]
pub struct Attributes {
    pub attributes: Vec<AstNode>,
    pub location: Option<Location>,
}

#[derive(Debug)]
pub enum AstNode {
    String {
        location: Location,
    },
    Primitive {
        location: Location,
        value: Primitive,
    },
    Text {
        values: Vec<AstNode>,
        attributes: Attributes,
        children: Vec<AstNode>,
        location: Location,
    },
    Span {
        values: Vec<AstNode>,
        attributes: Attributes,
        location: Location,
    },
    VStack {
        children: Vec<AstNode>,
        attributes: Attributes,
        location: Location,
    },
    HStack {
        children: Vec<AstNode>,
        attributes: Attributes,
        location: Location,
    },
    Identifier {
        location: Location,
    },
    Attribute {
        name: Box<AstNode>,
        value: Expr,
        location: Location,
    },
    Declaration {
        name: Box<AstNode>,
        value: Expr,
        location: Location,
    },
    Error {
        token: TokenKind,
        location: Location,
    },
}

impl AstNode {
    pub fn location(&self) -> Location {
        match self {
            AstNode::String { location } => *location,
            AstNode::Primitive { location, .. } => *location,
            AstNode::Text { location, .. } => *location,
            AstNode::Span { location, .. } => *location,
            AstNode::VStack { location, .. } => *location,
            AstNode::HStack { location, .. } => *location,
            AstNode::Identifier { location } => *location,
            AstNode::Attribute { location, .. } => *location,
            AstNode::Declaration { location, .. } => *location,
            AstNode::Error { location, .. } => *location,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct Scope {
    pub variables: Vec<String>,
    pub parent: Option<usize>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Expr {
    Unary {
        op: aml_token::Operator,
        expr: Box<Expr>,
        location: Location,
    },
    Binary {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: aml_token::Operator,
        location: Location,
    },
    Ident {
        location: Location,
    },
    String {
        location: Location,
    },
    Call {
        fun: Box<Expr>,
        args: Vec<Expr>,
        location: Location,
    },
    Primitive {
        value: aml_token::Primitive,
        location: Location,
    },
    ArrayIndex {
        lhs: Box<Expr>,
        index: Box<Expr>,
        location: Location,
    },
    List {
        items: Vec<Expr>,
        location: Location,
    },
    Map {
        location: Location,
        items: Vec<(Expr, Expr)>,
    },
    Error {
        token: TokenKind,
        location: Location,
    },
}

impl Expr {
    pub fn location(&self) -> Location {
        match self {
            Expr::Unary { location, .. } => *location,
            Expr::Binary { location, .. } => *location,
            Expr::Ident { location } => *location,
            Expr::String { location } => *location,
            Expr::Primitive { location, .. } => *location,
            Expr::ArrayIndex { location, .. } => *location,
            Expr::List { location, .. } => *location,
            Expr::Error { location, .. } => *location,
            Expr::Map { location, .. } => *location,
            Expr::Call { location, .. } => *location,
        }
    }
}
