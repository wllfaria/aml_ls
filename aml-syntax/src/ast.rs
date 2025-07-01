use std::collections::HashMap;
use serde::Serialize;
use aml_core::Location;

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

#[derive(Debug, Clone, Serialize)]
pub enum Expr {
    Unary {
        op: aml_token::Operator,
        expr: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: aml_token::Operator,
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
    },
    Primitive(aml_token::Primitive),
    ArrayIndex {
        lhs: Box<Expr>,
        index: Box<Expr>,
    },
    List(Vec<Expr>),
    Map {
        items: Vec<(Expr, Expr)>,
    },
}