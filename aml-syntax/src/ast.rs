use std::collections::HashMap;

use aml_core::Location;
use aml_token::{Container, Operator, Primitive, TokenKind};
use serde::Serialize;

#[derive(Debug, Serialize)]
pub struct Scope {
    pub variables: Vec<String>,
    pub parent: Option<usize>,
}

#[derive(Debug, Default)]
pub struct Ast {
    pub nodes: Vec<AstNode>,
    pub variables: HashMap<String, Location>,
    pub scopes: Vec<Scope>,
}

#[derive(Debug, Default)]
pub struct Attributes {
    pub items: Vec<AstNode>,
    pub location: Option<Location>,
}

#[derive(Debug, Serialize)]
pub enum DeclarationKind {
    Local,
    Global,
}

#[derive(Debug)]
pub struct Declaration {
    pub kind: DeclarationKind,
    pub name: Box<AstNode>,
    pub value: Expr,
    pub location: Location,
}

impl Declaration {
    pub fn is_global(&self) -> bool {
        matches!(self.kind, DeclarationKind::Global)
    }

    pub fn is_local(&self) -> bool {
        !self.is_global()
    }
}

#[derive(Debug)]
pub struct ErrorNode {
    pub token: TokenKind,
    pub location: Location,
}

#[derive(Debug)]
pub struct Component {
    pub name: Box<AstNode>,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Debug)]
pub struct ComponentSlot {
    pub name: Box<AstNode>,
    pub location: Location,
}

#[derive(Debug)]
pub struct PrimitiveNode {
    pub value: Primitive,
    pub location: Location,
}

#[derive(Debug)]
pub struct ContainerNode {
    pub kind: Container,
    pub children: Vec<AstNode>,
    pub attributes: Attributes,
    pub location: Location,
    pub keyword: Location,
}

#[derive(Debug)]
pub struct Text {
    pub values: Vec<AstNode>,
    pub attributes: Attributes,
    pub children: Vec<AstNode>,
    pub location: Location,
    pub keyword: Location,
}

#[derive(Debug)]
pub struct For {
    pub binding: Box<AstNode>,
    pub value: Expr,
    pub children: Vec<AstNode>,
    pub location: Location,
    pub keyword: Location,
}

#[derive(Debug)]
pub struct Span {
    pub values: Vec<AstNode>,
    pub attributes: Attributes,
    pub location: Location,
    pub keyword: Location,
}

#[derive(Debug)]
pub struct Attribute {
    pub name: Box<AstNode>,
    pub value: Expr,
    pub location: Location,
}

#[derive(Debug)]
pub enum AstNode {
    String(Location),
    Component(Component),
    ComponentSlot(ComponentSlot),
    Primitive(PrimitiveNode),
    Container(ContainerNode),
    Text(Text),
    Span(Span),
    Identifier(Location),
    Attribute(Attribute),
    Declaration(Declaration),
    For(For),
    Error(ErrorNode),
}

impl AstNode {
    pub fn location(&self) -> Location {
        match self {
            AstNode::String(location) => *location,
            AstNode::Primitive(primitive) => primitive.location,
            AstNode::Text(text) => text.location,
            AstNode::Span(span) => span.location,
            AstNode::Container(container) => container.location,
            AstNode::Identifier(location) => *location,
            AstNode::For(for_node) => for_node.location,
            AstNode::Attribute(attribute) => attribute.location,
            AstNode::Declaration(declaration) => declaration.location,
            AstNode::Error(error) => error.location,
            AstNode::Component(component) => component.location,
            AstNode::ComponentSlot(slot) => slot.location,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct Unary {
    pub op: Operator,
    pub expr: Box<Expr>,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: Operator,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct Call {
    pub fun: Box<Expr>,
    pub args: Vec<Expr>,
    pub location: Location,
}

impl Call {
    pub fn has_error(&self) -> bool {
        self.fun.has_error() || self.args.iter().any(|arg| arg.has_error())
    }
}

#[derive(Debug, Serialize)]
pub struct PrimitiveExpr {
    pub value: Primitive,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct ArrayIndex {
    pub lhs: Box<Expr>,
    pub index: Box<Expr>,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct List {
    pub items: Vec<Expr>,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct Map {
    pub location: Location,
    pub items: Vec<(Expr, Expr)>,
}

impl Map {
    pub fn has_error(&self) -> bool {
        self.items
            .iter()
            .any(|(key, value)| key.has_error() || value.has_error())
    }
}

#[derive(Debug, Serialize)]
pub struct ErrorExpr {
    pub token: TokenKind,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub enum Expr {
    Unary(Unary),
    Binary(Binary),
    Ident(Location),
    String(Location),
    Call(Call),
    Primitive(PrimitiveExpr),
    ArrayIndex(ArrayIndex),
    List(List),
    Map(Map),
    Error(ErrorExpr),
}

impl Expr {
    pub fn location(&self) -> Location {
        match self {
            Expr::Unary(unary) => unary.location,
            Expr::Binary(binary) => binary.location,
            Expr::Ident(location) => *location,
            Expr::String(location) => *location,
            Expr::Primitive(primitive) => primitive.location,
            Expr::ArrayIndex(array_index) => array_index.location,
            Expr::List(list) => list.location,
            Expr::Map(map) => map.location,
            Expr::Error(error) => error.location,
            Expr::Call(call) => call.location,
        }
    }

    pub fn has_error(&self) -> bool {
        match self {
            Expr::Error(_) => true,
            Expr::Ident(_) => false,
            Expr::Unary(unary) => unary.expr.has_error(),
            Expr::Binary(binary) => binary.lhs.has_error() || binary.rhs.has_error(),
            Expr::String(_) => false,
            Expr::Call(call) => call.has_error(),
            Expr::Primitive(_) => false,
            Expr::ArrayIndex(index) => index.lhs.has_error() || index.index.has_error(),
            Expr::List(list) => list.items.iter().any(|item| item.has_error()),
            Expr::Map(map) => map.has_error(),
        }
    }
}
