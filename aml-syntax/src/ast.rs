use std::collections::HashMap;

use aml_core::Location;
use aml_token::{Container, Operator, Primitive, TokenKind};
use serde::Serialize;

#[derive(Debug, Serialize)]
pub struct Scope {
    pub variables: Vec<String>,
    pub parent: Option<usize>,
}

pub trait AstVisitor<'ast> {
    fn visit_globals(&mut self, _decl: &'ast Declaration, _node: &'ast AstNode) {}
    fn visit_locals(&mut self, _decl: &'ast Declaration, _node: &'ast AstNode) {}
    fn visit_string(&mut self, _location: Location, _node: &'ast AstNode) {}
    fn visit_component(&mut self, _component: &'ast Component, _node: &'ast AstNode) {}
    fn visit_container(&mut self, _container: &'ast ContainerNode, _node: &'ast AstNode) {}
    fn visit_text(&mut self, _text: &'ast Text, _node: &'ast AstNode) {}
    fn visit_for(&mut self, _for_loop: &'ast For, _node: &'ast AstNode) {}
    fn visit_component_slot(&mut self, _slot: &'ast ComponentSlot, _node: &'ast AstNode) {}
    fn visit_primitive(&mut self, _prim: &'ast PrimitiveNode, _node: &'ast AstNode) {}
    fn visit_span(&mut self, _span: &'ast Span, _node: &'ast AstNode) {}
    fn visit_identifier(&mut self, _ident: Location, _node: &'ast AstNode) {}
    fn visit_attribute(&mut self, _attr: &'ast Attribute, _node: &'ast AstNode) {}
    fn visit_error(&mut self, _err: &'ast ErrorNode, _node: &'ast AstNode) {}
    fn visit_if(&mut self, _if_chain: &'ast IfChain, _node: &'ast AstNode) {}
}

#[derive(Debug, Default)]
pub struct Ast {
    pub nodes: Vec<AstNode>,
    pub variables: HashMap<String, Location>,
    pub scopes: Vec<Scope>,
}

impl Ast {
    pub fn accept<'ast, V>(&'ast self, visitor: &mut V)
    where
        V: AstVisitor<'ast>,
    {
        for node in self.nodes.iter() {
            node.accept(visitor);
        }
    }
}

#[derive(Debug, Default, PartialEq, PartialOrd)]
pub struct Attributes {
    pub items: Vec<AstNode>,
    pub location: Option<Location>,
}

#[derive(Debug, Serialize, PartialEq, PartialOrd)]
pub enum DeclarationKind {
    Local,
    Global,
}

#[derive(Debug, PartialEq, PartialOrd)]
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

#[derive(Debug, PartialEq, PartialOrd)]
pub struct ErrorNode {
    pub token: TokenKind,
    pub location: Location,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Component {
    pub name: Box<AstNode>,
    pub location: Location,
    pub attributes: Attributes,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct ComponentSlot {
    pub name: Box<AstNode>,
    pub location: Location,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct PrimitiveNode {
    pub value: Primitive,
    pub location: Location,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct ContainerNode {
    pub kind: Container,
    pub children: Vec<AstNode>,
    pub attributes: Attributes,
    pub location: Location,
    pub keyword: Location,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Text {
    pub values: Vec<AstNode>,
    pub attributes: Attributes,
    pub children: Vec<AstNode>,
    pub location: Location,
    pub keyword: Location,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct For {
    pub binding: Box<AstNode>,
    pub value: Expr,
    pub children: Vec<AstNode>,
    pub location: Location,
    pub keyword: Location,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Span {
    pub values: Vec<AstNode>,
    pub attributes: Attributes,
    pub location: Location,
    pub keyword: Location,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Attribute {
    pub name: Box<AstNode>,
    pub value: Expr,
    pub location: Location,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct IfChain {
    pub branches: Vec<ConditionalBranch>,
    pub location: Location,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum ConditionalBranch {
    If(If),
    /// Location is the location of the `else` keyword, if keyword is encoded in the node
    ElseIf(Location, If),
    Else(Else),
}

impl ConditionalBranch {
    pub fn location(&self) -> Location {
        match self {
            ConditionalBranch::If(if_node) => if_node.location,
            ConditionalBranch::ElseIf(keyword, _) => *keyword,
            ConditionalBranch::Else(else_node) => else_node.location,
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct If {
    pub condition: Expr,
    pub then: Vec<AstNode>,
    pub keyword: Location,
    pub location: Location,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Else {
    pub children: Vec<AstNode>,
    pub location: Location,
    pub keyword: Location,
}

#[derive(Debug, PartialEq, PartialOrd)]
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
    If(IfChain),
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
            AstNode::If(if_chain) => if_chain.location,
        }
    }

    pub fn text<'src>(&self, content: &'src str) -> &'src str {
        match self {
            AstNode::String(location) => &content[location.to_range()],
            AstNode::Primitive(primitive) => &content[primitive.location.to_range()],
            AstNode::Component(component) => component.name.text(content),
            AstNode::ComponentSlot(slot) => slot.name.text(content),
            AstNode::Identifier(location) => &content[location.to_range()],
            AstNode::Declaration(declaration) => declaration.name.text(content),
            node => unreachable!("text for {node:?} cannot be retrieved from ast"),
        }
    }

    pub fn accept<'ast, V>(&'ast self, visitor: &mut V)
    where
        V: AstVisitor<'ast>,
    {
        match self {
            AstNode::String(location) => visitor.visit_string(*location, self),
            AstNode::Primitive(primitive) => visitor.visit_primitive(primitive, self),
            AstNode::Component(component) => visitor.visit_component(component, self),
            AstNode::ComponentSlot(slot) => visitor.visit_component_slot(slot, self),
            AstNode::Identifier(location) => visitor.visit_identifier(*location, self),
            AstNode::Declaration(decl) if decl.is_global() => visitor.visit_globals(decl, self),
            AstNode::Declaration(declaration) => visitor.visit_locals(declaration, self),
            AstNode::Attribute(attribute) => visitor.visit_attribute(attribute, self),
            AstNode::Error(error) => visitor.visit_error(error, self),
            AstNode::Container(container) => visitor.visit_container(container, self),
            AstNode::Text(text) => visitor.visit_text(text, self),
            AstNode::Span(span) => visitor.visit_span(span, self),
            AstNode::For(for_loop) => visitor.visit_for(for_loop, self),
            AstNode::If(if_chain) => visitor.visit_if(if_chain, self),
        }
    }
}

pub trait ExprVisitor<'ast> {
    fn visit_unary(&mut self, _unary: &'ast Unary, _expr: &'ast Expr) {}
    fn visit_binary(&mut self, _binary: &'ast Binary, _expr: &'ast Expr) {}
    fn visit_ident(&mut self, _ident: Location, _expr: &'ast Expr) {}
    fn visit_string(&mut self, _string: Location, _expr: &'ast Expr) {}
    fn visit_call(&mut self, _call: &'ast Call, _expr: &'ast Expr) {}
    fn visit_primitive(&mut self, _prim: &'ast PrimitiveExpr, _expr: &'ast Expr) {}
    fn visit_array_index(&mut self, _index: &'ast ArrayIndex, _expr: &'ast Expr) {}
    fn visit_list(&mut self, _list: &'ast List, _expr: &'ast Expr) {}
    fn visit_map(&mut self, _map: &'ast Map, _expr: &'ast Expr) {}
    fn visit_error(&mut self, _error: &'ast ErrorExpr, _expr: &'ast Expr) {}
}

#[derive(Debug, Serialize, PartialEq, PartialOrd)]
pub struct Unary {
    pub op: Operator,
    pub expr: Box<Expr>,
    pub location: Location,
}

#[derive(Debug, Serialize, PartialEq, PartialOrd)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: Operator,
    pub location: Location,
}

#[derive(Debug, Serialize, PartialEq, PartialOrd)]
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

#[derive(Debug, Serialize, PartialEq, PartialOrd)]
pub struct PrimitiveExpr {
    pub value: Primitive,
    pub location: Location,
}

#[derive(Debug, Serialize, PartialEq, PartialOrd)]
pub struct ArrayIndex {
    pub lhs: Box<Expr>,
    pub index: Box<Expr>,
    pub location: Location,
}

#[derive(Debug, Serialize, PartialEq, PartialOrd)]
pub struct List {
    pub items: Vec<Expr>,
    pub location: Location,
}

#[derive(Debug, Serialize, PartialEq, PartialOrd)]
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

#[derive(Debug, Serialize, PartialEq, PartialOrd)]
pub struct ErrorExpr {
    pub token: TokenKind,
    pub location: Location,
}

#[derive(Debug, Serialize, PartialEq, PartialOrd)]
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

    pub fn accept<'ast, V>(&'ast self, visitor: &mut V)
    where
        V: ExprVisitor<'ast>,
    {
        match self {
            Expr::Unary(unary) => visitor.visit_unary(unary, self),
            Expr::Binary(binary) => visitor.visit_binary(binary, self),
            Expr::Ident(ident) => visitor.visit_ident(*ident, self),
            Expr::String(string) => visitor.visit_string(*string, self),
            Expr::Call(call) => visitor.visit_call(call, self),
            Expr::Primitive(primitive) => visitor.visit_primitive(primitive, self),
            Expr::ArrayIndex(array_index) => visitor.visit_array_index(array_index, self),
            Expr::List(list) => visitor.visit_list(list, self),
            Expr::Map(map) => visitor.visit_map(map, self),
            Expr::Error(error) => visitor.visit_error(error, self),
        }
    }
}
