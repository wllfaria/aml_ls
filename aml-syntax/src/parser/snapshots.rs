use std::collections::HashMap;

use aml_core::Location;
use aml_token::{Container, Primitive, TokenKind};
use serde::Serialize;

use crate::ast::*;
use crate::expressions::snapshots::SnapshotExpr;

pub trait ToSnapshot<'ast> {
    type Item;
    fn into_snapshot(self, content: &'ast str) -> Self::Item;
}

impl<'ast> ToSnapshot<'ast> for AstNode {
    type Item = SnapshotAstNode<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        match self {
            AstNode::Component(component) => component.into_snapshot(content),
            AstNode::ComponentSlot(component_slot) => component_slot.into_snapshot(content),
            AstNode::Primitive(primitive_node) => primitive_node.into_snapshot(content),
            AstNode::Container(container_node) => container_node.into_snapshot(content),
            AstNode::Text(text) => text.into_snapshot(content),
            AstNode::Span(span) => span.into_snapshot(content),
            AstNode::Attribute(attribute) => attribute.into_snapshot(content),
            AstNode::Declaration(declaration) => declaration.into_snapshot(content),
            AstNode::For(for_loop) => for_loop.into_snapshot(content),
            AstNode::Error(error_node) => error_node.into_snapshot(content),
            AstNode::String(location) => SnapshotAstNode::String(SnapshotString {
                location,
                value: &content[location.to_range()],
            }),
            AstNode::Identifier(location) => SnapshotAstNode::Identifier(SnapshotIdentifier {
                location,
                value: &content[location.to_range()],
            }),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotString<'ast> {
    pub value: &'ast str,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct SnapshotComponent<'ast> {
    pub name: Box<SnapshotAstNode<'ast>>,
    pub location: Location,
    pub attributes: Vec<SnapshotAstNode<'ast>>,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for Component {
    type Item = SnapshotAstNode<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotAstNode::Component(SnapshotComponent {
            location: self.location,
            original: &content[self.location.to_range()],
            name: Box::new(self.name.into_snapshot(content)),
            attributes: self
                .attributes
                .items
                .into_iter()
                .map(|n| n.into_snapshot(content))
                .collect(),
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotComponentSlot<'ast> {
    pub name: Box<SnapshotAstNode<'ast>>,
    pub location: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for ComponentSlot {
    type Item = SnapshotAstNode<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotAstNode::ComponentSlot(SnapshotComponentSlot {
            location: self.location,
            original: &content[self.location.to_range()],
            name: Box::new(self.name.into_snapshot(content)),
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotPrimitive<'ast> {
    pub value: Primitive,
    pub location: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for PrimitiveNode {
    type Item = SnapshotAstNode<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotAstNode::Primitive(SnapshotPrimitive {
            location: self.location,
            original: &content[self.location.to_range()],
            value: self.value,
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotContainer<'ast> {
    pub kind: Container,
    pub children: Vec<SnapshotAstNode<'ast>>,
    pub location: Location,
    pub attributes: Vec<SnapshotAstNode<'ast>>,
    pub original: &'ast str,
    pub keyword: Location,
}

impl<'ast> ToSnapshot<'ast> for ContainerNode {
    type Item = SnapshotAstNode<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotAstNode::Container(SnapshotContainer {
            kind: self.kind,
            keyword: self.keyword,
            location: self.location,
            children: self
                .children
                .into_iter()
                .map(|n| n.into_snapshot(content))
                .collect(),
            attributes: self
                .attributes
                .items
                .into_iter()
                .map(|n| n.into_snapshot(content))
                .collect(),
            original: &content[self.location.to_range()],
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotText<'ast> {
    pub values: Vec<SnapshotAstNode<'ast>>,
    pub attributes: Vec<SnapshotAstNode<'ast>>,
    pub children: Vec<SnapshotAstNode<'ast>>,
    pub text: &'ast str,
    pub location: Location,
    pub keyword: Location,
}

impl<'ast> ToSnapshot<'ast> for Text {
    type Item = SnapshotAstNode<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotAstNode::Text(SnapshotText {
            keyword: self.keyword,
            location: self.location,
            text: &content[self.location.to_range()],
            values: self
                .values
                .into_iter()
                .map(|n| n.into_snapshot(content))
                .collect(),
            attributes: self
                .attributes
                .items
                .into_iter()
                .map(|n| n.into_snapshot(content))
                .collect(),
            children: self
                .children
                .into_iter()
                .map(|n| n.into_snapshot(content))
                .collect(),
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotSpan<'ast> {
    pub values: Vec<SnapshotAstNode<'ast>>,
    pub attributes: Vec<SnapshotAstNode<'ast>>,
    pub value: &'ast str,
    pub location: Location,
    pub keyword: Location,
}

impl<'ast> ToSnapshot<'ast> for Span {
    type Item = SnapshotAstNode<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotAstNode::Span(SnapshotSpan {
            keyword: self.keyword,
            location: self.location,
            value: &content[self.location.to_range()],
            values: self
                .values
                .into_iter()
                .map(|n| n.into_snapshot(content))
                .collect(),
            attributes: self
                .attributes
                .items
                .into_iter()
                .map(|n| n.into_snapshot(content))
                .collect(),
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotIdentifier<'ast> {
    pub value: &'ast str,
    pub location: Location,
}

#[derive(Debug, Serialize)]
pub struct SnapshotAttribute<'ast> {
    pub name: Box<SnapshotAstNode<'ast>>,
    pub value: SnapshotExpr<'ast>,
    pub location: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for Attribute {
    type Item = SnapshotAstNode<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotAstNode::Attribute(SnapshotAttribute {
            location: self.location,
            value: self.value.into_snapshot(content),
            original: &content[self.location.to_range()],
            name: Box::new(self.name.into_snapshot(content)),
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotDeclaration<'ast> {
    kind: DeclarationKind,
    name: Box<SnapshotAstNode<'ast>>,
    value: SnapshotExpr<'ast>,
    location: Location,
}

impl<'ast> ToSnapshot<'ast> for Declaration {
    type Item = SnapshotAstNode<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotAstNode::Declaration(SnapshotDeclaration {
            kind: self.kind,
            location: self.location,
            value: self.value.into_snapshot(content),
            name: Box::new(self.name.into_snapshot(content)),
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotFor<'ast> {
    pub binding: Box<SnapshotAstNode<'ast>>,
    pub value: SnapshotExpr<'ast>,
    pub children: Vec<SnapshotAstNode<'ast>>,
    pub location: Location,
    pub keyword: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for For {
    type Item = SnapshotAstNode<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotAstNode::For(SnapshotFor {
            keyword: self.keyword,
            location: self.location,
            value: self.value.into_snapshot(content),
            original: &content[self.location.to_range()],
            binding: Box::new(self.binding.into_snapshot(content)),
            children: self
                .children
                .into_iter()
                .map(|n| n.into_snapshot(content))
                .collect(),
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotError<'ast> {
    pub token: TokenKind,
    pub location: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for ErrorNode {
    type Item = SnapshotAstNode<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotAstNode::Error(SnapshotError {
            token: self.token,
            location: self.location,
            original: &content[self.location.to_range()],
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotAst<'ast> {
    pub nodes: Vec<SnapshotAstNode<'ast>>,
    pub variables: HashMap<String, Location>,
    pub scopes: Vec<Scope>,
}

impl<'ast> SnapshotAst<'ast> {
    pub fn from_ast(ast: Ast, content: &'ast str) -> Self {
        Self {
            scopes: ast.scopes,
            variables: ast.variables,
            nodes: ast
                .nodes
                .into_iter()
                .map(|n| n.into_snapshot(content))
                .collect(),
        }
    }
}

#[derive(Debug, Serialize)]
pub enum SnapshotAstNode<'ast> {
    String(SnapshotString<'ast>),
    Component(SnapshotComponent<'ast>),
    ComponentSlot(SnapshotComponentSlot<'ast>),
    Primitive(SnapshotPrimitive<'ast>),
    Container(SnapshotContainer<'ast>),
    Text(SnapshotText<'ast>),
    Span(SnapshotSpan<'ast>),
    Identifier(SnapshotIdentifier<'ast>),
    Attribute(SnapshotAttribute<'ast>),
    Declaration(SnapshotDeclaration<'ast>),
    For(SnapshotFor<'ast>),
    Error(SnapshotError<'ast>),
}
