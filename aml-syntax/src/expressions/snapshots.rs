use aml_core::Location;
use aml_token::{Operator, Primitive, TokenKind};
use serde::Serialize;

use crate::ast::*;
use crate::parser::snapshots::ToSnapshot;

impl<'ast> ToSnapshot<'ast> for Expr {
    type Item = SnapshotExpr<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        match self {
            Expr::Ident(location) => SnapshotExpr::Ident(SnapshotIdentifier {
                location,
                value: &content[location.to_range()],
            }),
            Expr::String(location) => SnapshotExpr::String(SnapshotString {
                location,
                value: &content[location.to_range()],
            }),
            Expr::Unary(unary) => unary.into_snapshot(content),
            Expr::Binary(binary) => binary.into_snapshot(content),
            Expr::Call(call) => call.into_snapshot(content),
            Expr::Primitive(primitive) => primitive.into_snapshot(content),
            Expr::ArrayIndex(array_index) => array_index.into_snapshot(content),
            Expr::List(list) => list.into_snapshot(content),
            Expr::Map(map) => map.into_snapshot(content),
            Expr::Error(error) => error.into_snapshot(content),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotUnary<'ast> {
    pub original: &'ast str,
    pub op: Operator,
    pub expr: Box<SnapshotExpr<'ast>>,
    pub location: Location,
}

impl<'ast> ToSnapshot<'ast> for Unary {
    type Item = SnapshotExpr<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotExpr::Unary(SnapshotUnary {
            op: self.op,
            expr: Box::new(self.expr.into_snapshot(content)),
            location: self.location,
            original: &content[self.location.to_range()],
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotBinary<'ast> {
    pub lhs: Box<SnapshotExpr<'ast>>,
    pub rhs: Box<SnapshotExpr<'ast>>,
    pub op: Operator,
    pub location: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for Binary {
    type Item = SnapshotExpr<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotExpr::Binary(SnapshotBinary {
            lhs: Box::new(self.lhs.into_snapshot(content)),
            rhs: Box::new(self.rhs.into_snapshot(content)),
            op: self.op,
            location: self.location,
            original: &content[self.location.to_range()],
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotIdentifier<'ast> {
    pub value: &'ast str,
    pub location: Location,
}

impl<'ast> ToSnapshot<'ast> for SnapshotIdentifier<'ast> {
    type Item = SnapshotExpr<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotExpr::Ident(SnapshotIdentifier {
            location: self.location,
            value: &content[self.location.to_range()],
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotString<'ast> {
    pub value: &'ast str,
    pub location: Location,
}

impl<'ast> ToSnapshot<'ast> for SnapshotString<'ast> {
    type Item = SnapshotExpr<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotExpr::String(SnapshotString {
            location: self.location,
            value: &content[self.location.to_range()],
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotCall<'ast> {
    pub fun: Box<SnapshotExpr<'ast>>,
    pub args: Vec<SnapshotExpr<'ast>>,
    pub location: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for Call {
    type Item = SnapshotExpr<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotExpr::Call(SnapshotCall {
            fun: Box::new(self.fun.into_snapshot(content)),
            args: self
                .args
                .into_iter()
                .map(|e| e.into_snapshot(content))
                .collect(),
            location: self.location,
            original: &content[self.location.to_range()],
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotPrimitive<'ast> {
    pub value: Primitive,
    pub location: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for PrimitiveExpr {
    type Item = SnapshotExpr<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotExpr::Primitive(SnapshotPrimitive {
            value: self.value,
            location: self.location,
            original: &content[self.location.to_range()],
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotArrayIndex<'ast> {
    pub lhs: Box<SnapshotExpr<'ast>>,
    pub index: Box<SnapshotExpr<'ast>>,
    pub location: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for ArrayIndex {
    type Item = SnapshotExpr<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotExpr::ArrayIndex(SnapshotArrayIndex {
            location: self.location,
            original: &content[self.location.to_range()],
            lhs: Box::new(self.lhs.into_snapshot(content)),
            index: Box::new(self.index.into_snapshot(content)),
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotList<'ast> {
    pub items: Vec<SnapshotExpr<'ast>>,
    pub location: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for List {
    type Item = SnapshotExpr<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotExpr::List(SnapshotList {
            location: self.location,
            original: &content[self.location.to_range()],
            items: self
                .items
                .into_iter()
                .map(|e| e.into_snapshot(content))
                .collect(),
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotMap<'ast> {
    pub items: Vec<(SnapshotExpr<'ast>, SnapshotExpr<'ast>)>,
    pub location: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for Map {
    type Item = SnapshotExpr<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotExpr::Map(SnapshotMap {
            location: self.location,
            original: &content[self.location.to_range()],
            items: self
                .items
                .into_iter()
                .map(|(l, r)| (l.into_snapshot(content), r.into_snapshot(content)))
                .collect(),
        })
    }
}

#[derive(Debug, Serialize)]
pub struct SnapshotErrorExpr<'ast> {
    pub token: TokenKind,
    pub location: Location,
    pub original: &'ast str,
}

impl<'ast> ToSnapshot<'ast> for ErrorExpr {
    type Item = SnapshotExpr<'ast>;

    fn into_snapshot(self, content: &'ast str) -> Self::Item {
        SnapshotExpr::Error(SnapshotErrorExpr {
            token: self.token,
            location: self.location,
            original: &content[self.location.to_range()],
        })
    }
}

#[derive(Debug, Serialize)]
pub enum SnapshotExpr<'ast> {
    Unary(SnapshotUnary<'ast>),
    Binary(SnapshotBinary<'ast>),
    Ident(SnapshotIdentifier<'ast>),
    String(SnapshotString<'ast>),
    Call(SnapshotCall<'ast>),
    Primitive(SnapshotPrimitive<'ast>),
    ArrayIndex(SnapshotArrayIndex<'ast>),
    List(SnapshotList<'ast>),
    Map(SnapshotMap<'ast>),
    Error(SnapshotErrorExpr<'ast>),
}
