use aml_core::Location;
use aml_token::{Operator, Primitive, TokenKind};
use serde::Serialize;

use crate::ast::Expr;

#[derive(Debug, Serialize)]
pub enum SnapshotExpr<'ast> {
    Unary {
        original: &'ast str,
        op: Operator,
        expr: Box<SnapshotExpr<'ast>>,
        location: Location,
    },
    Binary {
        original: &'ast str,
        lhs: Box<SnapshotExpr<'ast>>,
        rhs: Box<SnapshotExpr<'ast>>,
        op: Operator,
        location: Location,
    },
    Ident {
        value: &'ast str,
        location: Location,
    },
    String {
        value: &'ast str,
        location: Location,
    },
    Call {
        original: &'ast str,
        fun: Box<SnapshotExpr<'ast>>,
        args: Vec<SnapshotExpr<'ast>>,
        location: Location,
    },
    Primitive {
        original: &'ast str,
        value: Primitive,
        location: Location,
    },
    ArrayIndex {
        original: &'ast str,
        lhs: Box<SnapshotExpr<'ast>>,
        index: Box<SnapshotExpr<'ast>>,
        location: Location,
    },
    List {
        original: &'ast str,
        items: Vec<SnapshotExpr<'ast>>,
        location: Location,
    },
    Map {
        original: &'ast str,
        items: Vec<(SnapshotExpr<'ast>, SnapshotExpr<'ast>)>,
        location: Location,
    },
    Error {
        original: &'ast str,
        location: Location,
        token: TokenKind,
    },
}

impl<'ast> SnapshotExpr<'ast> {
    pub fn from_expr(expr: Expr, content: &'ast str) -> Self {
        match expr {
            Expr::Unary { op, expr, location } => Self::Unary {
                op,
                location,
                original: &content[location.to_range()],
                expr: Box::new(SnapshotExpr::from_expr(*expr, content)),
            },
            Expr::Binary {
                lhs,
                rhs,
                op,
                location,
            } => Self::Binary {
                op,
                location,
                original: &content[location.to_range()],
                lhs: Box::new(SnapshotExpr::from_expr(*lhs, content)),
                rhs: Box::new(SnapshotExpr::from_expr(*rhs, content)),
            },
            Expr::Ident { location } => Self::Ident {
                value: &content[location.to_range()],
                location,
            },
            Expr::String { location } => Self::String {
                value: &content[location.to_range()],
                location,
            },
            Expr::Call {
                fun,
                args,
                location,
            } => Self::Call {
                fun: Box::new(SnapshotExpr::from_expr(*fun, content)),
                args: args
                    .into_iter()
                    .map(|e| SnapshotExpr::from_expr(e, content))
                    .collect(),
                location,
                original: &content[location.to_range()],
            },
            Expr::Primitive { value, location } => Self::Primitive {
                value,
                location,
                original: &content[location.to_range()],
            },
            Expr::ArrayIndex {
                lhs,
                index,
                location,
            } => Self::ArrayIndex {
                location,
                original: &content[location.to_range()],
                lhs: Box::new(SnapshotExpr::from_expr(*lhs, content)),
                index: Box::new(SnapshotExpr::from_expr(*index, content)),
            },
            Expr::List { items, location } => Self::List {
                location,
                original: &content[location.to_range()],
                items: items
                    .into_iter()
                    .map(|e| SnapshotExpr::from_expr(e, content))
                    .collect(),
            },
            Expr::Map { items, location } => Self::Map {
                location,
                original: &content[location.to_range()],
                items: items
                    .into_iter()
                    .map(|(l, r)| {
                        (
                            SnapshotExpr::from_expr(l, content),
                            SnapshotExpr::from_expr(r, content),
                        )
                    })
                    .collect(),
            },
            Expr::Error { location, token } => Self::Error {
                token,
                location,
                original: &content[location.to_range()],
            },
        }
    }
}
