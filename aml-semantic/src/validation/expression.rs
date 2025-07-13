use aml_core::Location;
use aml_syntax::ast::*;
use aml_token::{Operator, Primitive};

use crate::analysis::node_analyzer::AnalysisCtx;
use crate::diagnostics::Diagnostics;
use crate::symbol_table::{SymbolType, ValueType};

#[derive(Debug)]
pub struct ExpressionAnalyzer<'src> {
    content: &'src str,
}

impl<'src> ExpressionAnalyzer<'src> {
    pub fn new(content: &'src str) -> Self {
        Self { content }
    }

    pub fn analyze_expression(&self, expr: &Expr, ctx: &mut AnalysisCtx<'_>) -> ValueType {
        if let Some(errors) = expr.errors() {
            // TODO(wiru): we need different kind of errors, not just "unexpected token"
            for error in errors.iter() {
                ctx.diagnostics.error(
                    error.location,
                    format!("unexpected token '{:?}'", error.token),
                );
            }
        }

        match expr {
            Expr::String(_) => ValueType::String,
            Expr::Ident(location) => self.resolve_identifier_type(*location, ctx),
            Expr::Primitive(primitive) => match primitive.value {
                Primitive::Bool(_) => ValueType::Boolean,
                Primitive::Int(_) => ValueType::Int,
                Primitive::Float(_) => ValueType::Float,
                Primitive::Hex(_) => ValueType::Hex,
            },
            Expr::List(list) => ValueType::List(
                list.items
                    .iter()
                    .map(|expr| self.analyze_expression(expr, ctx))
                    .collect(),
            ),
            Expr::Map(map) => {
                let entry_types = map
                    .items
                    .iter()
                    .map(|(key, val)| {
                        (
                            self.analyze_expression(key, ctx),
                            self.analyze_expression(val, ctx),
                        )
                    })
                    .collect();

                ValueType::Map(entry_types)
            }
            Expr::Binary(binary) => {
                let lhs_type = self.analyze_expression(&binary.lhs, ctx);
                let rhs_type = self.analyze_expression(&binary.rhs, ctx);
                let expected_type = self.get_operator_result_type(binary.op);
                self.validate_operand_types(
                    &lhs_type,
                    &rhs_type,
                    binary.lhs.location(),
                    binary.rhs.location(),
                    &expected_type,
                    ctx.diagnostics,
                );
                expected_type
            }
            Expr::Unary(unary) => {
                let expr_type = self.analyze_expression(&unary.expr, ctx);

                match (unary.op, expr_type) {
                    (Operator::Not, ValueType::Boolean) => ValueType::Boolean,
                    (Operator::Minus, ValueType::Int) => ValueType::Int,
                    (Operator::Minus, ValueType::Float) => ValueType::Float,
                    (op, expr) => {
                        ctx.diagnostics.error(
                            unary.location,
                            format!("invalid operand type {expr} for operator '{op:?}'"),
                        );
                        ValueType::Unknown
                    }
                }
            }
            Expr::Call(call) => {
                for arg in call.args.iter() {
                    self.analyze_expression(arg, ctx);
                }
                ValueType::Unknown
            }
            Expr::ArrayIndex(array_index) => {
                let lhs_type = self.analyze_expression(&array_index.lhs, ctx);

                let ValueType::List(items) = lhs_type else {
                    ctx.diagnostics.error(
                        array_index.lhs.location(),
                        "array index must be applied to a list",
                    );
                    return ValueType::Unknown;
                };

                let loc = array_index.location;
                let val = match self.extract_integer_literal(expr) {
                    Some(v) if v < 0 => {
                        ctx.diagnostics.error(loc, "index must be non-negative");
                        return ValueType::Unknown;
                    }
                    Some(v) if (v as usize) >= items.len() => {
                        ctx.diagnostics.error(loc, "array index out of bounds");
                        return ValueType::Unknown;
                    }
                    Some(v) => v as usize,
                    None => {
                        ctx.diagnostics.error(loc, "array index must be an integer");
                        return ValueType::Unknown;
                    }
                };

                items[val].clone()
            }
            Expr::Error(error) => {
                ctx.diagnostics.error(
                    error.location,
                    format!("unexpected token '{:?}'", error.token),
                );
                ValueType::Unknown
            }
        }
    }

    fn resolve_identifier_type(&self, location: Location, ctx: &mut AnalysisCtx<'_>) -> ValueType {
        let name = &self.content[location.to_range()];

        if let Some(symbol) = ctx.symbol_table.lookup_symbol(name) {
            return match &symbol.symbol_type {
                SymbolType::Variable(value_type) => value_type.clone(),
                SymbolType::Element => ValueType::Unknown,
            };
        }

        if let Some(symbol) = ctx.global_scope.lookup_symbol(name) {
            return match &symbol.symbol_type {
                SymbolType::Variable(value_type) => value_type.clone(),
                SymbolType::Element => ValueType::Unknown,
            };
        }

        ctx.diagnostics.error(
            location,
            format!("reference to undefined identifier '{name}'"),
        );
        ValueType::Unknown
    }

    fn get_operator_result_type(&self, op: Operator) -> ValueType {
        match op {
            Operator::Plus | Operator::Minus | Operator::Mul | Operator::Div | Operator::Mod => {
                // TODO(wiru): can't really determine if its an int or float
                ValueType::Int
            }
            Operator::EqualEqual
            | Operator::NotEqual
            | Operator::GreaterThan
            | Operator::LessThan
            | Operator::GreaterThanOrEqual
            | Operator::LessThanOrEqual
            | Operator::And
            | Operator::Or => ValueType::Boolean,
            _ => ValueType::Unknown,
        }
    }

    fn validate_operand_types(
        &self,
        lhs_type: &ValueType,
        rhs_type: &ValueType,
        lhs_location: Location,
        rhs_location: Location,
        expected_type: &ValueType,
        diagnostics: &mut Diagnostics,
    ) {
        if lhs_type != expected_type {
            diagnostics.error(
                lhs_location,
                format!("Mismatched types. Expected `{expected_type}` found `{lhs_type}`"),
            );
        }
        if rhs_type != expected_type {
            diagnostics.error(
                rhs_location,
                format!("Mismatched types. Expected `{expected_type}` found `{rhs_type}`"),
            );
        }
    }

    pub fn extract_integer_literal(&self, expr: &Expr) -> Option<i64> {
        match expr {
            Expr::Primitive(primitive) => match primitive.value {
                Primitive::Int(i) => Some(i),
                _ => None,
            },
            _ => None,
        }
    }
}
