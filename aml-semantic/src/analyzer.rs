use std::path::PathBuf;

use aml_core::Location;
use aml_syntax::Ast;
use aml_syntax::ast::*;
use aml_token::{Operator, Primitive};

use crate::global_scope::{GlobalScope, GlobalSymbol};
use crate::symbol_table::{SymbolTable, SymbolType, ValueType};

#[derive(Debug)]
pub struct SemanticInfo {
    pub symbol_table: SymbolTable,
    pub diagnostics: Vec<SemanticDiagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SemanticDiagnostic {
    pub location: Location,
    pub message: String,
    pub severity: DiagnosticSeverity,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
}

#[derive(Debug)]
pub struct SemanticAnalyzer<'src> {
    symbol_table: SymbolTable,
    diagnostics: Vec<SemanticDiagnostic>,
    content: &'src str,
    global_scope: &'src mut GlobalScope,
}

impl<'src> SemanticAnalyzer<'src> {
    pub fn new(content: &'src str, global_scope: &'src mut GlobalScope) -> SemanticAnalyzer<'src> {
        SemanticAnalyzer {
            symbol_table: SymbolTable::new(),
            diagnostics: Vec::new(),
            content,
            global_scope,
        }
    }

    pub fn analyze(&mut self, ast: &Ast) -> SemanticInfo {
        for node in ast.nodes.iter() {
            self.analyze_node(node);
        }

        SemanticInfo {
            symbol_table: std::mem::take(&mut self.symbol_table),
            diagnostics: std::mem::take(&mut self.diagnostics),
        }
    }

    pub fn collect_globals(&'src mut self, ast: &'src Ast) {
        ast.accept(&mut GlobalCollector {
            file_path: PathBuf::new(),
            content: self.content,
            analyzer: self,
        });
    }

    fn declare_variable(&mut self, declaration: &Declaration) {
        let Some(name) = self.get_node_text(&declaration.name) else {
            self.add_diagnostic(
                declaration.name.location(),
                "invalid identifier name".into(),
                DiagnosticSeverity::Error,
            );
            return;
        };
        let value_type = self.analyze_expression(&declaration.value);
        let symbol_type = SymbolType::Variable(value_type);

        self.symbol_table
            .declare_symbol(name.into(), declaration.location, symbol_type);
    }

    fn analyze_node(&mut self, node: &AstNode) {
        match node {
            AstNode::Text(text) => self.analyze_text_element(
                &text.values,
                &text.attributes,
                &text.children,
                text.location,
            ),
            AstNode::Span(span) => self.analyze_span_element(&span.values, &span.attributes),
            AstNode::Container(container) => container
                .children
                .iter()
                .for_each(|child| self.analyze_node(child)),
            AstNode::Attribute(attribute) => _ = self.analyze_expression(&attribute.value),
            AstNode::Identifier { .. } => {}
            AstNode::String { .. } => {}
            AstNode::Primitive { .. } => {}
            AstNode::Error(error) => {
                self.add_diagnostic(
                    error.location,
                    format!("unexpected token '{:?}'", error.token),
                    DiagnosticSeverity::Error,
                );
            }

            // Local declarations are collected as they are defined, although they are also
            // hoisted to the root scope, they cannot be used before being declared
            AstNode::Declaration(declaration) if declaration.is_local() => {
                self.declare_variable(declaration);
            }
            // Global declarations are collected before analyzing as they can be used before
            // being defined.
            AstNode::Declaration { .. } => {}

            AstNode::Component { .. } => {}
            AstNode::ComponentSlot { .. } => {}
            AstNode::For { .. } => {}
            AstNode::If { .. } => {}
            AstNode::Switch { .. } => {}
            AstNode::With { .. } => {}
        }
    }

    fn analyze_text_element(
        &mut self,
        value: &[AstNode],
        attributes: &Attributes,
        children: &[AstNode],
        location: Location,
    ) {
        self.symbol_table.push_scope(None);
        attributes
            .items
            .iter()
            .for_each(|attr| self.analyze_node(attr));
        self.validate_text_element_value(value, location);
        children.iter().for_each(|child| self.analyze_node(child));
        self.symbol_table.pop_scope();
    }

    fn analyze_span_element(&mut self, values: &[AstNode], attributes: &Attributes) {
        attributes
            .items
            .iter()
            .for_each(|attr| self.analyze_node(attr));

        values.iter().for_each(|value| self.analyze_node(value));
    }

    fn validate_text_element_value(&mut self, values: &[AstNode], location: Location) {
        if values.is_empty() {
            self.add_diagnostic(
                location,
                "Text element has no value to display".into(),
                DiagnosticSeverity::Warning,
            )
        }

        for value in values {
            match value {
                AstNode::String { .. } => {}
                AstNode::Primitive { .. } => {}
                AstNode::Identifier { .. } => {
                    let name = self.get_node_text(value);
                    let Some(name) = name else {
                        self.add_diagnostic(
                            value.location(),
                            "invalid identifier name".into(),
                            DiagnosticSeverity::Error,
                        );
                        continue;
                    };

                    if self.symbol_table.lookup_symbol(name).is_some() {
                        continue;
                    }

                    if self.global_scope.lookup_symbol(name).is_some() {
                        continue;
                    }

                    self.add_diagnostic(
                        location,
                        format!("reference to undefined identifier '{name:?}'"),
                        DiagnosticSeverity::Error,
                    );
                }
                _ => self.add_diagnostic(
                    location,
                    "Text element value must be a string literal".into(),
                    DiagnosticSeverity::Error,
                ),
            }
        }
    }

    pub fn analyze_expression(&mut self, expr: &Expr) -> ValueType {
        match expr {
            Expr::String(_) => ValueType::String,
            Expr::Primitive(primitive) => match primitive.value {
                Primitive::Bool(_) => ValueType::Boolean,
                Primitive::Int(_) => ValueType::Int,
                Primitive::Float(_) => ValueType::Float,
                Primitive::Hex(_) => ValueType::Hex,
            },
            Expr::Ident(location) => self.resolve_identifier_type(*location),
            Expr::List(list) => ValueType::List(
                list.items
                    .iter()
                    .map(|expr| self.analyze_expression(expr))
                    .collect(),
            ),
            Expr::Map(map) => {
                let entry_types = map
                    .items
                    .iter()
                    .map(|(key, val)| (self.analyze_expression(key), self.analyze_expression(val)))
                    .collect();

                ValueType::Map(entry_types)
            }
            Expr::Binary(binary) => {
                let lhs_type = self.analyze_expression(&binary.lhs);
                let rhs_type = self.analyze_expression(&binary.rhs);
                let expected_type = self.get_operator_result_type(binary.op);
                self.validate_operand_types(&lhs_type, &rhs_type, &expected_type, binary.op);
                expected_type
            }
            Expr::Unary(unary) => {
                let expr_type = self.analyze_expression(&unary.expr);

                match (unary.op, expr_type) {
                    (Operator::Not, ValueType::Boolean) => ValueType::Boolean,
                    (Operator::Minus, ValueType::Int) => ValueType::Int,
                    (Operator::Minus, ValueType::Float) => ValueType::Float,
                    (op, expr) => {
                        self.add_diagnostic(
                            unary.location,
                            format!("invalid operand type {expr} for operator '{op:?}'"),
                            DiagnosticSeverity::Error,
                        );
                        ValueType::Unknown
                    }
                }
            }
            Expr::Call(call) => {
                for arg in call.args.iter() {
                    self.analyze_expression(arg);
                }
                ValueType::Unknown
            }
            Expr::ArrayIndex(array_index) => {
                let lhs_type = self.analyze_expression(&array_index.lhs);

                let ValueType::List(items) = lhs_type else {
                    return self.error(
                        array_index.lhs.location(),
                        "array index must be applied to a list",
                    );
                };

                let loc = array_index.location;
                let val = match self.extract_integer_literal(expr) {
                    Some(v) if v < 0 => return self.error(loc, "index must be non-negative"),
                    Some(v) if (v as usize) >= items.len() => {
                        return self.error(loc, "array index out of bounds");
                    }
                    Some(v) => v as usize,
                    None => return self.error(loc, "array index must be an integer"),
                };

                items[val].clone()
            }
            Expr::Error(error) => {
                self.add_diagnostic(
                    error.location,
                    format!("unexpected token '{:?}'", error.token),
                    DiagnosticSeverity::Error,
                );
                ValueType::Unknown
            }
        }
    }

    fn resolve_identifier_type(&mut self, location: Location) -> ValueType {
        let name = &self.content[location.to_range()];

        if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
            return match &symbol.symbol_type {
                SymbolType::Variable(value_type) => value_type.clone(),
                SymbolType::Element => ValueType::Unknown,
            };
        }

        if let Some(symbol) = self.global_scope.lookup_symbol(name) {
            return match &symbol.symbol_type {
                SymbolType::Variable(value_type) => value_type.clone(),
                SymbolType::Element => ValueType::Unknown,
            };
        }

        self.add_diagnostic(
            location,
            format!("reference to undefined identifier '{name}'"),
            DiagnosticSeverity::Error,
        );
        ValueType::Unknown
    }

    fn get_operator_result_type(&self, op: Operator) -> ValueType {
        match op {
            Operator::Plus | Operator::Minus | Operator::Mul | Operator::Div | Operator::Mod => {
                // TODO: can't really determine if its an int or float
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
        &mut self,
        lhs_type: &ValueType,
        rhs_type: &ValueType,
        expected_type: &ValueType,
        _op: Operator,
    ) {
        if lhs_type != expected_type && !matches!(lhs_type, ValueType::Unknown) {
            // TODO(wiru): add location information and improve diagnostic message
        }
        if rhs_type != expected_type && !matches!(rhs_type, ValueType::Unknown) {
            // TODO: add location information and improve diagnostic message
        }
    }

    fn get_node_text(&self, node: &AstNode) -> Option<&'src str> {
        match node {
            AstNode::Identifier(location) => Some(&self.content[location.to_range()]),
            AstNode::String(location) => Some(&self.content[location.to_range()]),
            _ => None,
        }
    }

    fn error(&mut self, location: Location, message: impl Into<String>) -> ValueType {
        self.add_diagnostic(location, message.into(), DiagnosticSeverity::Error);
        ValueType::Unknown
    }

    fn add_diagnostic(
        &mut self,
        location: Location,
        message: String,
        severity: DiagnosticSeverity,
    ) {
        self.diagnostics.push(SemanticDiagnostic {
            location,
            message,
            severity,
        });
    }

    fn extract_integer_literal(&self, expr: &Expr) -> Option<i64> {
        match expr {
            Expr::Primitive(primitive) => match primitive.value {
                Primitive::Int(i) => Some(i),
                _ => None,
            },
            _ => None,
        }
    }
}

struct GlobalCollector<'src> {
    file_path: PathBuf,
    content: &'src str,
    analyzer: &'src mut SemanticAnalyzer<'src>,
}

impl<'src> AstVisitor<'src> for GlobalCollector<'src> {
    fn visit_globals(&mut self, decl: &Declaration, _: &AstNode) {
        assert!(decl.is_global());

        let name = decl.name.text(self.content).into();
        let value_type = self.analyzer.analyze_expression(&decl.value);
        let symbol_type = SymbolType::Variable(value_type);
        let location = decl.location;

        self.analyzer.global_scope.declare_global(GlobalSymbol {
            name,
            location,
            symbol_type,
            definition: self.file_path.clone(),
        });
    }
}
