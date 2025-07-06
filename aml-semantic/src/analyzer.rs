use aml_core::Location;
use aml_syntax::ast::Attributes;
use aml_syntax::{Ast, AstNode, Expr};
use aml_token::{Operator, Primitive};

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

#[derive(Debug, Default)]
pub struct SemanticAnalyzer<'source> {
    symbol_table: SymbolTable,
    diagnostics: Vec<SemanticDiagnostic>,
    content: &'source str,
}

impl<'source> SemanticAnalyzer<'source> {
    pub fn new(content: &'source str) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            diagnostics: Vec::new(),
            content,
        }
    }

    pub fn analyze(&mut self, ast: &Ast) -> SemanticInfo {
        ast.nodes
            .iter()
            .for_each(|node| self.collect_declarations(node));

        ast.nodes.iter().for_each(|node| self.analyze_node(node));

        SemanticInfo {
            symbol_table: std::mem::take(&mut self.symbol_table),
            diagnostics: std::mem::take(&mut self.diagnostics),
        }
    }

    fn collect_declarations(&mut self, node: &AstNode) {
        match node {
            AstNode::Text { children, .. } => children
                .iter()
                .for_each(|child| self.collect_declarations(child)),
            AstNode::Declaration {
                name,
                value,
                location,
            } => self.declare_variable(name, value, *location),
            AstNode::Container { children, .. } => children
                .iter()
                .for_each(|child| self.collect_declarations(child)),
            _ => {}
        }
    }

    fn declare_variable(&mut self, name: &AstNode, value: &Expr, location: Location) {
        let name = self.get_node_text(name);
        let value_type = self.analyze_expression(value);
        let symbol_type = SymbolType::Variable { value_type };

        self.symbol_table
            .declare_symbol(name.into(), location, symbol_type);
    }

    fn analyze_node(&mut self, node: &AstNode) {
        match node {
            AstNode::Text {
                values,
                attributes,
                children,
                location,
                ..
            } => self.analyze_text_element(values, attributes, children, *location),
            AstNode::Span {
                values, attributes, ..
            } => self.analyze_span_element(values, attributes),
            AstNode::Container { children, .. } => {
                children.iter().for_each(|child| self.analyze_node(child))
            }
            AstNode::Attribute { value, .. } => _ = self.analyze_expression(value),
            AstNode::Identifier { .. } => {}
            AstNode::String { .. } => {}
            AstNode::Primitive { .. } => {}
            AstNode::Error { .. } => {}

            AstNode::Declaration { .. } => {
                // we collected variables before analyzing to hoist them to the root scope
                // so we can just skip them here
            }
            AstNode::Component { .. } => {}
            AstNode::ComponentSlot { .. } => {}
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
            .attributes
            .iter()
            .for_each(|attr| self.analyze_node(attr));
        self.validate_text_element_value(value, location);
        children.iter().for_each(|child| self.analyze_node(child));
        self.symbol_table.pop_scope();
    }

    fn analyze_span_element(&mut self, values: &[AstNode], attributes: &Attributes) {
        attributes
            .attributes
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
                    let Some(_) = self.symbol_table.lookup_symbol(name) else {
                        self.add_diagnostic(
                            location,
                            format!("reference to undefined identifier '{name}'"),
                            DiagnosticSeverity::Error,
                        );
                        return;
                    };
                }
                _ => self.add_diagnostic(
                    location,
                    "Text element value must be a string literal".into(),
                    DiagnosticSeverity::Error,
                ),
            }
        }
    }

    fn analyze_expression(&mut self, expr: &Expr) -> ValueType {
        match expr {
            Expr::String { .. } => ValueType::String,
            Expr::Primitive { value, .. } => self.infer_primitive_type(value),
            Expr::Ident { location } => self.resolve_identifier_type(*location),
            Expr::List { items, .. } => {
                ValueType::List(Box::new(self.infer_collection_element_type(items)))
            }
            Expr::Map { items, .. } => self.infer_map_type(items),
            Expr::Binary { lhs, rhs, op, .. } => self.infer_binary_expression_type(lhs, rhs, op),
            Expr::Unary { expr, op, .. } => self.infer_unary_expression_type(expr, op),
            Expr::Call { fun, args, .. } => self.analyze_function_call(fun, args),
            Expr::ArrayIndex { lhs, index, .. } => self.analyze_array_access(lhs, index),
            Expr::Error { .. } => todo!(),
        }
    }

    fn infer_primitive_type(&self, primitive: &Primitive) -> ValueType {
        match primitive {
            Primitive::Bool(_) => ValueType::Boolean,
            Primitive::Int(_) => ValueType::Number,
            Primitive::Float(_) => ValueType::Number,
            Primitive::Hex(_) => ValueType::Hex,
        }
    }

    fn resolve_identifier_type(&mut self, location: Location) -> ValueType {
        let name = &self.content[location.to_range()];

        let Some(symbol) = self.symbol_table.lookup_symbol(name) else {
            self.add_diagnostic(
                location,
                format!("reference to undefined identifier '{name}'"),
                DiagnosticSeverity::Error,
            );
            return ValueType::Unknown;
        };

        match &symbol.symbol_type {
            SymbolType::Variable { value_type } => value_type.clone(),
            SymbolType::Element => ValueType::Unknown,
        }
    }

    fn infer_collection_element_type(&mut self, exprs: &[Expr]) -> ValueType {
        exprs
            .iter()
            .map(|expr| self.analyze_expression(expr))
            .find(|t| !matches!(t, ValueType::Unknown))
            .unwrap_or(ValueType::Unknown)
    }

    fn infer_map_type(&mut self, items: &[(Expr, Expr)]) -> ValueType {
        let (key_type, value_type) = items
            .iter()
            .map(|(key, val)| (self.analyze_expression(key), self.analyze_expression(val)))
            .fold(
                (ValueType::Unknown, ValueType::Unknown),
                |(acc_k, acc_v), (k, v)| {
                    (
                        if matches!(acc_k, ValueType::Unknown) { k } else { acc_k },
                        if matches!(acc_v, ValueType::Unknown) { v } else { acc_v },
                    )
                },
            );

        ValueType::Map(Box::new(key_type), Box::new(value_type))
    }

    fn infer_binary_expression_type(&mut self, lhs: &Expr, rhs: &Expr, op: &Operator) -> ValueType {
        let lhs_type = self.analyze_expression(lhs);
        let rhs_type = self.analyze_expression(rhs);
        let expected_type = self.get_operator_result_type(op);

        self.validate_operand_types(&lhs_type, &rhs_type, &expected_type, op);
        expected_type
    }

    fn infer_unary_expression_type(&mut self, expr: &Expr, op: &Operator) -> ValueType {
        let expr_type = self.analyze_expression(expr);
        match op {
            Operator::Not => ValueType::Boolean,
            Operator::Minus => ValueType::Number,
            _ => expr_type,
        }
    }

    fn analyze_function_call(&mut self, fun: &Expr, args: &[Expr]) -> ValueType {
        self.analyze_expression(fun);
        for arg in args {
            self.analyze_expression(arg);
        }
        ValueType::Unknown
    }

    fn analyze_array_access(&mut self, lhs: &Expr, index: &Expr) -> ValueType {
        let lhs_type = self.analyze_expression(lhs);
        self.analyze_expression(index);

        match lhs_type {
            ValueType::List(element_type) => *element_type,
            _ => ValueType::Unknown,
        }
    }

    fn get_operator_result_type(&self, op: &Operator) -> ValueType {
        match op {
            Operator::Plus | Operator::Minus | Operator::Mul | Operator::Div | Operator::Mod => {
                ValueType::Number
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
        _op: &Operator,
    ) {
        if lhs_type != expected_type && !matches!(lhs_type, ValueType::Unknown) {
            // TODO(wiru): add location information and improve diagnostic message
        }
        if rhs_type != expected_type && !matches!(rhs_type, ValueType::Unknown) {
            // TODO: add location information and improve diagnostic message
        }
    }

    fn get_node_text(&self, node: &AstNode) -> &'source str {
        match node {
            AstNode::Identifier { location } => &self.content[location.to_range()],
            AstNode::String { location } => &self.content[location.to_range()],
            _ => panic!("Unsupported node type for text extraction"),
        }
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
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_info(template: &str) -> SemanticInfo {
        let tokens = aml_token::Lexer::new(template).collect();
        let tokens = aml_token::Tokens::new(tokens, template.len());
        let ast = aml_syntax::Parser::new(tokens).parse();
        SemanticAnalyzer::new(template).analyze(&ast)
    }

    #[test]
    fn test_text_without_value() {
        let template = r#"text"#;
        let semantic_info = get_info(template);
        assert_eq!(
            semantic_info.diagnostics,
            vec![SemanticDiagnostic {
                location: Location::new(0, 4),
                message: "Text element has no value to display".into(),
                severity: DiagnosticSeverity::Warning,
            }]
        );
    }

    #[test]
    fn test_text_with_multiple_values() {
        let template = r#"text "Hello, " "World!""#;
        let semantic_info = get_info(template);
        assert!(semantic_info.diagnostics.is_empty());
    }

    #[test]
    fn test_text_with_declared_variable() {
        let template = r#"
let my_bg = "red"
text [foreground: my_bg] "Hello, World!"
    "#;
        let semantic_info = get_info(template);
        assert!(semantic_info.diagnostics.is_empty());
    }

    #[test]
    fn test_text_with_undeclared_variable() {
        let template = r#"text [foreground: undeclared] "Hello, World!""#;
        let semantic_info = get_info(template);
        assert_eq!(
            semantic_info.diagnostics,
            [SemanticDiagnostic {
                location: Location::new(18, 28),
                message: "reference to undefined identifier 'undeclared'".into(),
                severity: DiagnosticSeverity::Error,
            }]
        );
    }
}
