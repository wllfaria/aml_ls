use std::collections::HashMap;

use aml_core::Location;
use aml_syntax::{Ast, AstNode, Expr};
use aml_token::{Operator, Primitive};

use crate::symbol_table::{SymbolTable, SymbolType, ValueType};

#[derive(Debug)]
pub struct SemanticInfo {
    pub symbol_table: SymbolTable,
    pub variable_references: HashMap<Location, String>,
    pub diagnostics: Vec<SemanticDiagnostic>,
}

#[derive(Debug, Clone)]
pub struct SemanticDiagnostic {
    pub location: Location,
    pub message: String,
    pub severity: DiagnosticSeverity,
}

#[derive(Debug, Clone)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Default)]
pub struct SemanticAnalyzer<'source> {
    symbol_table: SymbolTable,
    diagnostics: Vec<SemanticDiagnostic>,
    variable_references: HashMap<Location, String>,
    content: &'source str,
}

impl<'source> SemanticAnalyzer<'source> {
    pub fn new(content: &'source str) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            diagnostics: Vec::new(),
            variable_references: HashMap::new(),
            content,
        }
    }

    pub fn analyze(&mut self, ast: &Ast) -> SemanticInfo {
        for node in &ast.nodes {
            self.analyze_node(node);
        }

        SemanticInfo {
            symbol_table: std::mem::take(&mut self.symbol_table),
            variable_references: std::mem::take(&mut self.variable_references),
            diagnostics: std::mem::take(&mut self.diagnostics),
        }
    }

    fn analyze_node(&mut self, node: &AstNode) {
        match node {
            AstNode::Text {
                value,
                attributes,
                children,
                location,
            } => {
                self.symbol_table.push_scope(None);

                attributes.iter().for_each(|attr| self.analyze_node(attr));

                match value {
                    Some(value) if matches!(value.as_ref(), AstNode::String { .. }) => {}
                    Some(_) => self.add_diagnostic(*location, "".into(), DiagnosticSeverity::Error),
                    None => self.add_diagnostic(*location, "".into(), DiagnosticSeverity::Error),
                }

                children.iter().for_each(|child| self.analyze_node(child));

                self.symbol_table.pop_scope();
            }
            AstNode::Span {
                value, attributes, ..
            } => {
                attributes.iter().for_each(|attr| self.analyze_node(attr));
                if let Some(value_node) = value {
                    self.analyze_node(value_node);
                }
            }
            AstNode::Attribute { value, .. } => _ = self.analyze_expression(value),
            AstNode::Identifier { .. } => {
                // TODO: a dangling identifier at the same level of an element.
                // do something here
            }
            AstNode::String { .. } => {
                // TODO: what to do here?
            }
            AstNode::Declaration {
                name,
                value,
                location,
            } => {
                let name = self.get_node_text(name);
                let value_type = self.analyze_expression(value);
                let symbol_type = SymbolType::Variable { value_type };

                self.symbol_table
                    .declare_symbol(name.to_string(), *location, symbol_type);
            }
        }
    }

    fn analyze_expression(&mut self, expr: &Expr) -> ValueType {
        match expr {
            Expr::String { .. } => ValueType::String,
            Expr::Primitive(primitive) => match primitive {
                Primitive::Bool(_) => ValueType::Boolean,
                Primitive::Int(_) => ValueType::Number,
                Primitive::Float(_) => ValueType::Number,
                Primitive::Hex(_) => ValueType::Hex,
            },
            Expr::Ident { location } => {
                let name = &self.content[location.to_range()];

                let Some(symbol) = self.symbol_table.lookup_symbol(name) else {
                    self.add_diagnostic(
                        *location,
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
            Expr::List(exprs) => {
                let mut element_type = ValueType::Unknown;
                for expr in exprs {
                    let expr_type = self.analyze_expression(expr);
                    if matches!(element_type, ValueType::Unknown) {
                        element_type = expr_type;
                    }
                }
                ValueType::List(Box::new(element_type))
            }
            Expr::Map { items } => {
                let mut key_type = ValueType::Unknown;
                let mut value_type = ValueType::Unknown;

                for (key, val) in items {
                    let k_type = self.analyze_expression(key);
                    let v_type = self.analyze_expression(val);

                    if matches!(key_type, ValueType::Unknown) {
                        key_type = k_type;
                    }
                    if matches!(value_type, ValueType::Unknown) {
                        value_type = v_type;
                    }
                }

                ValueType::Map(Box::new(key_type), Box::new(value_type))
            }
            Expr::Binary { lhs, rhs, op } => {
                let lhs_type = self.analyze_expression(lhs);
                let rhs_type = self.analyze_expression(rhs);

                let op_type = match op {
                    Operator::Plus
                    | Operator::Minus
                    | Operator::Mul
                    | Operator::Div
                    | Operator::Mod => ValueType::Number,
                    Operator::EqualEqual
                    | Operator::NotEqual
                    | Operator::GreaterThan
                    | Operator::LessThan
                    | Operator::GreaterThanOrEqual
                    | Operator::LessThanOrEqual
                    | Operator::And
                    | Operator::Or => ValueType::Boolean,
                    _ => ValueType::Unknown,
                };

                if lhs_type != op_type {
                    // TODO: diagnostic of wrong type in operator
                }

                if rhs_type != op_type {
                    // TODO: diagnostic of wrong type in operator
                }

                op_type
            }
            Expr::Unary { expr, op } => {
                let expr_type = self.analyze_expression(expr);
                match op {
                    Operator::Not => ValueType::Boolean,
                    Operator::Minus => ValueType::Number,
                    _ => expr_type,
                }
            }
            Expr::Call { fun, args } => {
                self.analyze_expression(fun);
                for arg in args {
                    self.analyze_expression(arg);
                }
                ValueType::Unknown
            }
            Expr::ArrayIndex { lhs, index } => {
                let lhs_type = self.analyze_expression(lhs);
                self.analyze_expression(index);

                match lhs_type {
                    ValueType::List(element_type) => *element_type,
                    _ => ValueType::Unknown,
                }
            }
        }
    }

    fn get_node_text(&self, node: &AstNode) -> &'source str {
        match node {
            AstNode::Identifier { value } => &self.content[value.to_range()],
            _ => todo!(),
        }
    }

    fn get_node_location(&self, node: &AstNode) -> Location {
        match node {
            AstNode::Identifier { value } => *value,
            AstNode::String { value } => *value,
            AstNode::Text { location, .. } => *location,
            AstNode::Span { location, .. } => *location,
            _ => todo!(),
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

    #[test]
    fn test_analysis() {
        let template = r#"
text [foreground: my_bg] ""
    let my_bg = #ff0000
    span [foreground: my_bg] 'something'
"#;
        let tokens = aml_token::Lexer::new(template).collect();
        let tokens = aml_token::Tokens::new(tokens, template.len());
        let ast = aml_syntax::Parser::new(tokens).parse();
        let semantic_info = SemanticAnalyzer::new(template).analyze(&ast);

        println!("{semantic_info:#?}");

        panic!();
    }
}
