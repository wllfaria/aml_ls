use std::path::PathBuf;

use aml_syntax::Ast;
use aml_syntax::ast::*;

use crate::ValueType;
use crate::analysis::node_analyzer::{AnalysisCtx, NodeAnalyzer};
use crate::diagnostics::{Diagnostics, SemanticDiagnostic};
use crate::global_scope::{GlobalScope, GlobalSymbol};
use crate::symbol_table::{SymbolTable, SymbolType};
use crate::validation::expression::ExpressionAnalyzer;

#[derive(Debug)]
pub struct SemanticInfo {
    pub symbol_table: SymbolTable,
    pub diagnostics: Vec<SemanticDiagnostic>,
}

#[derive(Debug)]
pub struct SemanticAnalyzer<'src> {
    content: &'src str,
    diagnostics: Diagnostics,
    symbol_table: SymbolTable,
    node_analyzer: NodeAnalyzer<'src>,
    global_scope: &'src mut GlobalScope,
    expression_analyzer: ExpressionAnalyzer<'src>,
}

impl<'src> SemanticAnalyzer<'src> {
    pub fn new(content: &'src str, global_scope: &'src mut GlobalScope) -> SemanticAnalyzer<'src> {
        SemanticAnalyzer {
            content,
            global_scope,
            diagnostics: Default::default(),
            symbol_table: SymbolTable::new(),
            node_analyzer: NodeAnalyzer::new(content),
            expression_analyzer: ExpressionAnalyzer::new(content),
        }
    }

    pub fn analyze(&mut self, ast: &Ast) -> SemanticInfo {
        for node in ast.nodes.iter() {
            self.analyze_node(node, None);
        }

        SemanticInfo {
            symbol_table: std::mem::take(&mut self.symbol_table),
            diagnostics: std::mem::take(&mut self.diagnostics).items(),
        }
    }

    fn analyze_node(&mut self, node: &AstNode, parent: Option<&AstNode>) {
        self.node_analyzer.analyze_node(&mut AnalysisCtx {
            node,
            parent,
            global_scope: self.global_scope,
            diagnostics: &mut self.diagnostics,
            symbol_table: &mut self.symbol_table,
        });
    }

    pub fn analyze_expression(&mut self, expr: &Expr, node: &AstNode) -> ValueType {
        self.expression_analyzer.analyze_expression(
            expr,
            &mut AnalysisCtx {
                node,
                parent: None,
                global_scope: self.global_scope,
                diagnostics: &mut self.diagnostics,
                symbol_table: &mut self.symbol_table,
            },
        )
    }

    pub fn collect_globals(&'src mut self, ast: &'src Ast) {
        ast.accept(&mut GlobalCollector {
            file_path: PathBuf::new(),
            content: self.content,
            analyzer: self,
        });
    }
}

struct GlobalCollector<'src> {
    file_path: PathBuf,
    content: &'src str,
    analyzer: &'src mut SemanticAnalyzer<'src>,
}

impl<'src> AstVisitor<'src> for GlobalCollector<'src> {
    fn visit_globals(&mut self, decl: &Declaration, node: &AstNode) {
        assert!(decl.is_global());

        let name = decl.name.text(self.content).into();

        let expression_analyzer = &self.analyzer.expression_analyzer;
        let symbol_table = &mut self.analyzer.symbol_table;
        let global_scope = &mut self.analyzer.global_scope;
        let diagnostics = &mut self.analyzer.diagnostics;

        let value_type = expression_analyzer.analyze_expression(
            &decl.value,
            &mut AnalysisCtx {
                node,
                parent: None,
                symbol_table,
                global_scope,
                diagnostics,
            },
        );

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
