use aml_syntax::Ast;

use crate::global_scope::GlobalScope;
use crate::{DiagnosticSeverity, SemanticAnalyzer};

fn get_ast(template: &str) -> Ast {
    let tokens = aml_token::Lexer::new(template).collect::<Vec<_>>();
    let tokens = aml_token::Tokens::new(tokens, template.len());
    let parser = aml_syntax::Parser::new(tokens);
    parser.parse()
}

#[test]
fn test_analyze_if_statement_with_multiple_conditions() {
    let template = "if true && false) || true";
    let ast = get_ast(template);
    let mut global_scope = GlobalScope::new();
    let mut analyzer = SemanticAnalyzer::new(template, &mut global_scope);
    let info = analyzer.analyze(&ast);

    assert!(!info.diagnostics.is_empty());
    assert!(info.diagnostics.len() == 1);
    assert!(info.diagnostics[0].message == "unexpected token 'Operator(RParen)'");
    assert!(info.diagnostics[0].severity == DiagnosticSeverity::Error);
}
