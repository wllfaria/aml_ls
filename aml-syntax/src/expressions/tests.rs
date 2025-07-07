use aml_token::{Lexer, Tokens};

use super::snapshots::SnapshotExpr;
use crate::expressions::precedences;
use crate::parser::snapshots::ToSnapshot;

fn parse(input: &str) -> SnapshotExpr<'_> {
    let tokens = Lexer::new(input).collect();
    let mut tokens = Tokens::new(tokens, input.len());
    let expression = super::parse_expression_inner(&mut tokens, precedences::INITIAL);
    expression.into_snapshot(input)
}

#[test]
fn test_simple_add() {
    let input = "1 + 2";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_simple_sub() {
    let input = "1 - 2";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_simple_mul() {
    let input = "5 + 1 * 2";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_simple_div() {
    let input = "5 - 1 / 2";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_expr_grouping_precedence() {
    let input = "(5 + 1) * 2";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_function_call() {
    let input = "fun(1, a + 2 * 3, 3)";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_function_no_args() {
    let input = "f()";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_array_index() {
    let input = "array[0][1]";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_map_lookup() {
    let input = "map['key']";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_dot_lookup() {
    let input = "a.b.c";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_modulo() {
    let input = "5 + 1 % 2";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_simple_list() {
    let input = "[1, 2, a, 4]";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_nested_list() {
    let input = "[1, [2, 3, [4, 5]]]";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_simple_equality() {
    let input = "1 == 2";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_map_declaration() {
    let input = "{a: 1, b: c}";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_and() {
    let input = "1 == 2 && 3 == 4";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_not() {
    let input = "1 != 2 && 3 != 4";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_either() {
    let input = "a ? b ? c";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_unclosed_parenthesis() {
    let input = "func(1, 2";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_unclosed_bracket() {
    let input = "[1, 2, 3";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_unclosed_curly_brace() {
    let input = "{a: 1, b: 2";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_missing_comma_in_list() {
    let input = "[1 + 12 * a b c - 13]";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_missing_comma_in_function_args() {
    let input = "foo(1 2 3)";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_missing_operand() {
    let input = "1 + ";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_missing_left_operand() {
    let input = "* 2";
    insta::assert_yaml_snapshot!(parse(input));
}

// TODO: this test panics right now due to an asssertion, but it should produce a nice error
// node for error reporting
// #[test]
// fn test_mixed_brackets() {
//     let input = "(1 + [2)";
//     insta::assert_yaml_snapshot!(parse(input));
// }

#[test]
fn test_incomplete_binary_expression() {
    let input = "a +";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_double_operators() {
    let input = "1 + + 2";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_empty_map_entry() {
    let input = "{: 1, b:}";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_invalid_map_key() {
    let input = "{1+2: 3}";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_incomplete_dot_lookup() {
    let input = "obj.";
    insta::assert_yaml_snapshot!(parse(input));
}

// TODO: this also hits an assertion error, which it shouldn't
// #[test]
// fn test_empty_array_index() {
//     let input = "array[]";
//     insta::assert_yaml_snapshot!(parse(input));
// }

#[test]
fn test_invalid_either() {
    let input = "a ? ";
    insta::assert_yaml_snapshot!(parse(input));
}

#[test]
fn test_consecutive_either() {
    let input = "a ? ? c";
    insta::assert_yaml_snapshot!(parse(input));
}
