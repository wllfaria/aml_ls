use aml_token::{Lexer, Tokens};

use super::snapshots::SnapshotAst;
use crate::parser::Parser;

fn get_ast(template: &str) -> SnapshotAst<'_> {
    let tokens = Lexer::new(template).collect::<Vec<_>>();
    let tokens = Tokens::new(tokens, template.len());
    let parser = Parser::new(tokens);
    SnapshotAst::from_ast(parser.parse(), template)
}

#[test]
fn test_simple_text_element() {
    let template = r#"text "Hello""#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_text_element_with_attributes() {
    let template = r#"text [foreground: #ff0000] "Hello""#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_text_element_with_multiple_attributes() {
    let template = r#"text [foreground: #ff0000, background: #00ff00] "Hello""#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_text_with_multiple_values() {
    let template = r#"text "Hello" world true 1"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_simple_span_element() {
    let template = r#"span "Hello""#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_span_element_with_attributes() {
    let template = r#"span [foreground: #ff0000] "Hello""#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn parse_span_element_with_multiple_attributes() {
    let template = r#"span [foreground: #ff0000, background: #00ff00] "Hello""#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_span_with_multiple_values() {
    let template = r#"span "Hello" world true 1"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_vstack_with_no_children() {
    let template = r#"vstack"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_vstack_with_children() {
    let template = r#"
vstack
    text "Hello"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_vstack_with_multiple_children_and_attributes() {
    let template = r#"
vstack [width: 10]
    text "Hello"
    text "World"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_hstack_with_children() {
    let template = r#"
hstack
    text "Hello"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_hstack_with_multiple_children_and_attributes() {
    let template = r#"
hstack [width: 10]
    text "Hello"
    text "World"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_border_with_no_children() {
    let template = r#"border"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_component() {
    let template = r#"@name"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_component_with_attributes() {
    let template = r#"@name [foreground: #ff0000]"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_component_slot() {
    let template = r#"$children"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_for_loop_inline_list() {
    let template = r#"
for item in [1, 2, 3]
    text item
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_for_loop_identifier() {
    let template = r#"
for item in list_of_items
    text item
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_for_loop_no_value() {
    let template = r#"
for item in
    text item
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_for_loop_no_binding() {
    let template = r#"
for  in [1,2,3]
    text item
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_function_call_on_variable() {
    let template = r#"
local result = func()
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_if_statement() {
    let template = r#"
if condition
    text "Hello"
else
    text "World"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_if_elseif_statement() {
    let template = r#"
if condition
    text "Hello"
else if condition
    text "World"
else
    text "!"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_malformed_if_elseif_statement() {
    let template = r#"
if condition
    text "Hello"
else if
    text "World"
else
    text "!"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_if_statement_with_multiple_conditions() {
    let template = r#"
if (true && true) && (true && true)
    text [foreground: #3812ff] "Hello, world!"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_switch_statement() {
    let template = r#"
switch condition
    case value: text "Hello"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_switch_statement_with_default() {
    let template = r#"
switch condition
    case "foo": text "foo"
    default: text "Hello"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_switch_statement_with_default_and_block_children() {
    let template = r#"
switch condition
    case "foo": text "foo"
    default
        vstack
            text "Hello"
            text "World!"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_switch_statement_with_multiple_cases() {
    let template = r#"
switch condition
    case "foo": text "foo"
    case "bar": text "bar"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_switch_without_colon() {
    let template = r#"
switch condition
    case "foo"  text "foo"
    default: text "Hello"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_with_statement() {
    let template = r#"
with list as [1, 2, 3]
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_with_statement_with_identifier() {
    let template = r#"
with list as my_list
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_with_statement_with_children() {
    let template = r#"
with list as [1, 2, 3]
    vstack
        text "Hello"
        text "World"
"#;
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}

#[test]
fn test_attribute_without_name() {
    let template = "text [: #ff0000]";
    let ast = get_ast(template);
    insta::assert_yaml_snapshot!(ast);
}
