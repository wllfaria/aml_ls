use aml_core::Location;
use aml_token::{Operator, TokenKind, Tokens};

use crate::ast::Expr;

pub mod precedences {
    pub const INITIAL: u8 = 0;
    pub const CONDITIONAL: u8 = 2;
    pub const EQUALITY: u8 = 3;
    pub const LOGICAL: u8 = 4;
    pub const SUM: u8 = 5;
    pub const PRODUCT: u8 = 6;
    pub const PREFIX: u8 = 8;
    pub const CALL: u8 = 10;
    pub const SUBCRIPT: u8 = 11;
}

fn get_precedence(op: Operator) -> u8 {
    match op {
        Operator::Dot | Operator::LBracket => precedences::SUBCRIPT,
        Operator::LParen => precedences::CALL,
        Operator::Mul | Operator::Div | Operator::Mod => precedences::PRODUCT,
        Operator::Plus | Operator::Minus => precedences::SUM,
        Operator::GreaterThan
        | Operator::GreaterThanOrEqual
        | Operator::LessThan
        | Operator::LessThanOrEqual => precedences::LOGICAL,
        Operator::EqualEqual | Operator::NotEqual => precedences::EQUALITY,
        Operator::Or | Operator::And | Operator::Either => precedences::CONDITIONAL,

        _ => precedences::INITIAL,
    }
}

pub fn parse_expression(tokens: &mut Tokens) -> Expr {
    parse_expression_inner(tokens, precedences::INITIAL)
}

fn parse_expression_inner(tokens: &mut Tokens, precedence: u8) -> Expr {
    let next = tokens.next_no_indent();
    let location = next.location();

    let mut lhs = match next.kind() {
        TokenKind::Operator(Operator::LBracket) => parse_collection(tokens, location),
        TokenKind::Operator(Operator::LCurly) => parse_map(tokens, location),
        TokenKind::Operator(Operator::LParen) => {
            let lhs = parse_expression_inner(tokens, precedences::INITIAL);
            assert!(matches!(
                tokens.next_no_indent().kind(),
                TokenKind::Operator(Operator::RParen)
            ));
            lhs
        }
        TokenKind::Operator(op @ Operator::Minus) => parse_unary_expression(tokens, op, location),
        TokenKind::Operator(op @ Operator::Not) => parse_unary_expression(tokens, op, location),
        TokenKind::String(location) => Expr::String { location },
        TokenKind::Primitive(primitive) => Expr::Primitive {
            value: primitive,
            location,
        },
        TokenKind::Identifier(location) => Expr::Ident { location },

        // all of these are invalid
        TokenKind::Operator(Operator::RBracket)
        | TokenKind::Operator(Operator::RParen)
        | TokenKind::Operator(Operator::Association)
        | TokenKind::Operator(Operator::RCurly)
        | TokenKind::Operator(Operator::Mul)
        | TokenKind::Operator(Operator::Div)
        | TokenKind::Operator(Operator::Mod)
        | TokenKind::Operator(Operator::PlusEqual)
        | TokenKind::Operator(Operator::MinusEqual)
        | TokenKind::Operator(Operator::MulEqual)
        | TokenKind::Operator(Operator::DivEqual)
        | TokenKind::Operator(Operator::ModEqual)
        | TokenKind::Operator(Operator::Colon)
        | TokenKind::Operator(Operator::Comma)
        | TokenKind::Operator(Operator::Plus)
        | TokenKind::Operator(Operator::Dot)
        | TokenKind::Operator(Operator::GreaterThan)
        | TokenKind::Operator(Operator::GreaterThanOrEqual)
        | TokenKind::Operator(Operator::LessThan)
        | TokenKind::Operator(Operator::LessThanOrEqual)
        | TokenKind::Operator(Operator::EqualEqual)
        | TokenKind::Operator(Operator::NotEqual)
        | TokenKind::Operator(Operator::Or)
        | TokenKind::Operator(Operator::And)
        | TokenKind::Operator(Operator::Either)
        | TokenKind::Equal
        | TokenKind::For
        | TokenKind::In
        | TokenKind::If
        | TokenKind::Else
        | TokenKind::Switch
        | TokenKind::Case
        | TokenKind::Default
        | TokenKind::With
        | TokenKind::As
        | TokenKind::Component
        | TokenKind::ComponentSlot
        | TokenKind::Decl
        | TokenKind::Local
        | TokenKind::Global
        | TokenKind::Eof
        | TokenKind::Error(_)
        | TokenKind::Element(_)
        | TokenKind::Container(_)
        | TokenKind::Newline => {
            return Expr::Error {
                location,
                token: next.kind(),
            };
        }

        TokenKind::Indent(_) => unreachable!(),
    };

    loop {
        let TokenKind::Operator(op) = tokens.peek_skip_indent().kind() else {
            return lhs;
        };

        match op {
            Operator::Association
            | Operator::PlusEqual
            | Operator::MinusEqual
            | Operator::MulEqual
            | Operator::DivEqual
            | Operator::ModEqual => {
                let token = tokens.next_no_indent();
                return Expr::Error {
                    location: token.location(),
                    token: token.kind(),
                };
            }
            _ => {}
        }

        let op_precedence = get_precedence(op);

        if precedence >= op_precedence {
            break;
        }

        tokens.consume();

        match op {
            Operator::LParen => {
                lhs = parse_function(tokens, lhs, location);
                continue;
            }
            Operator::LBracket => {
                let index = parse_expression_inner(tokens, precedences::INITIAL);
                let next_token = tokens.next_no_indent();

                assert!(matches!(
                    next_token.kind(),
                    TokenKind::Operator(Operator::RBracket)
                ));

                lhs = Expr::ArrayIndex {
                    lhs: Box::new(lhs),
                    location: location.merge(next_token.location()),
                    index: Box::new(index),
                };

                continue;
            }
            _ => {}
        }

        let rhs = parse_expression_inner(tokens, op_precedence);
        let location = location.merge(rhs.location());
        lhs = Expr::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            location,
        };
    }

    lhs
}

fn parse_unary_expression(tokens: &mut Tokens, operator: Operator, location: Location) -> Expr {
    let expr = parse_expression_inner(tokens, precedences::PREFIX);
    let location = location.merge(expr.location());

    Expr::Unary {
        op: operator,
        expr: Box::new(expr),
        location,
    }
}

fn parse_collection(tokens: &mut Tokens, start_location: Location) -> Expr {
    let mut items = vec![];

    let end_location = loop {
        let next_token = tokens.peek_skip_indent();
        match next_token.kind() {
            TokenKind::Newline => {
                tokens.consume();
                break next_token.location();
            }
            TokenKind::Eof => {
                items.push(Expr::Error {
                    location: next_token.location(),
                    token: next_token.kind(),
                });
                break next_token.location();
            }
            TokenKind::Operator(Operator::RBracket) => {
                tokens.consume();
                break next_token.location();
            }
            TokenKind::Operator(Operator::Comma) => {
                tokens.consume();
                continue;
            }
            _ => {}
        }
        items.push(parse_expression_inner(tokens, precedences::INITIAL));
    };

    let location = start_location.merge(end_location);
    Expr::List { items, location }
}

fn parse_map(tokens: &mut Tokens, start_location: Location) -> Expr {
    let mut items = vec![];

    let end_location = loop {
        let next_token = tokens.peek_skip_indent();
        match next_token.kind() {
            TokenKind::Newline => {
                tokens.consume();
                continue;
            }
            TokenKind::Operator(Operator::Comma) => {
                tokens.consume();
                continue;
            }
            TokenKind::Operator(Operator::RCurly) => {
                tokens.consume();
                break next_token.location();
            }
            _ => {}
        }

        let key = parse_expression_inner(tokens, precedences::INITIAL);

        match tokens.peek_skip_indent().kind() {
            TokenKind::Operator(Operator::Colon) => tokens.consume(),
            _ => {
                items.push((
                    key,
                    Expr::Error {
                        location: next_token.location(),
                        token: next_token.kind(),
                    },
                ));
                break next_token.location();
            }
        }
        let value = parse_expression_inner(tokens, precedences::INITIAL);
        items.push((key, value));
    };

    let location = start_location.merge(end_location);

    Expr::Map { items, location }
}

fn parse_function(tokens: &mut Tokens, lhs: Expr, start_location: Location) -> Expr {
    let mut args = vec![];

    let end_location = loop {
        let next_token = tokens.peek_skip_indent();
        match next_token.kind() {
            TokenKind::Operator(Operator::Comma) => {
                tokens.consume();
                continue;
            }
            TokenKind::Operator(Operator::RParen) => {
                tokens.consume();
                break next_token.location();
            }
            TokenKind::Eof => {
                args.push(Expr::Error {
                    location: next_token.location(),
                    token: next_token.kind(),
                });
                break next_token.location();
            }
            _ => {}
        }
        args.push(parse_expression_inner(tokens, precedences::INITIAL));
    };

    let location = start_location.merge(end_location);

    Expr::Call {
        fun: Box::new(lhs),
        args,
        location,
    }
}

#[cfg(test)]
pub(crate) mod test {
    use aml_core::Location;
    use aml_token::{Lexer, Primitive};
    use serde::Serialize;

    use super::*;

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

    fn parse(input: &str) -> SnapshotExpr<'_> {
        let tokens = Lexer::new(input).collect();
        let mut tokens = Tokens::new(tokens, input.len());
        let expression = parse_expression_inner(&mut tokens, precedences::INITIAL);
        SnapshotExpr::from_expr(expression, input)
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
}
