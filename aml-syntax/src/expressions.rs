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
    let mut lhs = match next.kind() {
        TokenKind::Operator(Operator::LBracket) => parse_collection(tokens),
        TokenKind::Operator(Operator::LCurly) => parse_map(tokens),
        TokenKind::Operator(Operator::LParen) => {
            let lhs = parse_expression_inner(tokens, precedences::INITIAL);
            assert!(matches!(
                tokens.next_no_indent().kind(),
                TokenKind::Operator(Operator::RParen)
            ));
            lhs
        }
        TokenKind::Operator(Operator::Minus) => Expr::Unary {
            op: Operator::Not,
            expr: Box::new(parse_expression_inner(tokens, precedences::PREFIX)),
        },
        TokenKind::Operator(Operator::Not) => Expr::Unary {
            op: Operator::Not,
            expr: Box::new(parse_expression_inner(tokens, precedences::PREFIX)),
        },

        TokenKind::String(location) => Expr::String { location },
        TokenKind::Primitive(primitive) => Expr::Primitive(primitive),
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
        | TokenKind::Eof
        | TokenKind::Error(_)
        | TokenKind::Element(_)
        | TokenKind::Newline => {
            return Expr::Error {
                location: next.location(),
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
            Operator::RBracket
            | Operator::RParen
            | Operator::Association
            | Operator::RCurly
            | Operator::Mul
            | Operator::Div
            | Operator::Mod
            | Operator::PlusEqual
            | Operator::MinusEqual
            | Operator::MulEqual
            | Operator::DivEqual
            | Operator::ModEqual
            | Operator::Colon
            | Operator::Comma
            | Operator::Plus
            | Operator::Dot
            | Operator::GreaterThan
            | Operator::GreaterThanOrEqual
            | Operator::LessThan
            | Operator::LessThanOrEqual
            | Operator::EqualEqual
            | Operator::NotEqual
            | Operator::Or
            | Operator::And
            | Operator::Either => {
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
                lhs = parse_function(tokens, lhs);
                continue;
            }
            Operator::LBracket => {
                lhs = Expr::ArrayIndex {
                    lhs: Box::new(lhs),
                    index: Box::new(parse_expression_inner(tokens, precedences::INITIAL)),
                };
                // TODO: error if no right bracket
                let _ = tokens.next_no_indent();
                continue;
            }
            _ => {}
        }

        let rhs = parse_expression_inner(tokens, op_precedence);
        lhs = Expr::Binary {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        };
    }

    lhs
}

fn parse_collection(tokens: &mut Tokens) -> Expr {
    let mut elements = vec![];

    loop {
        match tokens.peek_skip_indent().kind() {
            TokenKind::Newline => {
                tokens.consume();
                continue;
            }
            TokenKind::Operator(Operator::Comma) => {
                tokens.consume();
                continue;
            }
            TokenKind::Operator(Operator::RBracket) => {
                tokens.consume();
                break;
            }
            _ => {}
        }
        elements.push(parse_expression_inner(tokens, precedences::INITIAL));
    }

    Expr::List(elements)
}

fn parse_map(tokens: &mut Tokens) -> Expr {
    let mut items = vec![];

    loop {
        match tokens.peek_skip_indent().kind() {
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
                break;
            }
            _ => {}
        }

        let key = parse_expression_inner(tokens, precedences::INITIAL);
        match tokens.peek_skip_indent().kind() {
            TokenKind::Operator(Operator::Colon) => tokens.consume(),
            _ => break,
        }
        let value = parse_expression_inner(tokens, precedences::INITIAL);
        items.push((key, value));
    }

    Expr::Map { items }
}

fn parse_function(tokens: &mut Tokens, lhs: Expr) -> Expr {
    let mut args = vec![];

    loop {
        match tokens.peek_skip_indent().kind() {
            TokenKind::Operator(Operator::Comma) => {
                tokens.consume();
                continue;
            }
            TokenKind::Operator(Operator::RParen) => {
                tokens.consume();
                break;
            }
            _ => {}
        }
        args.push(parse_expression_inner(tokens, precedences::INITIAL));
    }

    Expr::Call {
        fun: Box::new(lhs),
        args,
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
            op: Operator,
            expr: Box<SnapshotExpr<'ast>>,
        },
        Binary {
            lhs: Box<SnapshotExpr<'ast>>,
            rhs: Box<SnapshotExpr<'ast>>,
            op: Operator,
        },
        Ident {
            location: Location,
            value: &'ast str,
        },
        String {
            location: Location,
            value: &'ast str,
        },
        Call {
            fun: Box<SnapshotExpr<'ast>>,
            args: Vec<SnapshotExpr<'ast>>,
        },
        Primitive(Primitive),
        ArrayIndex {
            lhs: Box<SnapshotExpr<'ast>>,
            index: Box<SnapshotExpr<'ast>>,
        },
        List {
            items: Vec<SnapshotExpr<'ast>>,
        },
        Map {
            items: Vec<(SnapshotExpr<'ast>, SnapshotExpr<'ast>)>,
        },
        Error {
            location: Location,
            token: TokenKind,
        },
    }

    impl<'ast> SnapshotExpr<'ast> {
        pub fn from_expr(expr: Expr, content: &'ast str) -> Self {
            match expr {
                Expr::Unary { op, expr } => Self::Unary {
                    op,
                    expr: Box::new(SnapshotExpr::from_expr(*expr, content)),
                },
                Expr::Binary { lhs, rhs, op } => Self::Binary {
                    lhs: Box::new(SnapshotExpr::from_expr(*lhs, content)),
                    rhs: Box::new(SnapshotExpr::from_expr(*rhs, content)),
                    op,
                },
                Expr::Ident { location } => Self::Ident {
                    value: &content[location.to_range()],
                    location,
                },
                Expr::String { location } => Self::String {
                    value: &content[location.to_range()],
                    location,
                },
                Expr::Call { fun, args } => Self::Call {
                    fun: Box::new(SnapshotExpr::from_expr(*fun, content)),
                    args: args
                        .into_iter()
                        .map(|e| SnapshotExpr::from_expr(e, content))
                        .collect(),
                },
                Expr::Primitive(primitive) => Self::Primitive(primitive),
                Expr::ArrayIndex { lhs, index } => Self::ArrayIndex {
                    lhs: Box::new(SnapshotExpr::from_expr(*lhs, content)),
                    index: Box::new(SnapshotExpr::from_expr(*index, content)),
                },
                Expr::List(exprs) => Self::List {
                    items: exprs
                        .into_iter()
                        .map(|e| SnapshotExpr::from_expr(e, content))
                        .collect(),
                },
                Expr::Map { items } => Self::Map {
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
                Expr::Error { location, token } => Self::Error { location, token },
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
    fn test_add() {
        let input = "1 + 2";
        insta::assert_yaml_snapshot!(parse(input));
    }

    #[test]
    fn test_sub() {
        let input = "1 - 2";
        insta::assert_yaml_snapshot!(parse(input));
    }

    #[test]
    fn test_mul() {
        let input = "5 + 1 * 2";
        insta::assert_yaml_snapshot!(parse(input));
    }

    #[test]
    fn test_div() {
        let input = "5 - 1 / 2";
        insta::assert_yaml_snapshot!(parse(input));
    }

    #[test]
    fn test_brackets() {
        let input = "(5 + 1) * 2";
        insta::assert_yaml_snapshot!(parse(input));
    }

    #[test]
    fn test_function() {
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
    fn test_list() {
        let input = "[1, 2, a, 4]";
        insta::assert_yaml_snapshot!(parse(input));
    }

    #[test]
    fn test_nested_list() {
        let input = "[1, [2, 3, [4, 5]]]";
        insta::assert_yaml_snapshot!(parse(input));
    }

    #[test]
    fn test_equality() {
        let input = "1 == 2";
        insta::assert_yaml_snapshot!(parse(input));
    }

    #[test]
    fn test_map() {
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
    fn test_invalid_expression() {
        let input = "1 ? ]";
        insta::assert_yaml_snapshot!(parse(input));
    }
}
