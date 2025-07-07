use std::iter::Peekable;
use std::str::CharIndices;

use crate::Container;
use crate::token::{Element, IntoToken, LexError, Operator, Primitive, Token, TokenKind};

pub struct Lexer<'lex> {
    chars: Peekable<CharIndices<'lex>>,
    content: &'lex str,
}

impl<'lex> Lexer<'lex> {
    pub fn new(content: &'lex str) -> Self {
        Self {
            chars: content.char_indices().peekable(),
            content,
        }
    }

    fn next_token(&mut self) -> Token {
        let (index, curr) = match self.chars.next() {
            None => return self.eof(),
            Some(curr) => curr,
        };

        let next = self.chars.peek().map(|(_, c)| *c);

        match (curr, next) {
            ('/', Some('/')) => {
                self.chars.next(); // consume the second slash
                loop {
                    if let Some((_, '\n')) | None = self.chars.peek() {
                        break;
                    };
                    self.chars.next();
                }
                self.next_token()
            }
            ('&', Some('&')) => {
                let _ = self.chars.next();
                Operator::And.into_token(index, index + 2)
            }
            ('|', Some('|')) => {
                let _ = self.chars.next();
                Operator::Or.into_token(index, index + 2)
            }
            ('=', Some('=')) => {
                let _ = self.chars.next();
                Operator::EqualEqual.into_token(index, index + 2)
            }
            ('!', Some('=')) => {
                let _ = self.chars.next();
                Operator::NotEqual.into_token(index, index + 2)
            }
            ('>', Some('=')) => {
                let _ = self.chars.next();
                Operator::GreaterThanOrEqual.into_token(index, index + 2)
            }
            ('<', Some('=')) => {
                let _ = self.chars.next();
                Operator::LessThanOrEqual.into_token(index, index + 2)
            }
            ('-', Some('>')) => {
                let _ = self.chars.next();
                Operator::Association.into_token(index, index + 2)
            }

            ('(', _) => Operator::LParen.into_token(index, index + 1),
            (')', _) => Operator::RParen.into_token(index, index + 1),
            ('[', _) => Operator::LBracket.into_token(index, index + 1),
            (']', _) => Operator::RBracket.into_token(index, index + 1),
            ('{', _) => Operator::LCurly.into_token(index, index + 1),
            ('}', _) => Operator::RCurly.into_token(index, index + 1),
            (':', _) => Operator::Colon.into_token(index, index + 1),
            (',', _) => Operator::Comma.into_token(index, index + 1),
            ('.', _) => Operator::Dot.into_token(index, index + 1),
            ('!', _) => Operator::Not.into_token(index, index + 1),
            ('+', _) => Operator::Plus.into_token(index, index + 1),
            ('-', _) => Operator::Minus.into_token(index, index + 1),
            ('*', _) => Operator::Mul.into_token(index, index + 1),
            ('/', _) => Operator::Div.into_token(index, index + 1),
            ('%', _) => Operator::Mod.into_token(index, index + 1),
            ('>', _) => Operator::GreaterThan.into_token(index, index + 1),
            ('<', _) => Operator::LessThan.into_token(index, index + 1),
            ('?', _) => Operator::Either.into_token(index, index + 1),
            ('=', _) => TokenKind::Equal.into_token(index, index + 1),
            ('\n', _) => TokenKind::Newline.into_token(index, index + 1),
            ('@', _) => TokenKind::Component.into_token(index, index + 1),
            ('$', _) => TokenKind::ComponentSlot.into_token(index, index + 1),

            ('a'..='z' | 'A'..='Z' | '_', _) => self.lex_identifier(index),
            ('0'..='9', _) => self.lex_number(index),
            ('"' | '\'', _) => self.lex_string(curr, index),
            _ if curr.is_whitespace() => self.lex_indent(index),
            ('#', Some('0'..='9' | 'a'..='f' | 'A'..='F')) => self.lex_hex_value(index),
            _ => self.eof(),
        }
    }

    fn eof(&self) -> Token {
        let location = self.content.len()..self.content.len();
        Token(TokenKind::Eof, location.into())
    }

    fn lex_identifier(&mut self, start_byte: usize) -> Token {
        let mut end_byte = start_byte;
        while let Some((e, 'a'..='z' | 'A'..='Z' | '_' | '|' | '0'..='9')) = self.chars.peek() {
            end_byte = *e;
            self.chars.next();
        }

        let str = &self.content[start_byte..=end_byte];

        let kind = match str {
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "true" => TokenKind::Primitive(Primitive::Bool(true)),
            "false" => TokenKind::Primitive(Primitive::Bool(false)),
            "let" => TokenKind::Decl,
            "local" => TokenKind::Local,
            "global" => TokenKind::Global,
            "switch" => TokenKind::Switch,
            "case" => TokenKind::Case,
            "default" => TokenKind::Default,
            "with" => TokenKind::With,
            "as" => TokenKind::As,
            "text" => TokenKind::Element(Element::Text),
            "span" => TokenKind::Element(Element::Span),
            "border" => TokenKind::Container(Container::Border),
            "alignment" => TokenKind::Container(Container::Alignment),
            "vstack" => TokenKind::Container(Container::VStack),
            "hstack" => TokenKind::Container(Container::HStack),
            "zstack" => TokenKind::Container(Container::ZStack),
            "row" => TokenKind::Container(Container::Row),
            "column" => TokenKind::Container(Container::Column),
            "expand" => TokenKind::Container(Container::Expand),
            "position" => TokenKind::Container(Container::Position),
            "spacer" => TokenKind::Container(Container::Spacer),
            "overflow" => TokenKind::Container(Container::Overflow),
            "padding" => TokenKind::Container(Container::Padding),
            "canvas" => TokenKind::Container(Container::Canvas),
            "container" => TokenKind::Container(Container::Container),
            _ => TokenKind::Identifier((start_byte, end_byte + 1).into()),
        };

        kind.into_token(start_byte, end_byte + 1)
    }

    fn lex_number(&mut self, start_byte: usize) -> Token {
        let mut end_byte = start_byte;
        while let Some((e, '0'..='9' | '.')) = self.chars.peek() {
            end_byte = *e;
            self.chars.next();
        }

        let mut literal = &self.content[start_byte..=end_byte];
        let mut dots = literal.splitn(3, '.');

        if let (Some(one), Some(two), Some(_)) = (dots.next(), dots.next(), dots.next()) {
            // numeric literal contains two or more dots, so we only lex,
            // from the first numerical to the last numerical before the
            // second dot, <one.len()>.<two.len()>
            //
            // 123.456.789 -> 123.456 (leave .789) to be lexed after
            literal = &literal[..one.len() + 1 + two.len()];
        }

        let is_float = literal.contains('.');
        let kind = match is_float {
            true => {
                let literal = literal.parse().expect("failed to parse float literal");
                TokenKind::Primitive(Primitive::Float(literal))
            }
            false => {
                let literal = literal.parse().expect("failed to parse int literal");
                TokenKind::Primitive(Primitive::Int(literal))
            }
        };

        kind.into_token(start_byte, end_byte + 1)
    }

    fn lex_string(&mut self, delimiter: char, start_byte: usize) -> Token {
        loop {
            match self.chars.next() {
                Some((end_byte, next)) if next == delimiter => {
                    // add +1 to include the closing delimiter in the token
                    let end_byte = end_byte + 1;
                    let location = (start_byte, end_byte).into();
                    break TokenKind::String(location).into_token(start_byte, end_byte);
                }
                // Handle escape sequences
                Some((_, '\\')) => {
                    // found escape character, so consume the next character regardless of what it
                    // is. This properly handles \", \', \\, \n, etc.
                    if self.chars.next().is_none() {
                        // if there is no character, the string is unterminated
                        break LexError::UnterminatedString
                            .into_token(start_byte, self.content.len());
                    };
                }
                None => {
                    break LexError::UnterminatedString.into_token(start_byte, self.content.len());
                }
                _ => {}
            }
        }
    }

    fn lex_indent(&mut self, start_byte: usize) -> Token {
        let mut count = 1;

        loop {
            match self.chars.peek() {
                Some((_, next)) if next.is_whitespace() && *next != '\n' => {
                    count += 1;
                    self.chars.next();
                }
                Some(_) | None => break,
            }
        }

        TokenKind::Indent(count).into_token(start_byte, start_byte + count)
    }

    fn lex_hex_value(&mut self, start_byte: usize) -> Token {
        const SHORT: usize = 3;
        const LONG: usize = 6;

        let start_byte = start_byte + 1; // consume #
        let mut end_byte = start_byte;

        while let Some((_, '0'..='9' | 'a'..='f' | 'A'..='F')) = self.chars.peek() {
            self.chars.next();
            end_byte += 1;
        }

        let hex = &self.content[start_byte..end_byte];
        let len = hex.len();
        // Make sure that it's either three or six characters,
        // otherwise it's an invalid hex
        if len != 3 && len != 6 {
            return LexError::InvalidHex.into_token(start_byte, end_byte);
        }

        let kind = match len {
            SHORT => {
                let r = u8::from_str_radix(&hex[0..1], 16).expect("already parsed");
                let r = r << 4 | r;
                let g = u8::from_str_radix(&hex[1..2], 16).expect("already parsed");
                let g = g << 4 | g;
                let b = u8::from_str_radix(&hex[2..3], 16).expect("already parsed");
                let b = b << 4 | b;
                TokenKind::Primitive(Primitive::Hex((r, g, b).into()))
            }
            LONG => {
                let r = u8::from_str_radix(&hex[0..2], 16).expect("already parsed");
                let g = u8::from_str_radix(&hex[2..4], 16).expect("already parsed");
                let b = u8::from_str_radix(&hex[4..6], 16).expect("already parsed");
                TokenKind::Primitive(Primitive::Hex((r, g, b).into()))
            }
            _ => unreachable!(),
        };

        kind.into_token(start_byte - 1, end_byte)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token(TokenKind::Eof, _) => None,
            token => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use aml_core::Location;
    use serde::Serialize;

    use super::*;

    #[derive(Debug, Serialize)]
    struct SnapshotToken<'tok> {
        kind: TokenKind,
        location: Location,
        value: &'tok str,
    }

    impl<'tok> SnapshotToken<'tok> {
        fn from_token(token: Token, content: &'tok str) -> Self {
            let value = &content[token.1.to_range()];

            Self {
                kind: token.0,
                location: token.1,
                value,
            }
        }
    }

    #[test]
    fn test_indentation() {
        let template = r#"
vstack        // 0 spaces
 text "hi"    // 1 space → Indent
   span       // 3 spaces → Indent
 text "bye"   // 1 space → Dedent
"#;

        let tokens = Lexer::new(template)
            .map(|t| SnapshotToken::from_token(t, template))
            .collect::<Vec<_>>();

        insta::assert_yaml_snapshot!(tokens);
    }

    #[test]
    fn test_attributes() {
        let template = r#"
vstack [width: 10, height: 3]
    text [foreground: "red"] "Hello you"
    border [height: -10]
"#;
        let tokens = Lexer::new(template)
            .map(|t| SnapshotToken::from_token(t, template))
            .collect::<Vec<_>>();

        insta::assert_yaml_snapshot!(tokens);
    }

    #[test]
    fn test_local_global() {
        let template = r#"
local my_var = 10
global my_var = 20
"#;
        let tokens = Lexer::new(template)
            .map(|t| SnapshotToken::from_token(t, template))
            .collect::<Vec<_>>();

        insta::assert_yaml_snapshot!(tokens);
    }
}
