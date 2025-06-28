use std::cmp::Ordering;
use std::ops::{Range, RangeBounds};
use std::str::FromStr;

use crate::error::{Error, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Element {
    Text,
    Span,
    Border,
    Alignment,
    VStack,
    HStack,
    ZStack,
    Row,
    Column,
    Expand,
    Position,
    Spacer,
    Overflow,
    Padding,
    Canvas,
    Container,
}

impl FromStr for Element {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "text" => Ok(Self::Text),
            "span" => Ok(Self::Span),
            "border" => Ok(Self::Border),
            "alignment" => Ok(Self::Alignment),
            "vstack" => Ok(Self::VStack),
            "hstack" => Ok(Self::HStack),
            "zstack" => Ok(Self::ZStack),
            "row" => Ok(Self::Row),
            "column" => Ok(Self::Column),
            "expand" => Ok(Self::Expand),
            "position" => Ok(Self::Position),
            "spacer" => Ok(Self::Spacer),
            "overflow" => Ok(Self::Overflow),
            "padding" => Ok(Self::Padding),
            "canvas" => Ok(Self::Canvas),
            "container" => Ok(Self::Container),
            _ => Err("Nuh uh".into()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Keyword {
    For,
    In,
    If,
    Else,
    Let,
    Switch,
    Case,
    Default,
    With,
    As,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    Element(Element),
    Keyword(Keyword),
    Identifier,
    String,
    Newline,
    Indent,
    Dedent,
}

impl IntoToken for TokenKind {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token {
        Token::new(self, (start_byte, end_byte).into())
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    pub start_byte: usize,
    pub end_byte: usize,
}

impl Location {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start_byte: start,
            end_byte: end,
        }
    }
}
impl From<(usize, usize)> for Location {
    fn from((start_byte, end_byte): (usize, usize)) -> Self {
        Self {
            start_byte,
            end_byte,
        }
    }
}

impl From<Range<usize>> for Location {
    fn from(range: Range<usize>) -> Self {
        let start = match range.start_bound() {
            std::ops::Bound::Included(start) => *start,
            std::ops::Bound::Excluded(start) => *start,
            std::ops::Bound::Unbounded => {
                panic!("can only construct a location from bounded ranges")
            }
        };

        let end = match range.end_bound() {
            std::ops::Bound::Included(end) => *end,
            std::ops::Bound::Excluded(end) => *end,
            std::ops::Bound::Unbounded => {
                panic!("can only construct a location from bounded ranges")
            }
        };

        Location {
            start_byte: start,
            end_byte: end,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Location,
}

impl Token {
    pub fn new(kind: TokenKind, location: Location) -> Self {
        Self { kind, location }
    }
}

pub struct Lexer<'lex> {
    cursor: usize,
    slice: &'lex str,
    content: &'lex str,
    current_indent: usize,
}

pub trait IntoToken {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token;
}

impl<'lex> Lexer<'lex> {
    pub fn new(content: &'lex str) -> Self {
        Self {
            slice: content,
            content,
            cursor: 0,
            current_indent: 0,
        }
    }

    fn advance_by(&mut self, amount: usize) {
        self.slice = &self.slice[amount..];
        self.cursor += amount;
    }

    fn make_token<T>(&mut self, tokenizable: T, size: usize) -> Token
    where
        T: IntoToken,
    {
        let start_byte = self.cursor;
        self.advance_by(size);
        tokenizable.into_token(start_byte, self.cursor)
    }

    fn lex_string(&mut self) -> Token {
        let start_byte = self.cursor;
        self.advance_by(1);
        let end_of_string = self.slice.find(['"', '\n']).unwrap_or(self.slice.len());

        self.cursor += end_of_string;
        self.slice = &self.slice[end_of_string + 1..];

        TokenKind::String.into_token(start_byte, self.cursor)
    }

    fn lex_identifier(&mut self) -> Token {
        let next_whitespace = self
            .slice
            .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
            .unwrap_or(self.slice.len());

        let identifier = &self.slice[..next_whitespace];

        match Element::from_str(identifier) {
            Ok(element) => self.make_token(TokenKind::Element(element), next_whitespace),
            Err(_) => self.make_token(TokenKind::Identifier, next_whitespace),
        }
    }

    fn handle_indentation(&mut self, new_indent: usize) -> Option<Token> {
        let current_indent = self.current_indent;
        self.current_indent = new_indent;

        match new_indent.cmp(&current_indent) {
            Ordering::Greater => Some(TokenKind::Indent.into_token(self.cursor, self.cursor)),
            Ordering::Less => Some(TokenKind::Dedent.into_token(self.cursor, self.cursor)),
            Ordering::Equal => None,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut chars = self.slice.chars().peekable();
            let curr = chars.next()?;
            let next = chars.peek();

            break match (curr, next) {
                ('\n', _) => {
                    let start_byte = self.cursor;
                    self.advance_by(1);

                    let leading_spaces = self.slice.chars().take_while(|&c| c == ' ').count();
                    self.advance_by(leading_spaces);

                    let newline_token = TokenKind::Newline.into_token(start_byte, start_byte + 1);

                    match self.handle_indentation(leading_spaces) {
                        Some(indent_token) => Some(Ok(indent_token)),
                        None => Some(Ok(newline_token)),
                    }
                }
                (c, _) if c.is_whitespace() => {
                    self.advance_by(c.len_utf8());
                    continue;
                }

                ('/', Some('/')) => {
                    let eol_location = self.slice.find('\n').unwrap_or(self.slice.len());
                    self.advance_by(eol_location);
                    continue;
                }

                ('"', _) => Some(Ok(self.lex_string())),

                ('a'..='z' | 'A'..='Z' | '_', _) => Some(Ok(self.lex_identifier())),
                _ => todo!(),
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let template = r#"
vstack        // 0 spaces
 text "hi"    // 1 space → Indent
   span       // 3 spaces → Indent
 text "bye"   // 1 space → Dedent
"#;

        let tokens = Lexer::new(template).map(|t| t.unwrap()).collect::<Vec<_>>();

        println!("{tokens:#?}");
        panic!();
    }
}
