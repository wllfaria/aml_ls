use aml_core::Location;
use serde::Serialize;

pub trait IntoToken {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token;
}

#[derive(Debug, Clone, Serialize, Copy, PartialEq, PartialOrd)]
pub enum TokenKind {
    Equal,
    For,
    In,
    If,
    Else,
    Switch,
    Case,
    Default,
    With,
    As,
    Component,
    ComponentSlot,
    Decl,
    Global,
    Local,
    Eof,

    Newline,
    Indent(usize),
    Operator(Operator),
    Element(Element),
    Container(Container),
    Error(LexError),
    Primitive(Primitive),
    // TODO: store the string
    Identifier(Location),
    String(Location),
}

impl TokenKind {
    pub fn expect_container(self) -> Container {
        match self {
            TokenKind::Container(container) => container,
            _ => panic!("expected container, got: {self:?}"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Copy, PartialEq, PartialOrd)]
pub enum Primitive {
    Bool(bool),
    Hex(Hex),
    Int(i64),
    Float(f64),
}

impl IntoToken for TokenKind {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token {
        Token(self, (start_byte, end_byte).into())
    }
}

#[derive(Debug, Clone, Serialize, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Element {
    Text,
    Span,
}

#[derive(Debug, Clone, Serialize, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Container {
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

#[derive(Debug, Clone, Serialize, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    LBracket,
    RBracket,
    LParen,
    RParen,
    Association,
    LCurly,
    RCurly,
    Mul,
    Div,
    Mod,
    PlusEqual,
    MinusEqual,
    MulEqual,
    DivEqual,
    ModEqual,
    Colon,
    Comma,
    Plus,
    Minus,
    Dot,
    Not,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    EqualEqual,
    NotEqual,
    Or,
    And,
    Either,
}

impl IntoToken for Operator {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token {
        Token(TokenKind::Operator(self), (start_byte, end_byte).into())
    }
}

#[derive(Debug, Clone, Serialize, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LexError {
    UnterminatedString,
    InvalidHex,
}

impl IntoToken for LexError {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token {
        Token(TokenKind::Error(self), (start_byte..end_byte).into())
    }
}

#[derive(Debug, Clone, Serialize, Copy, PartialEq, PartialOrd)]
pub struct Hex {
    r: u8,
    g: u8,
    b: u8,
}

impl From<(u8, u8, u8)> for Hex {
    fn from((r, g, b): (u8, u8, u8)) -> Self {
        Self { r, g, b }
    }
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq, PartialOrd)]
pub struct Token(pub TokenKind, pub Location);

impl Token {
    pub fn kind(&self) -> TokenKind {
        self.0
    }

    pub fn location(&self) -> Location {
        self.1
    }
}

#[derive(Debug)]
pub struct Tokens {
    pub inner: Vec<Token>,
    pub index: usize,
    pub eof: usize,
}

impl Tokens {
    pub fn new(inner: Vec<Token>, eof: usize) -> Self {
        Self {
            inner,
            index: 0,
            eof,
        }
    }

    pub fn next_token(&mut self) -> Token {
        match self.inner.get(self.index).copied() {
            Some(token) => {
                self.index += 1;
                token
            }
            None => TokenKind::Eof.into_token(self.eof - 1, self.eof),
        }
    }

    pub fn consume(&mut self) {
        self.index += 1;
    }

    pub fn next_no_indent(&mut self) -> Token {
        loop {
            let token = self.next_token();

            if let TokenKind::Indent(_) = token.kind() {
                continue;
            }

            break token;
        }
    }

    pub fn peek(&self) -> Token {
        self.inner.get(self.index).copied().unwrap_or(Token(
            TokenKind::Eof,
            (self.eof.saturating_sub(1)..self.eof).into(),
        ))
    }

    pub fn consume_indent(&mut self) {
        loop {
            if matches!(
                self.inner.get(self.index),
                Some(Token(TokenKind::Indent(_), _))
            ) {
                self.index += 1;
                continue;
            }
            break;
        }
    }

    pub fn consume_newlines(&mut self) {
        loop {
            if matches!(
                self.inner.get(self.index),
                Some(Token(TokenKind::Newline, _))
            ) {
                self.index += 1;
                continue;
            }
            break;
        }
    }

    pub fn consume_all_whitespace(&mut self) {
        loop {
            if matches!(
                self.inner.get(self.index).map(|t| t.0),
                Some(TokenKind::Indent(_))
            ) {
                self.index += 1;
                continue;
            }

            if matches!(
                self.inner.get(self.index).map(|t| t.0),
                Some(TokenKind::Newline)
            ) {
                self.index += 1;
                continue;
            }

            break;
        }
    }

    pub fn peek_skip_indent(&mut self) -> Token {
        loop {
            let token = self.peek();

            if let TokenKind::Indent(_) = token.kind() {
                self.index += 1;
                continue;
            }

            break token;
        }
    }

    pub fn read_indent(&mut self) -> Option<usize> {
        match self.peek().kind() {
            TokenKind::Indent(indent) => {
                self.consume();
                Some(indent)
            }
            _ => None,
        }
    }
}
