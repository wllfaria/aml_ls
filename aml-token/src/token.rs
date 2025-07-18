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
    Identifier(Location),
    String(Location),
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::In => write!(f, "in"),
            TokenKind::If => write!(f, "if"),
            TokenKind::As => write!(f, "as"),
            TokenKind::Equal => write!(f, "="),
            TokenKind::For => write!(f, "for"),
            TokenKind::Eof => write!(f, "EOF"),
            TokenKind::Decl => write!(f, "let"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Case => write!(f, "case"),
            TokenKind::With => write!(f, "with"),
            TokenKind::Component => write!(f, "@"),
            TokenKind::Local => write!(f, "local"),
            TokenKind::Global => write!(f, "global"),
            TokenKind::Switch => write!(f, "switch"),
            TokenKind::Error(_) => write!(f, "error"),
            TokenKind::Default => write!(f, "default"),
            TokenKind::ComponentSlot => write!(f, "$"),
            TokenKind::Newline => write!(f, "newline"),
            TokenKind::Indent(_) => write!(f, "indent"),
            TokenKind::String(_) => write!(f, "string"),
            TokenKind::Element(_) => write!(f, "element"),
            TokenKind::Operator(_) => write!(f, "operator"),
            TokenKind::Container(_) => write!(f, "container"),
            TokenKind::Primitive(_) => write!(f, "primitive"),
            TokenKind::Identifier(_) => write!(f, "identifier"),
        }
    }
}

impl TokenKind {
    pub fn expect_container(self) -> Container {
        match self {
            TokenKind::Container(container) => container,
            _ => panic!("expected container, got: {self:?}"),
        }
    }

    pub fn is_declaration(self) -> bool {
        matches!(self, TokenKind::Decl | TokenKind::Global | TokenKind::Local)
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
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl std::fmt::Display for Hex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02x}{:02x}{:02x}", self.r, self.g, self.b)
    }
}

impl From<(u8, u8, u8)> for Hex {
    fn from((r, g, b): (u8, u8, u8)) -> Self {
        Self { r, g, b }
    }
}

impl TryFrom<&str> for Hex {
    type Error = ();

    fn try_from(hex: &str) -> Result<Self, Self::Error> {
        if hex.is_empty() || !hex.starts_with("#") {
            return Err(());
        }

        let hex = &hex[1..];
        match hex.len() {
            3 => {
                let r = u8::from_str_radix(&hex[0..1], 16).map_err(|_| ())?;
                let r = r << 4 | r;
                let g = u8::from_str_radix(&hex[1..2], 16).map_err(|_| ())?;
                let g = g << 4 | g;
                let b = u8::from_str_radix(&hex[2..3], 16).map_err(|_| ())?;
                let b = b << 4 | b;
                Ok(Self::from((r, g, b)))
            }
            6 => {
                let r = u8::from_str_radix(&hex[0..2], 16).map_err(|_| ())?;
                let g = u8::from_str_radix(&hex[2..4], 16).map_err(|_| ())?;
                let b = u8::from_str_radix(&hex[4..6], 16).map_err(|_| ())?;
                Ok(Self::from((r, g, b)))
            }
            _ => Err(()),
        }
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
