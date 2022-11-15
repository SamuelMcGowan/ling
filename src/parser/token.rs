use super::span::Span;

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Punct(Punct),
    Keyword(Keyword),
    Ident(Ustr),
    Error(Error),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Punct {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Dot,
    Comma,
    Colon,
    Semicolon,

    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    AddEqual,
    SubEqual,
    MulEqual,
    DivEqual,
    ModEqual,
    PowEqual,

    Gt,
    Lt,
    Equal,

    GtEqual,
    LtEqual,
    EqualEqual,

    FatArrow,
    Arrow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Func,
    Return,

    Type,
    Enum,
    Data,
    Typeclass,
    Impl,

    Let,
    Match,

    If,
    Elif,
    Else,

    Loop,
    While,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    UnexpectedChar(char),
}

macro_rules! tkind {
    (punct $punct:ident) => {{
        use crate::parser::token::*;
        TokenKind::Punct(Punct::$punct)
    }};
    (kwd $kwd:ident) => {{
        use crate::parser::token::*;
        TokenKind::Keyword(Keyword::$kwd)
    }};
    (ident $ident:expr) => {{
        use ustr::Ustr;
        use crate::parser::token::*;

        TokenKind::Ident(Ustr::from($ident))
    }};
    (error $error:ident $($value:tt)?) => {{
        use crate::parser::token::*;
        TokenKind::Error(Error::$error $($value)?)
    }}
}

pub(crate) use tkind;
use ustr::Ustr;
