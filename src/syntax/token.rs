use serde::{Deserialize, Serialize};

use crate::constants::ConstIdx;

use super::source::Span;

#[derive(Debug, Clone, Copy, Serialize)]
pub(crate) struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub(crate) enum TokenKind {
    Punct(Punct),
    Keyword(Keyword),

    Ident(Ustr),
    Const(ConstIdx),

    Error(Error),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub(crate) enum Punct {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub(crate) enum Keyword {
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

    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub(crate) enum Error {
    UnexpectedChar(char),

    InvalidEscape,
    UnterminatedString,

    IntegerOverflow,
    MissingDigits,
}

macro_rules! tkind {
    (punct $punct:ident) => {{
        use crate::syntax::token::*;
        TokenKind::Punct(Punct::$punct)
    }};
    (kwd $kwd:ident) => {{
        use crate::syntax::token::*;
        TokenKind::Keyword(Keyword::$kwd)
    }};

    (ident $ident:expr) => {{
        use ustr::Ustr;
        use crate::syntax::token::*;

        TokenKind::Ident(Ustr::from($ident))
    }};
    (constant $constant:literal) => {{
        use crate::syntax::token::*;
        use crate::constants::ConstIdx;

        TokenKind::Const(ConstIdx($constant))
    }};

    (error $error:ident $($value:tt)?) => {{
        use crate::syntax::token::*;
        TokenKind::Error(Error::$error $($value)?)
    }}
}

pub(crate) use tkind;
use ustr::Ustr;
