use serde::Serialize;

use crate::constants::ConstIdx;
use crate::syntax::source::Span;

#[derive(Debug, Clone, Copy, Serialize)]
pub(crate) struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub(crate) enum TokenKind {
    Punct(Punct),
    Bracket(Bracket),

    Keyword(Keyword),

    Ident(Ustr),
    Const(ConstIdx),

    Error(Error),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub(crate) enum Punct {
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

    BangEqual,
    Bang,

    FatArrow,
    Arrow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub(crate) enum Bracket {
    Opening(BracketKind),
    Closing(BracketKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub(crate) enum BracketKind {
    Round,
    Curly,
    Square,
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
    Is,

    LogicalAnd,
    LogicalOr,
    LogicalNot,

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
        use crate::lexer::token::*;
        TokenKind::Punct(Punct::$punct)
    }};
    (bracket $side:ident $bracket:ident) => {{
        use crate::lexer::token::*;
        TokenKind::Bracket(Bracket::$side(BracketKind::$bracket))
    }};

    (kwd $kwd:ident) => {{
        use crate::lexer::token::*;
        TokenKind::Keyword(Keyword::$kwd)
    }};

    (ident $ident:expr) => {{
        use ustr::Ustr;
        use crate::lexer::token::*;

        TokenKind::Ident(Ustr::from($ident))
    }};
    (constant $constant:literal) => {{
        use crate::lexer::token::*;
        use crate::constants::ConstIdx;

        TokenKind::Const(ConstIdx($constant))
    }};

    (error $error:ident $($value:tt)?) => {{
        use crate::lexer::token::*;
        TokenKind::Error(Error::$error $($value)?)
    }}
}

pub(crate) use tkind;
use ustr::Ustr;
