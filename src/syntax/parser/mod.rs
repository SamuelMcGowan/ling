pub mod item;

use super::token::{BracketKind, Token, TokenKind};
use super::token_stream::{TokenIter, TokenTree};

#[derive(Debug, Clone, serde::Serialize)]
pub(crate) enum ParseError {
    Unexpected {
        expected: String,
        found: Option<TokenTree>,
    },
}

impl ParseError {
    fn unexpected(expected: impl Into<String>, found: Option<TokenTree>) -> Self {
        Self::Unexpected {
            expected: expected.into(),
            found,
        }
    }
}

pub(crate) type ParseResult<T> = Result<T, ParseError>;

pub(crate) struct Parser<'a> {
    errors: &'a mut Vec<ParseError>,
    tokens: TokenIter,
}

// Utilities.
impl<'a> Parser<'a> {
    pub fn new(tokens: TokenIter, errors: &'a mut Vec<ParseError>) -> Self {
        Self { tokens, errors }
    }

    fn parser_for_tokens(&mut self, tokens: TokenIter) -> Parser {
        Parser {
            errors: self.errors,
            tokens,
        }
    }

    fn next_is_group(&self, kind: BracketKind) -> bool {
        matches!(self.tokens.peek(), Some(TokenTree::Group { bracket_kind, ..}) if *bracket_kind == kind)
    }

    fn eat_kind(&mut self, kind: TokenKind) -> bool {
        match self.tokens.peek() {
            Some(TokenTree::Token(token)) if token.kind == kind => {
                self.tokens.next();
                true
            }
            _ => false,
        }
    }

    fn expect_kind(&mut self, kind: TokenKind) -> ParseResult<Token> {
        match self.tokens.next() {
            Some(TokenTree::Token(token)) if token.kind == kind => Ok(token),
            other => Err(ParseError::unexpected(
                format!("token of kind {kind:?}"),
                other,
            )),
        }
    }

    fn expect_group(
        &mut self,
        kind: BracketKind,
        expected: impl Into<String>,
    ) -> ParseResult<TokenIter> {
        match self.tokens.next() {
            Some(TokenTree::Group {
                bracket_kind,
                tokens,
            }) if bracket_kind == kind => Ok(tokens.into_iter()),
            other => Err(ParseError::unexpected(expected.into(), other)),
        }
    }

    fn expect_end(&mut self, tree_name: &str) -> ParseResult<()> {
        match self.tokens.next() {
            None => Ok(()),
            Some(tree) => Err(ParseError::unexpected(
                format!("end of {tree_name}"),
                Some(tree),
            )),
        }
    }
}

// Error handling/recovery.
impl Parser<'_> {
    fn parse_or_recover<T>(
        &mut self,
        parser: impl Fn(&mut Self) -> ParseResult<T>,
        recover: impl Fn(&mut Self) -> T,
    ) -> T {
        parser(self).unwrap_or_else(|error| {
            self.report(error);
            recover(self)
        })
    }

    fn recover_past(&mut self, kind: TokenKind) {
        self.tokens
            .by_ref()
            .take_while(|token| !matches!(token, TokenTree::Token(token) if token.kind == kind))
            .for_each(|_| {});
    }

    fn report(&mut self, error: ParseError) {
        self.errors.push(error);
    }
}
