mod ast;
mod token_iter;

use self::token_iter::TokenIter;

use super::token::{Token, TokenKind};

pub(crate) enum ParseError {}
pub(crate) type ParseResult<T> = Result<T, ParseError>;

pub(crate) struct Parser {
    tokens: TokenIter,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: TokenIter::new(tokens),
            errors: vec![],
        }
    }

    fn parse_or_recover<T>(
        &mut self,
        mut parser: impl FnMut(&mut Self) -> ParseResult<T>,
        mut recover: impl FnMut(&mut Self) -> T,
    ) -> T {
        parser(self).unwrap_or_else(|error| {
            self.report(error);
            recover(self)
        })
    }

    fn recover_until(&mut self, kinds: &[TokenKind]) {
        self.tokens.next_while(|token| !kinds.contains(&token.kind));
    }

    fn recover_past(&mut self, kinds: &[TokenKind]) {
        self.tokens
            .by_ref()
            .take_while(|token| !kinds.contains(&token.kind))
            .for_each(|_| {});
    }

    fn report(&mut self, error: ParseError) {
        self.errors.push(error);
    }
}
