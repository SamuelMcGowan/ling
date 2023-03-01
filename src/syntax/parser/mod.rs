mod ast;

use super::token::{Token, TokenKind};

pub(crate) struct TokenIter {
    tokens: Vec<Token>,
    pos: usize,
}

impl Iterator for TokenIter {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.peek();
        self.pos += 1;
        token
    }
}

impl TokenIter {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn peek(&self) -> Option<Token> {
        self.tokens.get(self.pos).copied()
    }

    pub fn peek_far(&self, n: usize) -> Option<Token> {
        self.tokens.get(self.pos + n).copied()
    }

    pub fn eat(&mut self, kind: TokenKind) -> bool {
        self.next_if(|token| token.kind == kind)
    }

    pub fn next_if(&mut self, mut predicate: impl FnMut(Token) -> bool) -> bool {
        if matches!(self.peek(), Some(token) if predicate(token)) {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn next_while(&mut self, mut predicate: impl FnMut(Token) -> bool) {
        while matches!(self.peek(), Some(token) if predicate(token)) {
            self.next();
        }
    }
}

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
