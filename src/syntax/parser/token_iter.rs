use crate::syntax::parser::{ParseError, ParseResult};
use crate::syntax::token::{Token, TokenKind};

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

    pub fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    pub fn peek(&self) -> Option<Token> {
        self.tokens.get(self.pos).copied()
    }

    pub fn peek_far(&self, n: usize) -> Option<Token> {
        self.tokens.get(self.pos + n).copied()
    }

    pub fn matches(&self, kind: TokenKind) -> bool {
        matches!(self.peek(), Some(token) if token.kind == kind)
    }

    pub fn eat(&mut self, kind: TokenKind) -> bool {
        self.next_if(|token| token.kind == kind)
    }

    pub fn expect(&mut self, kind: TokenKind) -> ParseResult<Token> {
        match self.peek() {
            Some(token) if token.kind == kind => {
                self.next();
                Ok(token)
            }
            other => Err(ParseError::Unexpected {
                expected: format!("token of kind {kind:?}"),
                found: other,
            }),
        }
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
