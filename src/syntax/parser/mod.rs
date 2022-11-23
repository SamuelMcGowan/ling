use super::token::{Token, TokenKind};
use super::ParseContext;

impl<'a> ParseContext<'a> {
    pub fn parser(&mut self) -> Parser<'_, 'a> {
        let tokens: Vec<_> = self.lexer().collect();
        Parser::new(tokens, self)
    }
}

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

pub(crate) struct Parser<'ctx, 'a> {
    tokens: TokenIter,
    context: &'ctx ParseContext<'a>,
}

impl<'ctx, 'a> Parser<'ctx, 'a> {
    pub fn new(tokens: Vec<Token>, context: &'ctx ParseContext<'a>) -> Self {
        Self {
            tokens: TokenIter::new(tokens),
            context,
        }
    }
}
