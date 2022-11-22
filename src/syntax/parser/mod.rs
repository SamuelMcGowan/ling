use std::iter::Peekable;
use std::vec::IntoIter;

use super::token::{Token, TokenKind};
use super::ParseContext;

impl<'a> ParseContext<'a> {
    pub fn parser(&mut self) -> Parser<'_, 'a> {
        // Clippy thinks this is avoidable.
        // It probably is, but it's a lot of work to avoid one allocation.
        #[allow(clippy::needless_collect)]
        let tokens: Vec<_> = self.lexer().collect();

        Parser {
            tokens: tokens.into_iter().peekable(),
            context: self,
        }
    }
}

pub(crate) struct Parser<'ctx, 'a> {
    tokens: Peekable<IntoIter<Token>>,
    context: &'ctx ParseContext<'a>,
}

impl Parser<'_, '_> {
    fn next(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<Token> {
        self.tokens.peek().copied()
    }

    fn eat_kind(&mut self, kind: TokenKind) -> bool {
        match self.peek() {
            Some(token) if token.kind == kind => {
                self.tokens.next();
                true
            }
            _ => false,
        }
    }

    fn eat_while(&mut self, mut predicate: impl FnMut(TokenKind) -> bool) {
        while matches!(self.peek(), Some(token) if predicate(token.kind)) {
            self.next();
        }
    }
}
