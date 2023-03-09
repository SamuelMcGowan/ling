mod ast;
mod item;
mod token_iter;

use self::ast::*;
use self::token_iter::TokenIter;

use super::token::{tkind, Token, TokenKind};

#[derive(Debug, Clone, serde::Serialize)]
pub(crate) enum ParseError {
    Unexpected {
        expected: String,
        found: Option<Token>,
    },
}
pub(crate) type ParseResult<T> = Result<T, ParseError>;

impl ParseError {
    fn unexpected(expected: impl Into<String>, found: Option<Token>) -> Self {
        Self::Unexpected {
            expected: expected.into(),
            found,
        }
    }
}

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
}

// Error handling/recovery.
impl Parser {
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

// Common parse functions.
impl Parser {
    fn parse_ident(&mut self) -> ParseResult<Ident> {
        match self.tokens.next() {
            Some(Token {
                kind: TokenKind::Ident(s),
                ..
            }) => Ok(Ident::Unresolved(s)),

            other => Err(ParseError::Unexpected {
                expected: "an identifier".to_string(),
                found: other,
            }),
        }
    }

    fn parse_ty(&mut self) -> ParseResult<Ty> {
        match self.tokens.next() {
            Some(Token {
                kind: TokenKind::Ident(s),
                ..
            }) => {
                let params = if self.tokens.eat(tkind!(punct LBracket)) {
                    let params = self.parse_list(tkind!(punct RBracket), Self::parse_ty)?;
                    self.tokens.expect(tkind!(punct RBracket))?;
                    params
                } else {
                    vec![]
                };

                Ok(Ty::Constructed {
                    ident: Ident::Unresolved(s),
                    params,
                })
            }

            Some(token) if token.kind == tkind!(punct LParen) => {
                self.tokens.expect(tkind!(punct RParen))?;
                Ok(Ty::Unit)
            }

            other => Err(ParseError::unexpected("a type", other)),
        }
    }

    fn parse_block(&mut self) -> ParseResult<Expr> {
        self.tokens.expect(tkind!(punct LBrace))?;
        self.tokens.expect(tkind!(punct RBrace))?;
        Ok(Expr::Unit)
    }

    fn parse_list<T>(
        &mut self,
        end: TokenKind,
        mut parse: impl FnMut(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<Vec<T>> {
        let mut items = vec![];

        while !self.tokens.matches(end) {
            items.push(parse(self)?);

            if !self.tokens.eat(tkind!(punct Comma)) {
                break;
            }
        }

        Ok(items)
    }
}
