use int_enum::IntEnum;

use crate::syntax::ast::*;
use crate::syntax::token::{tkind, BracketKind, Token, TokenKind};
use crate::syntax::token_stream::TokenTree;

use super::{ParseError, ParseResult, Parser};

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, IntEnum)]
enum Prec {
    Any = 0,

    LogicalOr = 1,
    LogicalAnd = 2,

    Equality = 3,
    Comparison = 4,

    Term = 5,
    Factor = 6,

    Unary = 7,
    Call = 8,
}

impl Prec {
    fn next(self) -> Self {
        Self::from_int(self as u8 + 1).expect("no higher precedence")
    }
}

enum Operator {
    Infix { kind: TokenKind, rhs_prec: Prec },
    Call,
}

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_prec(Prec::Any)
    }

    fn parse_prec(&mut self, prec: Prec) -> ParseResult<Expr> {
        let mut expr = self.parse_lhs()?;

        while let Some(tt) = self.tokens.peek() {
            let Some(op) = self.get_op(tt) else {
                break;
            };

            expr = match op {
                Operator::Infix { kind, rhs_prec } => {
                    if rhs_prec <= prec {
                        break;
                    }
                    self.tokens.next();

                    let rhs = self.parse_prec(rhs_prec)?;
                    Expr::Infix {
                        op: kind,
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }
                }

                Operator::Call => {
                    let TokenTree::Group { tokens, .. } =  self.tokens.next().unwrap() else {
                        unreachable!();
                    };

                    let mut parser = self.parser_for_tokens(tokens.into_iter());

                    let args =
                        parser.parse_list("arguments", tkind!(punct Comma), Parser::parse_expr)?;

                    Expr::Call {
                        callee: Box::new(expr),
                        args,
                    }
                }
            };
        }

        Ok(expr)
    }

    fn parse_lhs(&mut self) -> ParseResult<Expr> {
        match self.tokens.next() {
            Some(TokenTree::Token(Token {
                kind: TokenKind::Const(idx),
                ..
            })) => Ok(Expr::Const(idx)),

            Some(TokenTree::Token(Token {
                kind: TokenKind::Ident(ident),
                ..
            })) => Ok(Expr::Var(Ident::Unresolved(ident))),

            Some(TokenTree::Group {
                bracket_kind: BracketKind::Round,
                tokens,
                ..
            }) => {
                if tokens.is_empty() {
                    Ok(Expr::Unit)
                } else {
                    let mut parser = self.parser_for_tokens(tokens.into_iter());
                    let expr = parser.parse_expr()?;
                    parser.expect_end("parenthesised expression")?;
                    Ok(expr)
                }
            }

            other => Err(ParseError::unexpected("an expression", other)),
        }
    }

    fn get_op(&self, tt: &TokenTree) -> Option<Operator> {
        Some(match tt {
            TokenTree::Token(t) => {
                let rhs_prec = match t.kind {
                    t if t == tkind!(kwd LogicalOr) => Prec::LogicalOr.next(),
                    t if t == tkind!(kwd LogicalAnd) => Prec::LogicalAnd.next(),

                    t if t == tkind!(punct EqualEqual) || t == tkind!(punct BangEqual) => {
                        Prec::Equality.next()
                    }
                    t if t == tkind!(punct Gt)
                        || t == tkind!(punct Lt)
                        || t == tkind!(punct GtEqual)
                        || t == tkind!(punct LtEqual) =>
                    {
                        Prec::Comparison.next()
                    }

                    t if t == tkind!(punct Add) || t == tkind!(punct Sub) => Prec::Term,
                    t if t == tkind!(punct Mul)
                        || t == tkind!(punct Div)
                        || t == tkind!(punct Mod) =>
                    {
                        Prec::Factor
                    }
                    t if t == tkind!(punct Pow) => Prec::Factor,

                    _ => return None,
                };

                Operator::Infix {
                    kind: t.kind,
                    rhs_prec,
                }
            }

            TokenTree::Group {
                bracket_kind: BracketKind::Round,
                ..
            } => Operator::Call,

            _ => return None,
        })
    }
}
