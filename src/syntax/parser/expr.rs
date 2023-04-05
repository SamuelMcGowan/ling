use crate::syntax::ast::*;
use crate::syntax::token::{tkind, BracketKind, Token, TokenKind};
use crate::syntax::token_stream::TokenTree;

use super::{ParseError, ParseResult, Parser};

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
    Any = 0,

    LogicalOr,
    LogicalAnd,

    Equality,
    Comparison,

    Term,
    Factor,
    Exp,

    Unary,
    Call,
}

impl BinOp {
    fn should_parse_in_prec(&self, prec: Prec) -> bool {
        let op_prec = self.prec();
        op_prec > prec || self.r_assoc() && op_prec == prec
    }

    fn prec(&self) -> Prec {
        match self {
            Self::LogicalOr => Prec::LogicalOr,
            Self::LogicalAnd => Prec::LogicalAnd,

            Self::Equal | Self::NotEqual => Prec::Equality,
            Self::Gt | Self::Lt | Self::GtEqual | Self::LtEqual => Prec::Comparison,

            Self::Add | Self::Sub => Prec::Term,
            Self::Mul | Self::Div | Self::Mod => Prec::Factor,
            Self::Pow => Prec::Exp,
        }
    }

    fn r_assoc(&self) -> bool {
        matches!(self, Self::Pow)
    }
}

enum Operator {
    BinOp(BinOp),
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

            if matches!(op, Operator::BinOp(op) if !op.should_parse_in_prec(prec)) {
                break;
            }

            let tt = self.tokens.next().unwrap();

            expr = match op {
                Operator::BinOp(op) => {
                    let rhs = self.parse_prec(op.prec())?;
                    Expr::Infix {
                        op,
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }
                }

                Operator::Call => {
                    let TokenTree::Group { tokens, .. } =  tt else {
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
                let bin_op = match t.kind {
                    t if t == tkind!(kwd LogicalOr) => BinOp::LogicalOr,
                    t if t == tkind!(kwd LogicalAnd) => BinOp::LogicalAnd,

                    t if t == tkind!(punct EqualEqual) => BinOp::Equal,
                    t if t == tkind!(punct BangEqual) => BinOp::NotEqual,

                    t if t == tkind!(punct Gt) => BinOp::Gt,
                    t if t == tkind!(punct Lt) => BinOp::Lt,
                    t if t == tkind!(punct GtEqual) => BinOp::GtEqual,
                    t if t == tkind!(punct LtEqual) => BinOp::LtEqual,

                    t if t == tkind!(punct Add) => BinOp::Add,
                    t if t == tkind!(punct Sub) => BinOp::Sub,

                    t if t == tkind!(punct Mul) => BinOp::Mul,
                    t if t == tkind!(punct Div) => BinOp::Div,
                    t if t == tkind!(punct Mod) => BinOp::Mod,
                    t if t == tkind!(punct Pow) => BinOp::Pow,

                    _ => return None,
                };

                Operator::BinOp(bin_op)
            }

            TokenTree::Group {
                bracket_kind: BracketKind::Round,
                ..
            } => Operator::Call,

            _ => return None,
        })
    }
}
