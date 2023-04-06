use crate::syntax::ast::*;
use crate::syntax::token::{tkind, BracketKind, TokenKind};
use crate::syntax::token_stream::TokenTree;

use super::{ParseError, ParseResult, Parser};

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
    Lowest = 0,

    LogicalOr,
    LogicalAnd,

    Equality,
    Comparison,

    Term,
    Factor,
    Exp,

    Unary,
}

impl BinOp {
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

enum RhsRule {
    BinOp(BinOp),
    Call,
}

impl RhsRule {
    fn should_parse_in_prec(&self, prec: Prec) -> bool {
        match self {
            Self::BinOp(op) => {
                let op_prec = op.prec();
                let r_assoc = op.r_assoc();

                op_prec > prec || r_assoc && op_prec == prec
            }
            Self::Call => true,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_prec(Prec::Lowest)
    }

    fn parse_prec(&mut self, prec: Prec) -> ParseResult<Expr> {
        let mut expr = self.parse_lhs()?;

        while let Some(tt) = self.tokens.peek() {
            let Some(rhs_rule) = self.get_rhs_rule(tt) else {
                break;
            };

            if !rhs_rule.should_parse_in_prec(prec) {
                break;
            }

            let tt = self.tokens.next().unwrap();

            expr = match rhs_rule {
                RhsRule::BinOp(op) => {
                    let rhs = self.parse_prec(op.prec())?;
                    Expr::BinOp {
                        op,
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }
                }

                RhsRule::Call => {
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
            Some(TokenTree::Token(token)) => match token.kind {
                TokenKind::Const(idx) => Ok(Expr::Const(idx)),
                TokenKind::Ident(ident) => Ok(Expr::Var(Ident::Unresolved(ident))),

                kind if kind == tkind!(punct Sub) => {
                    let expr = self.parse_prec(Prec::Unary)?;
                    Ok(Expr::UnaryOp {
                        op: UnaryOp::Neg,
                        expr: Box::new(expr),
                    })
                }

                kind if kind == tkind!(punct Bang) => {
                    let expr = self.parse_prec(Prec::Unary)?;
                    Ok(Expr::UnaryOp {
                        op: UnaryOp::Not,
                        expr: Box::new(expr),
                    })
                }

                _ => Err(ParseError::unexpected(
                    "an expression",
                    Some(TokenTree::Token(token)),
                )),
            },

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

    fn get_rhs_rule(&self, tt: &TokenTree) -> Option<RhsRule> {
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

                RhsRule::BinOp(bin_op)
            }

            TokenTree::Group {
                bracket_kind: BracketKind::Round,
                ..
            } => RhsRule::Call,

            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_ron_snapshot;

    use crate::syntax::parser::test_parse;

    #[test]
    fn l_assoc() {
        assert_ron_snapshot!(test_parse("1 + 2 + 3", |p| p.parse_expr()));
    }

    #[test]
    fn r_assoc() {
        assert_ron_snapshot!(test_parse("1 ^ 2 ^ 3", |p| p.parse_expr()));
    }

    #[test]
    fn mul_add() {
        assert_ron_snapshot!(test_parse("1 * 2 + 3", |p| p.parse_expr()));
    }

    #[test]
    fn add_mul() {
        assert_ron_snapshot!(test_parse("1 + 2 * 3", |p| p.parse_expr()));
    }

    #[test]
    fn mul_pow() {
        assert_ron_snapshot!(test_parse("1 * 2 ^ 3", |p| p.parse_expr()));
    }

    #[test]
    fn pow_mul() {
        assert_ron_snapshot!(test_parse("1 ^ 2 * 3", |p| p.parse_expr()));
    }
}
