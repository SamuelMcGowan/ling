use super::{ParseError, ParseResult, Parser};
use crate::ast::*;
use crate::lexer::token::{tkind, BracketKind, TokenKind};
use crate::syntax::token_stream::TokenTree;

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

    Call,
    Access,
}

impl BinOp {
    fn should_parse_in_prec(&self, prec: Prec) -> bool {
        let op_prec = self.prec();
        let r_assoc = self.r_assoc();

        op_prec > prec || r_assoc && op_prec == prec
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

            Self::Call => Prec::Call,
            Self::Access => Prec::Access,
        }
    }

    fn r_assoc(&self) -> bool {
        matches!(self, Self::Pow)
    }
}

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_prec(Prec::Lowest)
    }

    fn parse_prec(&mut self, prec: Prec) -> ParseResult<Expr> {
        let mut expr = self.parse_lhs()?;

        while let Some(tt) = self.tokens.peek() {
            let Some(op) = self.get_op(tt) else {
                break;
            };

            if !op.should_parse_in_prec(prec) {
                break;
            }

            let tt = self.tokens.next().unwrap();

            expr = match op {
                BinOp::Call => {
                    let TokenTree::Group { tokens, .. } = tt else {
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

                BinOp::Access => {
                    let rhs = self.parse_spanned(|p| p.parse_prec(op.prec()));
                    let rhs_expr = rhs.inner?;

                    if let Expr::Var(Var::Simple(ident)) = rhs_expr {
                        Expr::Var(Var::Field {
                            expr: Box::new(expr),
                            field: ident,
                        })
                    } else {
                        // span is always valid - `parse_prec` always consumes a token
                        self.report(ParseError::InvalidAccessor(rhs.span.unwrap()));
                        Expr::Dummy
                    }
                }

                _ => {
                    let rhs = self.parse_prec(op.prec())?;
                    Expr::BinOp {
                        op,
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    }
                }
            }
        }

        Ok(expr)
    }

    fn parse_lhs(&mut self) -> ParseResult<Expr> {
        match self.tokens.next() {
            Some(TokenTree::Token(token)) => match token.kind {
                TokenKind::Const(idx) => Ok(Expr::Const(idx)),
                TokenKind::Ident(ident) => Ok(Expr::Var(Var::Simple(Ident::Unresolved(ident)))),

                kind if kind == tkind!(kwd If) => self.parse_if_expr(),

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

    fn get_op(&self, tt: &TokenTree) -> Option<BinOp> {
        Some(match tt {
            TokenTree::Token(t) => match t.kind {
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

                t if t == tkind!(punct Dot) => BinOp::Access,

                _ => return None,
            },

            TokenTree::Group {
                bracket_kind: BracketKind::Round,
                ..
            } => BinOp::Call,

            _ => return None,
        })
    }

    fn parse_if_expr(&mut self) -> ParseResult<Expr> {
        let mut branches = vec![IfBranch {
            cond: self.parse_expr()?,
            then: self.parse_block()?,
        }];

        while self.eat_kind(tkind!(kwd Elif)) {
            let cond = self.parse_expr()?;
            let then = self.parse_block()?;
            branches.push(IfBranch { cond, then });
        }

        let else_ = if self.eat_kind(tkind!(kwd Else)) {
            Some(Box::new(self.parse_block()?))
        } else {
            None
        };

        Ok(Expr::If { branches, else_ })
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_ron_snapshot;

    use crate::parser::test_parse;

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

    #[test]
    fn group() {
        assert_ron_snapshot!(test_parse("(1)", |p| p.parse_expr()));
    }

    #[test]
    fn group_err() {
        assert_ron_snapshot!(test_parse("(1 1)", |p| p.parse_expr()));
    }

    #[test]
    fn unit() {
        assert_ron_snapshot!(test_parse("()", |p| p.parse_expr()));
    }

    #[test]
    fn if_expr() {
        assert_ron_snapshot!(test_parse("if a == b { 12 } else { 14 }", |p| p.parse_expr()));
    }

    #[test]
    fn elif() {
        assert_ron_snapshot!(
            test_parse("if a { 12 } elif b { 14 } else { 16 }", |p| p.parse_expr())
        );
    }

    #[test]
    fn trailing_else() {
        assert_ron_snapshot!(test_parse(
            "if a { 12 } elif b { 14 } else { 16 } else { 18 }",
            |p| p.parse_expr()
        ));
    }

    #[test]
    fn add_if() {
        assert_ron_snapshot!(test_parse("if a { 12 } else { 14 } + 2", |p| p.parse_expr()))
    }

    #[test]
    fn method_call() {
        assert_ron_snapshot!(test_parse("a.b().c", |p| p.parse_expr()))
    }

    #[test]
    fn invalid_accessor() {
        assert_ron_snapshot!(test_parse("a.12", |p| p.parse_expr()))
    }
}
