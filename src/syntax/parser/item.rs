use crate::syntax::ast::*;
use crate::syntax::token::tkind;

use super::*;

impl Parser<'_> {
    pub fn parse_module(&mut self) -> ParseResult<Module> {
        let mut items = vec![];

        while !self.tokens.at_end() {
            let item = self.parse_item_or_recover();
            items.push(item);
        }

        Ok(Module { items })
    }

    fn parse_item_or_recover(&mut self) -> Item {
        self.parse_or_recover(Self::parse_item, |parser| {
            parser.recover_until(&[tkind!(kwd Func)]);
            Item::Dummy
        })
    }

    fn parse_item(&mut self) -> ParseResult<Item> {
        match self.tokens.next() {
            Some(TokenTree::Token(token)) if token.kind == tkind!(kwd Func) => {
                Ok(Item::Func(self.parse_func()?))
            }
            other => Err(ParseError::unexpected("an item", other)),
        }
    }

    fn parse_func(&mut self) -> ParseResult<Func> {
        let ident = self.parse_ident()?;

        let params = {
            let tokens = self.expect_group(BracketKind::Round, "parameters list")?;
            let mut parser = self.parser_for_tokens(tokens);

            parser.parse_list("function arguments", tkind!(punct Comma), |parser| {
                let ident = parser.parse_ident()?;
                parser.expect_kind(tkind!(punct Colon))?;
                let ty = parser.parse_ty()?;
                Ok((ident, ty))
            })?
        };

        let ret_ty = if self.eat_kind(tkind!(punct Arrow)) {
            self.parse_ty()?
        } else {
            Ty::Unit
        };

        let body = self.parse_block()?;

        Ok(Func {
            ident,
            params,
            ret_ty,
            body,
        })
    }

    fn parse_ident(&mut self) -> ParseResult<Ident> {
        match self.tokens.next() {
            Some(TokenTree::Token(Token {
                kind: TokenKind::Ident(ident),
                ..
            })) => Ok(Ident::Unresolved(ident)),
            other => Err(ParseError::unexpected("an identifier", other)),
        }
    }

    fn parse_ty(&mut self) -> ParseResult<Ty> {
        match self.tokens.next() {
            Some(TokenTree::Token(Token {
                kind: TokenKind::Ident(ident),
                ..
            })) => {
                let params = if let Some(tokens) = self.eat_group(BracketKind::Square) {
                    let mut parser = self.parser_for_tokens(tokens);
                    parser.parse_list("type parameters", tkind!(punct Comma), Parser::parse_ty)?
                } else {
                    vec![]
                };

                Ok(Ty::Constructed {
                    ident: Ident::Unresolved(ident),
                    params,
                })
            }

            Some(TokenTree::Group {
                bracket_kind: BracketKind::Round,
                tokens,
                ..
            }) if tokens.is_empty() => Ok(Ty::Unit),

            other => Err(ParseError::unexpected("a type", other)),
        }
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        let tokens = self.expect_group(BracketKind::Curly, "block")?;
        let mut parser = self.parser_for_tokens(tokens);

        let mut stmts = vec![];

        let mut eval_as_final = false;

        while !parser.tokens.at_end() {
            let stmt = parser.parse_stmt_or_recover();

            let expect_delim = stmt.expect_delim();
            let found_delim = parser.eat_kind(tkind!(punct Semicolon));

            stmts.push(stmt);

            if expect_delim && !found_delim {
                // peek so that we can carry on parsing statements
                if let Some(tt) = parser.tokens.peek() {
                    parser.report(ParseError::unexpected("semicolon", Some(tt.clone())));
                }
                // don't break, we want to keep parsing
            }

            eval_as_final = !found_delim;
        }

        // Guards against accidentally using `self` instead of `parser`.
        drop(parser);

        Ok(Block {
            stmts,
            eval_as_final,
        })
    }

    fn parse_stmt_or_recover(&mut self) -> Stmt {
        self.parse_or_recover(Self::parse_stmt, |parser| {
            // Recover *past* the semicolon so that we don't have to
            // guess whether or not to expect a semicolon in `parse_block`.

            // We don't recover up to any keywords in case they appear in the statement.
            // This could change if we later support top-items inside blocks, since these can't
            // appear inside expressions (except inside groups).

            parser.recover_past(tkind!(punct Semicolon));

            Stmt::Dummy
        })
    }

    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.parse_expr()?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        match self.tokens.next() {
            Some(TokenTree::Token(Token {
                kind: TokenKind::Const(const_idx),
                ..
            })) => Ok(Expr::Const(const_idx)),
            other => Err(ParseError::unexpected("an expression", other)),
        }
    }

    fn parse_list<T>(
        &mut self,
        tree_name: &str,
        delim: TokenKind,
        parse: impl Fn(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<Vec<T>> {
        let mut items = vec![];

        while !self.tokens.at_end() {
            let item = parse(self)?;
            items.push(item);

            if !self.eat_kind(delim) {
                if let Some(token) = self.tokens.next() {
                    return Err(ParseError::unexpected(
                        format!("token of kind {delim:?} or end of {tree_name:?}"),
                        Some(token),
                    ));
                }
                break;
            }
        }

        Ok(items)
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_ron_snapshot;

    use crate::syntax::parser::ParseError;
    use crate::syntax::token::Token;
    use crate::syntax::token_stream::TokenStream;
    use crate::syntax::{lexer::Lexer, parser::Parser};

    fn parse<T>(source: &str, f: impl Fn(&mut Parser) -> T) -> (T, Vec<Token>, Vec<ParseError>) {
        let lexer = Lexer::new(source);
        let (tokens, mismatched_brackets) = TokenStream::from_lexer(lexer);

        let mut errors = vec![];
        let mut parser = Parser::new(tokens.into_iter(), &mut errors);
        let res = f(&mut parser);

        (res, mismatched_brackets, errors)
    }

    #[test]
    fn simple_func() {
        assert_ron_snapshot!(parse("foo() {}", |p| p.parse_func()));
    }

    #[test]
    fn func_with_args() {
        assert_ron_snapshot!(parse("foo(a: uint, b: uint) -> string {}", |p| p.parse_func()));
    }

    #[test]
    fn func_with_args_and_trailing_comma() {
        assert_ron_snapshot!(parse("foo(a: uint, b: uint,) {}", |p| p.parse_func()));
    }

    #[test]
    fn func_with_missing_rparen() {
        assert_ron_snapshot!(parse("foo(a: uint, {}", |p| p.parse_func()));
    }

    #[test]
    fn func_with_missing_ret_type() {
        assert_ron_snapshot!(parse("foo(a: uint) -> {}", |p| p.parse_func()));
    }

    #[test]
    fn func_recovery() {
        assert_ron_snapshot!(parse("func foo(a: uint) -> {} {} func my_func() {}", |p| p
            .parse_module()));
    }

    #[test]
    fn block_empty() {
        assert_ron_snapshot!(parse("{}", |p| p.parse_block()));
    }

    #[test]
    fn block_final_expr() {
        assert_ron_snapshot!(parse("{12}", |p| p.parse_block()));
    }

    #[test]
    fn block_no_final_expr() {
        assert_ron_snapshot!(parse("{12;}", |p| p.parse_block()));
    }

    #[test]
    fn block_two_exprs_final_expr() {
        assert_ron_snapshot!(parse("{14;12}", |p| p.parse_block()));
    }

    #[test]
    fn block_two_exprs_no_final_expr() {
        assert_ron_snapshot!(parse("{14;12;}", |p| p.parse_block()));
    }

    #[test]
    fn block_errors() {
        assert_ron_snapshot!(parse("{10; 11 12 13;}", |p| p.parse_block()));
    }

    #[test]
    fn block_errors_final_expr() {
        assert_ron_snapshot!(parse("{10; 11 12}", |p| p.parse_block()));
    }
}
