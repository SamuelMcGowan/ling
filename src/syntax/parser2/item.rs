use crate::syntax::ast::*;
use crate::syntax::token::tkind;

use super::*;

impl Parser<'_> {
    pub fn parse_module(&mut self) -> ParseResult<Module> {
        let mut items = vec![];

        while !self.tokens.at_end() {
            let item = self.parse_item()?;
            items.push(item);
        }

        Ok(Module { items })
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
            let group = self.expect_group(BracketKind::Round, "parameters list")?;
            let mut parser = self.parser_for_tokens(group);

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
                // TODO: clean this up a bit
                let params = if self.next_is_group(BracketKind::Square) {
                    let group = self.expect_group(BracketKind::Square, "").unwrap();
                    let mut parser = self.parser_for_tokens(group);
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
            }) if tokens.is_empty() => Ok(Ty::Unit),

            other => Err(ParseError::unexpected("a type", other)),
        }
    }

    fn parse_block(&mut self) -> ParseResult<Expr> {
        let _group = self.expect_group(BracketKind::Curly, "block")?;
        // TODO: actually parse block
        Ok(Expr::Unit)
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
