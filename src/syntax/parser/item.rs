use crate::syntax::parser::ast::*;
use crate::syntax::parser::{ParseError, ParseResult, Parser};
use crate::syntax::token::tkind;

impl Parser {
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
            Some(token) if token.kind == tkind!(kwd Func) => Ok(Item::Func(self.parse_func()?)),
            other => Err(ParseError::unexpected("an item", other)),
        }
    }

    /// Expects `func` keyword to have been parsed.
    fn parse_func(&mut self) -> ParseResult<Func> {
        let ident = self.parse_ident()?;

        self.tokens.expect(tkind!(punct LParen))?;

        let params = self.parse_list(tkind!(punct RParen), |s| {
            let ident = s.parse_ident()?;
            s.tokens.expect(tkind!(punct Colon))?;
            let ty = s.parse_ty()?;
            Ok((ident, ty))
        })?;

        self.tokens.expect(tkind!(punct RParen))?;

        let ret_ty = if self.tokens.eat(tkind!(punct Arrow)) {
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
}

#[cfg(test)]
mod tests {
    use insta::assert_ron_snapshot;

    use crate::syntax::{lexer::Lexer, parser::Parser};

    fn parser(source: &str) -> Parser {
        let tokens = Lexer::new(source).collect();
        Parser::new(tokens)
    }

    #[test]
    fn test_func() {
        assert_ron_snapshot!("simple_func", parser("foo() {}").parse_func().unwrap());

        assert_ron_snapshot!(
            "func_with_args",
            parser("foo(a: uint, b: uint) -> string {}")
                .parse_func()
                .unwrap()
        );
    }
}
