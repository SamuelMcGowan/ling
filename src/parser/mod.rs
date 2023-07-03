pub mod expr;
pub mod item;

use crate::ast::Spanned;
use crate::diagnostic::{DiagnosticReporter, ParseError, ParseResult};
use crate::lexer::token::{BracketKind, Token, TokenKind};
use crate::token_tree::{TokenIter, TokenTree};

pub(crate) struct Parser<'a> {
    tokens: TokenIter,
    diagnostics: DiagnosticReporter<'a>,
}

// Utilities.
impl<'a> Parser<'a> {
    pub fn new(tokens: TokenIter, diagnostics: DiagnosticReporter<'a>) -> Self {
        Self {
            tokens,
            diagnostics,
        }
    }

    fn parser_for_tokens(&mut self, tokens: TokenIter) -> Parser {
        Parser {
            tokens,
            diagnostics: self.diagnostics.borrow(),
        }
    }

    fn eat_kind(&mut self, kind: TokenKind) -> bool {
        match self.tokens.peek() {
            Some(TokenTree::Token(token)) if token.kind == kind => {
                self.tokens.next();
                true
            }
            _ => false,
        }
    }

    fn expect_kind(&mut self, kind: TokenKind) -> ParseResult<Token> {
        match self.tokens.next() {
            Some(TokenTree::Token(token)) if token.kind == kind => Ok(token),
            other => Err(self.unexpected(format!("token of kind {kind:?}"), other)),
        }
    }

    fn expect_group(
        &mut self,
        kind: BracketKind,
        expected: impl Into<String>,
    ) -> ParseResult<TokenIter> {
        match self.tokens.next() {
            Some(TokenTree::Group {
                bracket_kind,
                tokens,
                ..
            }) if bracket_kind == kind => Ok(tokens.into_iter()),
            other => Err(self.unexpected(expected, other)),
        }
    }

    fn expect_end(&mut self, tree_name: &str) -> ParseResult<()> {
        match self.tokens.next() {
            None => Ok(()),
            Some(tt) => Err(self.unexpected(format!("end of {tree_name}"), Some(tt))),
        }
    }

    fn eat_group(&mut self, kind: BracketKind) -> Option<TokenIter> {
        match self.tokens.peek() {
            Some(TokenTree::Group { bracket_kind, .. }) if *bracket_kind == kind => {
                let Some(TokenTree::Group { tokens, ..}) = self.tokens.next() else {
                    unreachable!();
                };

                Some(tokens.into_iter())
            }
            _ => None,
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
                    return Err(self.unexpected(
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

// Error handling/recovery.
impl Parser<'_> {
    fn parse_or_recover<T>(
        &mut self,
        parser: impl Fn(&mut Self) -> ParseResult<T>,
        recover: impl Fn(&mut Self) -> T,
    ) -> T {
        parser(self).unwrap_or_else(|error| {
            self.diagnostics.report(error);
            recover(self)
        })
    }

    fn parse_spanned<T>(&mut self, parser: impl Fn(&mut Self) -> T) -> Spanned<T> {
        let start = self.tokens.peek().map(|tt| tt.span());
        let inner = parser(self);
        let end = self.tokens.prev_span();

        let span = match (start, end) {
            (Some(start), Some(end)) => Some(start.union(end)),
            _ => None,
        };

        Spanned { inner, span }
    }

    fn recover_until(&mut self, kinds: &[TokenKind]) {
        while let Some(tt) = self.tokens.peek() {
            if matches!(tt, TokenTree::Token(token) if kinds.contains(&token.kind)) {
                break;
            }
            self.tokens.next();
        }
    }

    fn recover_past(&mut self, kind: TokenKind) {
        self.tokens
            .by_ref()
            .take_while(|token| !matches!(token, TokenTree::Token(token) if token.kind == kind))
            .for_each(|_| {});
    }

    fn unexpected(&self, expected: impl Into<String>, tt: Option<TokenTree>) -> ParseError {
        let span = match &tt {
            Some(tt) => tt.span(),
            None => self.tokens.end_span(),
        };
        ParseError::unexpected(expected.into(), span)
    }
}

#[cfg(test)]
pub(crate) fn test_lex(
    source: &str,
) -> (
    crate::token_tree::TokenList,
    crate::diagnostic::DiagnosticOutput,
) {
    use crate::lexer::Lexer;
    use crate::source::with_test_source;
    use crate::token_tree::TokenList;

    with_test_source(source, |source, diagnostics| {
        let lexer = Lexer::new(source);
        TokenList::from_lexer(lexer, diagnostics)
    })
}

#[cfg(test)]
pub(crate) fn test_parse<T>(
    source: &str,
    f: impl Fn(&mut Parser) -> T,
) -> (T, crate::diagnostic::DiagnosticOutput) {
    use crate::lexer::Lexer;
    use crate::source::with_test_source;
    use crate::token_tree::TokenList;

    let (res, diagnostic_output) = with_test_source(source, |source, mut diagnostics| {
        let lexer = Lexer::new(source);
        let tokens = TokenList::from_lexer(lexer, diagnostics.borrow());

        let mut parser = Parser::new(tokens.into_iter(), diagnostics);
        f(&mut parser)
    });

    (res, diagnostic_output)
}
