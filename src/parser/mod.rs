pub mod expr;
pub mod item;

use crate::ast::Spanned;
use crate::lexer::token::{BracketKind, Token, TokenKind};
use crate::source::Span;
use crate::token_tree::{TokenIter, TokenTree};

#[derive(Debug, Clone, serde::Serialize)]
pub(crate) enum ParseError {
    Unexpected {
        expected: String,
        found: Option<Span>,
    },
    InvalidAssignmentTarget(Span),
    InvalidImplicitReturn(Span),
    InvalidAccessor(Span),
}

impl ParseError {
    fn unexpected(expected: impl Into<String>, found: Option<TokenTree>) -> Self {
        Self::Unexpected {
            expected: expected.into(),
            found: found.map(|tt| tt.span()),
        }
    }
}

pub(crate) type ParseResult<T> = Result<T, ParseError>;

pub(crate) struct Parser<'a> {
    errors: &'a mut Vec<ParseError>,
    tokens: TokenIter,
}

// Utilities.
impl<'a> Parser<'a> {
    pub fn new(tokens: TokenIter, errors: &'a mut Vec<ParseError>) -> Self {
        Self { tokens, errors }
    }

    fn parser_for_tokens(&mut self, tokens: TokenIter) -> Parser {
        Parser {
            errors: self.errors,
            tokens,
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
            other => Err(ParseError::unexpected(
                format!("token of kind {kind:?}"),
                other,
            )),
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
            other => Err(ParseError::unexpected(expected.into(), other)),
        }
    }

    fn expect_end(&mut self, tree_name: &str) -> ParseResult<()> {
        match self.tokens.next() {
            None => Ok(()),
            Some(tree) => Err(ParseError::unexpected(
                format!("end of {tree_name}"),
                Some(tree),
            )),
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

// Error handling/recovery.
impl Parser<'_> {
    fn parse_or_recover<T>(
        &mut self,
        parser: impl Fn(&mut Self) -> ParseResult<T>,
        recover: impl Fn(&mut Self) -> T,
    ) -> T {
        parser(self).unwrap_or_else(|error| {
            self.report(error);
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

    fn report(&mut self, error: ParseError) {
        self.errors.push(error);
    }
}

#[cfg(test)]
pub(crate) fn test_lex(
    source: &str,
) -> (
    crate::token_tree::TokenList,
    crate::diagnostic::DiagnosticOutput,
) {
    use crate::source::with_test_source;
    use crate::token_tree::TokenList;

    with_test_source(source, |source, diagnostics| {
        TokenList::from_source(source, diagnostics)
    })
}

#[cfg(test)]
pub(crate) fn test_parse<T>(
    source: &str,
    f: impl Fn(&mut Parser) -> T,
) -> (T, crate::diagnostic::DiagnosticOutput, Vec<ParseError>) {
    let (tokens, diagnostic_output) = test_lex(source);

    let mut errors = vec![];
    let mut parser = Parser::new(tokens.into_iter(), &mut errors);
    let res = f(&mut parser);

    (res, diagnostic_output, errors)
}
