mod builder;

use std::vec::IntoIter;

use serde::Serialize;

use self::builder::build_token_stream;
use crate::diagnostic::DiagnosticReporter;
use crate::lexer::token::{BracketKind, Token};
use crate::lexer::Lexer;
use crate::source::Span;

#[derive(Debug, Clone, Serialize)]
pub(crate) enum TokenTree {
    Token(Token),
    Group {
        bracket_kind: BracketKind,
        tokens: TokenList,
        span: Span,
    },
}

impl TokenTree {
    pub fn span(&self) -> Span {
        match self {
            Self::Token(token) => token.span,
            Self::Group { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct TokenList(Vec<TokenTree>);

impl TokenList {
    pub fn from_lexer(lexer: Lexer, diagnostics: DiagnosticReporter) -> Self {
        build_token_stream(lexer, diagnostics)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IntoIterator for TokenList {
    type IntoIter = TokenIter;
    type Item = TokenTree;

    fn into_iter(self) -> Self::IntoIter {
        TokenIter {
            iter: self.0.into_iter(),
            prev_span: None,
        }
    }
}

pub(crate) struct TokenIter {
    iter: IntoIter<TokenTree>,
    prev_span: Option<Span>,
}

impl TokenIter {
    pub fn at_end(&self) -> bool {
        self.peek().is_none()
    }

    pub fn peek(&self) -> Option<&TokenTree> {
        self.iter.as_slice().iter().next()
    }

    pub fn prev_span(&self) -> Option<Span> {
        self.prev_span
    }
}

impl Iterator for TokenIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.iter.next()?;
        self.prev_span = Some(item.span());
        Some(item)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl ExactSizeIterator for TokenIter {}

#[cfg(test)]
mod tests {
    use insta::assert_ron_snapshot;

    use crate::parser::test_lex;

    #[test]
    fn simple_tests() {
        assert_ron_snapshot!("token_tree_0", test_lex("( { ) } ( hello )"));
        assert_ron_snapshot!("token_tree_1", test_lex("{{{}}}"));
        assert_ron_snapshot!("token_tree_2", test_lex("((("));
        assert_ron_snapshot!("token_tree_3", test_lex(")))"));
        assert_ron_snapshot!("token_tree_4", test_lex("]{}["));

        assert_ron_snapshot!(
            "token_tree_func",
            test_lex(
                "
            func foo(a: uint, b: uint) -> uint {
                a + b
            }
        "
            )
        );
    }
}
