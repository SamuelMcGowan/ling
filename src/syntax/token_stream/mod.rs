mod builder;

use std::vec::IntoIter;

use serde::Serialize;

use super::lexer::Lexer;
use super::token::{BracketKind, Token};

use self::builder::build_token_stream;

#[derive(Debug, Clone, Serialize)]
pub(crate) struct TokenStream(Vec<TokenTree>);

impl TokenStream {
    pub fn from_lexer(lexer: Lexer) -> (Self, Vec<Token>) {
        build_token_stream(lexer)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl IntoIterator for TokenStream {
    type IntoIter = TokenIter;
    type Item = TokenTree;

    fn into_iter(self) -> Self::IntoIter {
        TokenIter(self.0.into_iter())
    }
}

pub(crate) struct TokenIter(IntoIter<TokenTree>);

impl TokenIter {
    pub fn at_end(&self) -> bool {
        self.peek().is_none()
    }
    pub fn peek(&self) -> Option<&TokenTree> {
        self.0.as_slice().iter().next()
    }
}

impl Iterator for TokenIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl ExactSizeIterator for TokenIter {}

#[derive(Debug, Clone, Serialize)]
pub(crate) enum TokenTree {
    Token(Token),
    Group {
        bracket_kind: BracketKind,
        tokens: TokenStream,
    },
}

#[cfg(test)]
mod tests {
    use insta::assert_ron_snapshot;

    use super::*;

    fn get_stream(source: &str) -> (TokenStream, Vec<Token>) {
        TokenStream::from_lexer(Lexer::new(source))
    }

    #[test]
    fn simple_tests() {
        assert_ron_snapshot!("token_tree_0", get_stream("( { ) } ( hello )"));
        assert_ron_snapshot!("token_tree_1", get_stream("{{{}}}"));
        assert_ron_snapshot!("token_tree_2", get_stream("((("));
        assert_ron_snapshot!("token_tree_3", get_stream(")))"));
        assert_ron_snapshot!("token_tree_4", get_stream("]{}["));

        assert_ron_snapshot!(
            "token_tree_func",
            get_stream(
                "
            func foo(a: uint, b: uint) -> uint {
                a + b
            }
        "
            )
        );
    }
}
