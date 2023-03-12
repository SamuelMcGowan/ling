mod builder;

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

    pub fn iter(&self) -> impl Iterator<Item = &TokenTree> {
        self.0.iter()
    }
}

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
