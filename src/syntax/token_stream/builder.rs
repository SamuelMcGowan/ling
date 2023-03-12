use crate::syntax::lexer::Lexer;
use crate::syntax::token::{Bracket, BracketKind, Token, TokenKind};

use super::{TokenStream, TokenTree};

pub(super) fn build_token_stream(lexer: Lexer) -> (TokenStream, Vec<Token>) {
    TokenStreamBuilder::new(lexer).build()
}

struct SpareBracket {
    kind: BracketKind,
    token: Token,
}

struct TokenStreamBuilder<'a> {
    lexer: Lexer<'a>,
    mismatched_brackets: Vec<Token>,
}

impl<'a> TokenStreamBuilder<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            mismatched_brackets: vec![],
        }
    }

    fn build(mut self) -> (TokenStream, Vec<Token>) {
        let mut nodes = vec![];

        while let Some(token) = self.lexer.next() {
            let (node, spare_bracket) = self.parse_node(token);

            if let Some(node) = node {
                nodes.push(node);
            }

            // Nobody claimed this poor bracket :(
            if let Some(spare_bracket) = spare_bracket {
                // we don't emit a node for unmatched closing brackets
                self.mismatched_brackets.push(spare_bracket.token);
            }
        }

        (TokenStream(nodes), self.mismatched_brackets)
    }

    fn parse_node(&mut self, token: Token) -> (Option<TokenTree>, Option<SpareBracket>) {
        match token.kind {
            TokenKind::Bracket(Bracket::Opening(kind)) => {
                // ooh look, a tree
                let (nodes, spare_bracket) = self.parse_tree(token, kind);
                (
                    Some(TokenTree::Group {
                        bracket_kind: kind,
                        tokens: TokenStream(nodes),
                    }),
                    spare_bracket,
                )
            }
            TokenKind::Bracket(Bracket::Closing(kind)) => {
                // an unexpected closing bracket! aargh!
                (None, Some(SpareBracket { kind, token }))
            }
            _ => (Some(TokenTree::Token(token)), None),
        }
    }

    fn parse_tree(
        &mut self,
        token: Token,
        kind: BracketKind,
    ) -> (Vec<TokenTree>, Option<SpareBracket>) {
        let mut nodes = vec![];

        let spare_bracket = loop {
            // get a token
            let Some(inner_token) = self.lexer.next() else {
                // unexpected end of input
                self.mismatched_brackets.push(token);
                break None;
            };

            // check for our closing bracket
            if matches!(inner_token.kind, TokenKind::Bracket(Bracket::Closing(k)) if k == kind) {
                break None;
            }

            // parse a child node
            let (node, spare_bracket) = self.parse_node(inner_token);

            if let Some(node) = node {
                nodes.push(node);
            }

            // check the spare bracket
            if let Some(spare_bracket) = spare_bracket {
                if spare_bracket.kind == kind {
                    // our closing bracket was found!
                    break None;
                } else {
                    // not our bracket
                    // our bracket was unterminated too, so we report that
                    self.mismatched_brackets.push(token);
                    break Some(spare_bracket);
                }
            }
        };

        (nodes, spare_bracket)
    }
}
