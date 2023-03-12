use serde::Serialize;

use super::lexer::Lexer;
use super::token::{Bracket, BracketKind, Token, TokenKind};

struct SpareBracket {
    kind: BracketKind,
    token: Token,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) enum Node {
    Token(Token),
    Tree { kind: BracketKind, nodes: Vec<Node> },
}

pub(crate) struct TreeBuilder<'a> {
    lexer: Lexer<'a>,
    mismatched_brackets: Vec<Token>,
}

impl<'a> TreeBuilder<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            mismatched_brackets: vec![],
        }
    }

    pub fn build(mut self) -> (Vec<Node>, Vec<Token>) {
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

        (nodes, self.mismatched_brackets)
    }

    fn parse_node(&mut self, token: Token) -> (Option<Node>, Option<SpareBracket>) {
        match token.kind {
            TokenKind::Bracket(Bracket::Opening(kind)) => {
                // ooh look, a tree
                let (nodes, spare_bracket) = self.parse_tree(token, kind);
                (Some(Node::Tree { kind, nodes }), spare_bracket)
            }
            TokenKind::Bracket(Bracket::Closing(kind)) => {
                // an unexpected closing bracket! aargh!
                (None, Some(SpareBracket { kind, token }))
            }
            _ => (Some(Node::Token(token)), None),
        }
    }

    fn parse_tree(&mut self, token: Token, kind: BracketKind) -> (Vec<Node>, Option<SpareBracket>) {
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

#[cfg(test)]
mod tests {
    use insta::assert_ron_snapshot;

    use super::*;

    fn get_tree(source: &str) -> (Vec<Node>, Vec<Token>) {
        let lexer = Lexer::new(source);
        let tt_builder = TreeBuilder::new(lexer);
        tt_builder.build()
    }

    #[test]
    fn simple_tests() {
        assert_ron_snapshot!("token_tree_0", get_tree("( { ) } ( hello )"));
        assert_ron_snapshot!("token_tree_1", get_tree("{{{}}}"));
        assert_ron_snapshot!("token_tree_2", get_tree("((("));
        assert_ron_snapshot!("token_tree_3", get_tree(")))"));
        assert_ron_snapshot!("token_tree_4", get_tree("]{}["));
    }
}
