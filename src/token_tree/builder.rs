use super::{TokenList, TokenTree};
use crate::diagnostic::{BracketMismatch, DiagnosticReporter};
use crate::lexer::token::{Bracket, BracketKind, Token, TokenKind};
use crate::lexer::Lexer;
use crate::source::Span;

pub(super) fn build_token_stream(lexer: Lexer, diagnostics: DiagnosticReporter) -> TokenList {
    TokenStreamBuilder::new(lexer, diagnostics).build()
}

struct SpareBracket {
    kind: BracketKind,
    token: Token,
}

struct TokenStreamBuilder<'a> {
    lexer: Lexer<'a>,

    diagnostics: DiagnosticReporter<'a>,
}

impl<'a> TokenStreamBuilder<'a> {
    fn new(lexer: Lexer<'a>, diagnostics: DiagnosticReporter<'a>) -> Self {
        Self { lexer, diagnostics }
    }

    fn build(mut self) -> TokenList {
        let mut trees = vec![];

        while let Some(token) = self.lexer.next() {
            let (tree, spare_bracket) = self.parse_tree(token);

            if let Some(tree) = tree {
                trees.push(tree);
            }

            // Nobody claimed this poor bracket :(
            if let Some(spare_bracket) = spare_bracket {
                // we don't emit a tree for unmatched closing brackets
                self.diagnostics.report(BracketMismatch {
                    bracket: Bracket::Closing(spare_bracket.kind),
                    span: spare_bracket.token.span,
                });
            }
        }

        TokenList(trees)
    }

    fn parse_tree(&mut self, token: Token) -> (Option<TokenTree>, Option<SpareBracket>) {
        match token.kind {
            TokenKind::Bracket(Bracket::Opening(kind)) => {
                // ooh look, a tree
                let (tokens, span, spare_bracket) = self.parse_group(token, kind);
                (
                    Some(TokenTree::Group {
                        bracket_kind: kind,
                        tokens,
                        span,
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

    fn parse_group(
        &mut self,
        token: Token,
        kind: BracketKind,
    ) -> (TokenList, Span, Option<SpareBracket>) {
        let mut trees = vec![];

        macro_rules! prev_span {
            () => {
                trees
                    .last()
                    .map(|tree: &TokenTree| tree.span())
                    .unwrap_or(token.span)
            };
        }

        let (end_span, spare_bracket) = loop {
            // get a token
            let Some(inner_token) = self.lexer.next() else {
                // unexpected end of input
                self.diagnostics.report(BracketMismatch {
                    bracket: Bracket::Opening(kind),
                    span: token.span,
                });
                break (prev_span!(), None);
            };

            // check for our closing bracket
            if matches!(inner_token.kind, TokenKind::Bracket(Bracket::Closing(k)) if k == kind) {
                break (inner_token.span, None);
            }

            // parse a child tree
            let (tree, spare_bracket) = self.parse_tree(inner_token);

            if let Some(tree) = tree {
                trees.push(tree);
            }

            // check the spare bracket
            if let Some(spare_bracket) = spare_bracket {
                if spare_bracket.kind == kind {
                    // our closing bracket was found!
                    break (spare_bracket.token.span, None);
                } else {
                    // not our bracket
                    // our bracket was unterminated too, so we report that
                    self.diagnostics.report(BracketMismatch {
                        bracket: Bracket::Opening(kind),
                        span: token.span,
                    });
                    break (prev_span!(), Some(spare_bracket));
                }
            }
        };

        let span = token.span.union(end_span);

        (TokenList(trees), span, spare_bracket)
    }
}
