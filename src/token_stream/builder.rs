use super::{TokenStream, TokenTree};
use crate::diagnostic::{BracketMismatch, DiagnosticReporter};
use crate::lexer::token::{Bracket, BracketKind, Token, TokenKind};
use crate::lexer::Lexer;
use crate::source::{Source, Span};

pub(super) fn build_token_stream(source: Source, diagnostics: DiagnosticReporter) -> TokenStream {
    TokenStreamBuilder::new(source, diagnostics).build()
}

struct SpareBracket {
    kind: BracketKind,
    token: Token,
}

struct TokenStreamBuilder<'a> {
    source: Source<'a>,
    lexer: Lexer<'a>,

    diagnostics: DiagnosticReporter<'a>,
}

impl<'a> TokenStreamBuilder<'a> {
    fn new(source: Source<'a>, diagnostics: DiagnosticReporter<'a>) -> Self {
        Self {
            source,
            lexer: Lexer::new(source),

            diagnostics,
        }
    }

    fn build(mut self) -> TokenStream {
        let mut nodes = vec![];

        while let Some(token) = self.lexer.next() {
            let (node, spare_bracket) = self.parse_node(token);

            if let Some(node) = node {
                nodes.push(node);
            }

            // Nobody claimed this poor bracket :(
            if let Some(spare_bracket) = spare_bracket {
                // we don't emit a node for unmatched closing brackets
                self.diagnostics.report(BracketMismatch {
                    bracket: Bracket::Closing(spare_bracket.kind),
                    span: spare_bracket.token.span.with_file(self.source.file_id()),
                });
            }
        }

        TokenStream(nodes)
    }

    fn parse_node(&mut self, token: Token) -> (Option<TokenTree>, Option<SpareBracket>) {
        match token.kind {
            TokenKind::Bracket(Bracket::Opening(kind)) => {
                // ooh look, a tree
                let (nodes, span, spare_bracket) = self.parse_tree(token, kind);
                (
                    Some(TokenTree::Group {
                        bracket_kind: kind,
                        tokens: TokenStream(nodes),
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

    fn parse_tree(
        &mut self,
        token: Token,
        kind: BracketKind,
    ) -> (Vec<TokenTree>, Span, Option<SpareBracket>) {
        let mut nodes = vec![];

        macro_rules! prev_span {
            () => {
                nodes
                    .last()
                    .map(|node: &TokenTree| node.span())
                    .unwrap_or(token.span)
            };
        }

        let (end_span, spare_bracket) = loop {
            // get a token
            let Some(inner_token) = self.lexer.next() else {
                // unexpected end of input
                self.diagnostics.report(BracketMismatch {
                    bracket: Bracket::Opening(kind),
                    span: token.span.with_file(self.source.file_id()),
                });
                break (prev_span!(), None);
            };

            // check for our closing bracket
            if matches!(inner_token.kind, TokenKind::Bracket(Bracket::Closing(k)) if k == kind) {
                break (inner_token.span, None);
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
                    break (spare_bracket.token.span, None);
                } else {
                    // not our bracket
                    // our bracket was unterminated too, so we report that
                    self.diagnostics.report(BracketMismatch {
                        bracket: Bracket::Opening(kind),
                        span: token.span.with_file(self.source.file_id()),
                    });
                    break (prev_span!(), Some(spare_bracket));
                }
            }
        };

        let span = token.span.union(end_span);

        (nodes, span, spare_bracket)
    }
}
