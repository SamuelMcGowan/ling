use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use codespan_reporting::term;
#[cfg(test)]
use codespan_reporting::term::termcolor::NoColor;
#[cfg(not(test))]
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use serde::Serialize;
use ustr::Ustr;

use crate::lexer::token::Bracket;
use crate::source::{SourceDb, Span};

pub(crate) struct DiagnosticOutput {
    #[cfg(not(test))]
    output: StandardStream,

    #[cfg(test)]
    output: NoColor<Vec<u8>>,

    config: Box<term::Config>,
    had_errors: bool,
}

impl Default for DiagnosticOutput {
    fn default() -> Self {
        Self {
            #[cfg(not(test))]
            output: StandardStream::stderr(ColorChoice::Auto),

            #[cfg(test)]
            output: NoColor::new(vec![]),

            config: Box::default(),
            had_errors: false,
        }
    }
}

#[cfg(test)]
impl Serialize for DiagnosticOutput {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let s = String::from_utf8_lossy(self.output.get_ref());
        s.serialize(serializer)
    }
}

impl DiagnosticOutput {
    pub fn reporter<'a>(
        &'a mut self,
        source_db: &'a SourceDb,
        source_id: usize,
    ) -> DiagnosticReporter {
        DiagnosticReporter {
            output: self,
            source_db,
            source_id,
        }
    }

    pub fn had_errors(&self) -> bool {
        self.had_errors
    }
}

pub(crate) struct DiagnosticReporter<'a> {
    output: &'a mut DiagnosticOutput,

    source_db: &'a SourceDb,
    source_id: usize,
}

impl<'a> DiagnosticReporter<'a> {
    pub fn borrow(&mut self) -> DiagnosticReporter {
        DiagnosticReporter {
            source_db: self.source_db,
            output: self.output,
            source_id: self.source_id,
        }
    }

    pub fn report(&mut self, diagnostic: impl IntoDiagnostic) {
        let diagnostic = diagnostic.into_diagnostic(self.source_id);
        self.output.had_errors |= diagnostic.severity >= Severity::Error;

        term::emit(
            &mut self.output.output,
            &self.output.config,
            self.source_db,
            &diagnostic,
        )
        .expect("failed to emit diagnostic");
    }

    pub fn had_errors(&self) -> bool {
        self.output.had_errors
    }
}

pub trait IntoDiagnostic {
    fn into_diagnostic(self, source_id: usize) -> Diagnostic<usize>;
}

// TODO: implement `Display` for brackets and tokens.

pub(crate) struct BracketMismatch {
    pub bracket: Bracket,
    pub span: Span,
}

impl IntoDiagnostic for BracketMismatch {
    fn into_diagnostic(self, source_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_labels(vec![Label::primary(source_id, self.span)])
            .with_message(format!("mismatched {:?}", self.bracket))
    }
}

#[derive(Debug, Clone, Serialize)]
pub(crate) enum ParseError {
    Unexpected { expected: String, span: Span },
    InvalidAssignmentTarget(Span),
    InvalidImplicitReturn(Span),
    InvalidAccessor(Span),
}

impl ParseError {
    pub fn unexpected(expected: impl Into<String>, span: Span) -> Self {
        Self::Unexpected {
            expected: expected.into(),
            span,
        }
    }
}

impl IntoDiagnostic for ParseError {
    fn into_diagnostic(self, source_id: usize) -> Diagnostic<usize> {
        macro_rules! label_primary {
            ($span:expr) => {
                Label::primary(source_id, $span)
            };
        }

        match self {
            Self::Unexpected { expected, span } => Diagnostic::error()
                .with_message(format!("expected {expected}"))
                .with_labels(vec![label_primary!(span)]),

            Self::InvalidAssignmentTarget(span) => Diagnostic::error()
                .with_message("invalid assignment target")
                .with_labels(vec![label_primary!(span)]),

            Self::InvalidImplicitReturn(span) => Diagnostic::error()
                .with_message("invalid implicit return")
                .with_labels(vec![label_primary!(span)]),

            Self::InvalidAccessor(span) => Diagnostic::error()
                .with_message("invalid accessor")
                .with_labels(vec![label_primary!(span)]),
        }
    }
}

pub(crate) type ParseResult<T> = Result<T, ParseError>;

// TODO: store spans instead
#[derive(Debug)]
pub(crate) enum SymbolError {
    WrongKind { ident: Ustr, should_be_value: bool },
    SymbolNotFound(Ustr),
    GlobalShadowed(Ustr),
}

impl IntoDiagnostic for SymbolError {
    fn into_diagnostic(self, source_id: usize) -> Diagnostic<usize> {
        match self {
            Self::WrongKind {
                ident,
                should_be_value,
            } => {
                let message = if should_be_value {
                    "expected a value"
                } else {
                    "did not expect a value"
                };
                Diagnostic::error().with_message(message)
            }

            Self::SymbolNotFound(ident) => {
                Diagnostic::error().with_message(format!("symbol `{ident}` not found"))
            }

            Self::GlobalShadowed(ident) => {
                Diagnostic::error().with_message(format!("global `{ident}` shadowed"))
            }
        }
    }
}
