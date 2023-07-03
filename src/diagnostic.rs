use codespan_reporting::diagnostic::{Diagnostic, Severity};
use codespan_reporting::term;
#[cfg(test)]
use codespan_reporting::term::termcolor::NoColor;
#[cfg(not(test))]
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
#[cfg(test)]
use serde::Serialize;

use crate::lexer::token::Bracket;
use crate::source::{SourceDb, Span};

pub(crate) struct DiagnosticOutput {
    #[cfg(not(test))]
    output: StandardStream,

    #[cfg(test)]
    output: NoColor<Vec<u8>>,

    config: term::Config,
    had_errors: bool,
}

impl Default for DiagnosticOutput {
    fn default() -> Self {
        Self {
            #[cfg(not(test))]
            output: StandardStream::stderr(ColorChoice::Auto),

            #[cfg(test)]
            output: NoColor::new(vec![]),

            config: term::Config::default(),
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
}

pub trait IntoDiagnostic {
    fn into_diagnostic(self, file_id: usize) -> Diagnostic<usize>;
}

// TODO: implement `Display` for brackets and tokens.

pub(crate) struct BracketMismatch {
    pub bracket: Bracket,
    pub span: Span,
}

impl IntoDiagnostic for BracketMismatch {
    fn into_diagnostic(self, file_id: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_labels(vec![self.span.with_source(file_id).label_primary()])
            .with_message(format!("mismatched {:?}", self.bracket))
    }
}
