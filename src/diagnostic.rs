use codespan_reporting::diagnostic::{Diagnostic, Severity};
use codespan_reporting::term;
#[cfg(test)]
use codespan_reporting::term::termcolor::NoColor;
#[cfg(not(test))]
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
#[cfg(test)]
use serde::Serialize;

use crate::lexer::token::Bracket;
use crate::source::{FileSpan, SourceDb};

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
    pub fn reporter<'a>(&'a mut self, source_db: &'a SourceDb) -> DiagnosticReporter {
        DiagnosticReporter {
            source_db,
            output: self,
        }
    }

    pub fn had_errors(&self) -> bool {
        self.had_errors
    }
}

pub(crate) struct DiagnosticReporter<'a> {
    source_db: &'a SourceDb,
    output: &'a mut DiagnosticOutput,
}

impl<'a> DiagnosticReporter<'a> {
    pub fn new(source_db: &'a SourceDb, output: &'a mut DiagnosticOutput) -> Self {
        Self { source_db, output }
    }

    pub fn report(&mut self, diagnostic: impl IntoDiagnostic) {
        let diagnostic = diagnostic.into_diagnostic();
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
    fn into_diagnostic(self) -> Diagnostic<usize>;
}

// TODO: implement `Display` for brackets and tokens.

pub(crate) struct BracketMismatch {
    pub bracket: Bracket,
    pub span: FileSpan,
}

impl IntoDiagnostic for BracketMismatch {
    fn into_diagnostic(self) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_labels(vec![self.span.label_primary()])
            .with_message(format!("mismatched {:?}", self.bracket))
    }
}
