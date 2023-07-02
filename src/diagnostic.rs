use codespan_reporting::diagnostic::{Diagnostic, Severity};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use crate::source::SourceDb;

pub(crate) struct DiagnosticReporter<'src> {
    stream: StandardStream,
    config: term::Config,
    source_db: &'src SourceDb,

    had_errors: bool,
}

impl<'src> DiagnosticReporter<'src> {
    pub fn new(source_db: &'src SourceDb) -> Self {
        Self {
            stream: StandardStream::stderr(ColorChoice::Auto),
            config: term::Config::default(),
            source_db,
            had_errors: false,
        }
    }

    pub fn report(&mut self, diagnostic: impl IntoDiagnostic) {
        let diagnostic = diagnostic.into_diagnostic();
        self.had_errors |= diagnostic.severity >= Severity::Error;
        term::emit(&mut self.stream, &self.config, self.source_db, &diagnostic)
            .expect("failed to emit diagnostic");
    }

    pub fn had_errors(&self) -> bool {
        self.had_errors
    }
}

pub trait IntoDiagnostic {
    fn into_diagnostic(self) -> Diagnostic<usize>;
}
