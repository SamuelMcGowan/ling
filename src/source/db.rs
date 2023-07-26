use std::ops::Range;
use std::path::{Path, PathBuf};

use codespan_reporting::files::{Error as FileError, Files, SimpleFile};

use super::iter::SourceIter;

#[derive(Debug, Clone)]
pub(crate) struct SourceDb {
    files: Vec<SimpleFile<String, String>>,
    root_dir: PathBuf,
}

impl SourceDb {
    pub fn new(root_dir: impl Into<PathBuf>) -> Self {
        Self {
            files: vec![],
            root_dir: root_dir.into(),
        }
    }

    pub fn load(&mut self, name: impl Into<String>, path: impl AsRef<Path>) -> Option<SourceId> {
        let source = std::fs::read_to_string(path).ok()?;
        Some(self.add(name.into(), source))
    }

    pub fn add(&mut self, path: impl Into<String>, source: impl Into<String>) -> SourceId {
        let id = self.files.len();
        self.files.push(SimpleFile::new(path.into(), source.into()));
        SourceId(id)
    }

    fn get(&self, id: SourceId) -> Result<&SimpleFile<String, String>, FileError> {
        self.files.get(id.0).ok_or(FileError::FileMissing)
    }
}

impl<'a> Files<'a> for SourceDb {
    type FileId = SourceId;

    type Name = &'a str;
    type Source = Source<'a>;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, FileError> {
        self.get(id).map(|file| file.name().as_ref())
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, FileError> {
        self.get(id).map(|file| Source {
            path: file.name(),
            source: file.source(),

            id,
        })
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, FileError> {
        self.get(id)
            .and_then(|file| file.line_index((), byte_index))
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<Range<usize>, FileError> {
        self.get(id)
            .and_then(|file| file.line_range((), line_index))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct SourceId(usize);

#[derive(Debug, Clone, Copy)]
pub(crate) struct Source<'a> {
    path: &'a str,
    source: &'a str,

    id: SourceId,
}

impl<'a> Source<'a> {
    pub fn path(&self) -> &'a str {
        self.path
    }

    pub fn source(&self) -> &'a str {
        self.source
    }

    pub fn source_iter(&self) -> SourceIter<'a> {
        SourceIter::new(self.source)
    }

    pub fn id(&self) -> SourceId {
        self.id
    }
}

impl AsRef<str> for Source<'_> {
    fn as_ref(&self) -> &str {
        self.source()
    }
}

// TODO: put all these test functions in separate modules so we don't have to
// use full type names.
#[cfg(test)]
pub(crate) fn with_test_source<T>(
    source: &str,
    mut f: impl FnMut(Source, crate::diagnostic::DiagnosticReporter) -> T,
) -> (T, crate::diagnostic::DiagnosticOutput) {
    use crate::diagnostic::DiagnosticOutput;

    let mut source_db = SourceDb::new("");

    let source_id = source_db.add("test_source", source);
    let source = source_db.source(source_id).unwrap();

    let mut diagnostic_output = DiagnosticOutput::default();
    let diagnostics = diagnostic_output.reporter(&source_db, source_id);

    (f(source, diagnostics), diagnostic_output)
}
