use std::ops::Range;

use codespan_reporting::files::{Error as FileError, Files, SimpleFile};

use super::{ModulePath, SourceIter};

#[derive(Default, Debug, Clone)]
pub(crate) struct SourceDb {
    files: Vec<SimpleFile<ModulePath, String>>,
}

impl SourceDb {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, path: ModulePath, source: impl Into<String>) -> usize {
        let id = self.files.len();
        self.files.push(SimpleFile::new(path, source.into()));
        id
    }

    fn get(&self, id: usize) -> Result<&SimpleFile<ModulePath, String>, FileError> {
        self.files.get(id).ok_or(FileError::FileMissing)
    }
}

impl<'a> Files<'a> for SourceDb {
    type FileId = usize;

    type Name = ModulePath;
    type Source = Source<'a>;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, FileError> {
        self.get(id).map(|file| *file.name())
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, FileError> {
        self.get(id).map(|file| Source {
            path: *file.name(),
            source: file.source(),

            file_id: id,
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

#[derive(Debug, Clone, Copy)]
pub(crate) struct Source<'a> {
    path: ModulePath,
    source: &'a str,

    file_id: usize,
}

impl<'a> Source<'a> {
    pub fn path(&self) -> ModulePath {
        self.path
    }

    pub fn source(&self) -> &'a str {
        self.source
    }

    pub fn source_iter(&self) -> SourceIter<'a> {
        SourceIter::new(self.source)
    }

    pub fn file_id(&self) -> usize {
        self.file_id
    }
}

impl AsRef<str> for Source<'_> {
    fn as_ref(&self) -> &str {
        self.source()
    }
}
