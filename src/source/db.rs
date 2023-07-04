use std::ops::Range;
use std::path::PathBuf;

use codespan_reporting::files::{Error as FileError, Files, SimpleFile};

use super::{ModulePath, SourceIter};

#[derive(Debug, Clone)]
pub(crate) struct ModuleSourceDb {
    files: Vec<SimpleFile<ModulePath, String>>,
    root_dir: PathBuf,
}

impl ModuleSourceDb {
    pub fn new(root_dir: impl Into<PathBuf>) -> Self {
        Self {
            files: vec![],
            root_dir: root_dir.into(),
        }
    }

    pub fn load(&mut self, path: ModulePath) -> Option<ModuleId> {
        let module_path_buf = path.into_path_buf(&self.root_dir);
        let source = std::fs::read_to_string(module_path_buf).ok()?;
        Some(self.add(path, source))
    }

    pub fn add(&mut self, path: ModulePath, source: impl Into<String>) -> ModuleId {
        let id = self.files.len();
        self.files.push(SimpleFile::new(path, source.into()));
        ModuleId(id)
    }

    fn get(&self, id: ModuleId) -> Result<&SimpleFile<ModulePath, String>, FileError> {
        self.files.get(id.0).ok_or(FileError::FileMissing)
    }
}

impl<'a> Files<'a> for ModuleSourceDb {
    type FileId = ModuleId;

    type Name = ModulePath;
    type Source = ModuleSource<'a>;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, FileError> {
        self.get(id).map(|file| *file.name())
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, FileError> {
        self.get(id).map(|file| ModuleSource {
            path: *file.name(),
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
pub(crate) struct ModuleId(usize);

#[derive(Debug, Clone, Copy)]
pub(crate) struct ModuleSource<'a> {
    path: ModulePath,
    source: &'a str,

    id: ModuleId,
}

impl<'a> ModuleSource<'a> {
    pub fn path(&self) -> ModulePath {
        self.path
    }

    pub fn source(&self) -> &'a str {
        self.source
    }

    pub fn source_iter(&self) -> SourceIter<'a> {
        SourceIter::new(self.source)
    }

    pub fn file_id(&self) -> ModuleId {
        self.id
    }
}

impl AsRef<str> for ModuleSource<'_> {
    fn as_ref(&self) -> &str {
        self.source()
    }
}

// TODO: put all these test functions in separate modules so we don't have to
// use full type names.
#[cfg(test)]
pub(crate) fn with_test_module<T>(
    source: &str,
    mut f: impl FnMut(ModuleSource, crate::diagnostic::DiagnosticReporter) -> T,
) -> (T, crate::diagnostic::DiagnosticOutput) {
    use crate::diagnostic::DiagnosticOutput;

    let mut module_src_db = ModuleSourceDb::new("");

    let module_id = module_src_db.add(ModulePath::root("test_module"), source);
    let source = module_src_db.source(module_id).unwrap();

    let mut diagnostic_output = DiagnosticOutput::default();
    let diagnostics = diagnostic_output.reporter(&module_src_db, module_id);

    (f(source, diagnostics), diagnostic_output)
}
