use std::fmt;
use std::path::PathBuf;

use ustr::Ustr;

const FILE_EXT: &str = "ling";
const MODULE_SEP: &str = "::";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModulePath {
    name: Ustr,
    parents: Ustr,
}

impl ModulePath {
    pub fn new<'a>(name: &'a str, parents: impl Iterator<Item = &'a str>) -> Self {
        let mut parents_str = String::new();
        for part in parents {
            parents_str.push_str("::");
            parents_str.push_str(part);
        }

        Self {
            name: name.into(),
            parents: Ustr::from(&parents_str),
        }
    }

    pub fn into_path_buf(self) -> PathBuf {
        let mut path = PathBuf::new();

        for segment in self.parents.split(MODULE_SEP) {
            path.push(segment);
        }

        path.push(format!("{}.{FILE_EXT}", self.name));

        path
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{MODULE_SEP}{}", self.parents, self.name)
    }
}
