use std::fmt;

use ustr::Ustr;

const ROOT_NAME: &str = "$";
const SUPER_NAME: &str = "super";
const MODULE_SEP: &str = "::";

#[derive(Debug, Clone)]
pub struct ModulePath {
    pub(crate) is_absolute: bool,
    pub(crate) parents: Vec<ModulePathComponent>,
    pub(crate) name: Ustr,
}

impl ModulePath {
    pub fn root(name: impl Into<Ustr>) -> Self {
        Self {
            is_absolute: true,
            parents: vec![],
            name: name.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum ModulePathComponent {
    Named(Ustr),
    Super,
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_absolute {
            write!(f, "{ROOT_NAME}")?;
        }

        for parent in &self.parents {
            write!(f, "{parent}{MODULE_SEP}")?;
        }

        write!(f, "{}", self.name)?;

        Ok(())
    }
}

impl fmt::Display for ModulePathComponent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Named(name) => write!(f, "{name}"),
            Self::Super => write!(f, "{SUPER_NAME}"),
        }
    }
}
