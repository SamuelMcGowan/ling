use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::path::PathBuf;

use ustr::Ustr;

use crate::ast::Module;
use crate::symbol_table::SymbolTable;

#[derive(Debug)]
pub(crate) struct ModuleTree {
    modules: Vec<ModuleCompiled>,
    root_dir: PathBuf,
}

impl ModuleTree {
    pub fn new(root_dir: impl Into<PathBuf>) -> Self {
        Self {
            modules: vec![ModuleCompiled {
                parent: None,
                children: HashMap::new(),
                state: ModuleState::None,
            }],
            root_dir: root_dir.into(),
        }
    }

    pub fn root_id(&self) -> ModuleId {
        ModuleId(0)
    }

    pub fn insert(&mut self, parent_id: ModuleId, name: Ustr) -> Option<ModuleId> {
        let id = ModuleId(self.modules.len());

        let parent = self.modules.get_mut(parent_id.0)?;
        match parent.children.entry(name) {
            Entry::Occupied(_) => return None,
            Entry::Vacant(vacant) => {
                vacant.insert(id);
            }
        }

        self.modules.push(ModuleCompiled {
            parent: Some(parent_id),
            children: HashMap::new(),

            state: ModuleState::None,
        });

        Some(id)
    }

    pub fn get(&self, source_id: ModuleId) -> Option<&ModuleCompiled> {
        self.modules.get(source_id.0)
    }

    pub fn get_mut(&mut self, source_id: ModuleId) -> Option<&mut ModuleCompiled> {
        self.modules.get_mut(source_id.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ModuleId(usize);

#[derive(Debug)]
pub(crate) struct ModuleCompiled {
    parent: Option<ModuleId>,
    children: HashMap<Ustr, ModuleId>,

    pub state: ModuleState,
}

impl ModuleCompiled {
    pub fn parent(&self) -> Option<ModuleId> {
        self.parent
    }

    pub fn children(&self) -> impl Iterator<Item = (Ustr, ModuleId)> + '_ {
        self.children.iter().map(|(&name, &id)| (name, id))
    }
}

#[derive(Debug)]
pub(crate) enum ModuleState {
    None,
    Parsed(Module),
    Resolved(SymbolTable),
}
