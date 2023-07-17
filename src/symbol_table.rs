use std::collections::hash_map::Entry;
use std::collections::HashMap;

use serde::Serialize;
use ustr::Ustr;

use crate::ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub(crate) struct SymbolId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[must_use]
pub(crate) enum GlobalShadowed {
    Yes,
    No,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub enum GlobalScope {
    Builtin,
    Global,
}

#[derive(Default, Debug, Serialize)]
pub(crate) struct SymbolTable {
    symbols: Vec<Symbol>,

    #[serde(serialize_with = "crate::utils::ordered_map")]
    globals: HashMap<Ustr, (GlobalScope, SymbolId)>,
}

impl SymbolTable {
    pub fn add(&mut self, symbol: Symbol) -> SymbolId {
        let id = self.symbols.len();
        self.symbols.push(symbol);
        SymbolId(id)
    }

    /// Returns `None` if global is already defined.
    pub fn add_global(&mut self, symbol: Symbol, scope: GlobalScope) -> (SymbolId, GlobalShadowed) {
        let ident = symbol.ident;
        let id = SymbolId(self.symbols.len());

        self.symbols.push(symbol);

        let shadowed = match self.globals.entry(ident) {
            Entry::Occupied(mut occupied) => {
                let &(shadowed_scope, _) = occupied.get();
                if scope > shadowed_scope {
                    occupied.insert((scope, id));
                    GlobalShadowed::No
                } else {
                    GlobalShadowed::Yes
                }
            }
            Entry::Vacant(vacant) => {
                vacant.insert((scope, id));
                GlobalShadowed::No
            }
        };

        (id, shadowed)
    }

    pub fn get_global(&self, ident: Ustr) -> Option<SymbolId> {
        self.globals.get(&ident).map(|&(_, id)| id)
    }

    pub fn get(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(id.0)
    }

    pub fn get_mut(&mut self, id: SymbolId) -> Option<&mut Symbol> {
        self.symbols.get_mut(id.0)
    }
}

#[derive(Debug, Serialize)]
pub(crate) struct Symbol {
    pub ident: Ustr,
    pub kind: SymbolKind,
}

#[derive(Debug, Serialize)]
pub(crate) enum SymbolKind {
    Function(Def<ast::Func>),
    TyStruct(Def<ast::Struct>),
    TyEnum(Def<ast::Enum>),

    Var,

    TyParam,
}

impl SymbolKind {
    pub fn is_value(&self) -> bool {
        matches!(self, Self::Var | Self::Function(_))
    }
}

#[derive(Debug, Serialize)]
pub enum Def<T> {
    Ast(T),
    Builtin,
    Undefined,
}
