use serde::Serialize;
use ustr::Ustr;

use crate::ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub(crate) struct SymbolId(usize);

#[derive(Default, Debug, Serialize)]
pub(crate) struct SymbolTable {
    symbol_lookup: Vec<Symbol>,
}

impl SymbolTable {
    pub fn add(&mut self, symbol: Symbol) -> SymbolId {
        let id = self.symbol_lookup.len();
        self.symbol_lookup.push(symbol);
        SymbolId(id)
    }

    pub fn get(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbol_lookup.get(id.0)
    }

    pub fn get_mut(&mut self, id: SymbolId) -> Option<&mut Symbol> {
        self.symbol_lookup.get_mut(id.0)
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
