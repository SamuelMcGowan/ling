use ustr::Ustr;

#[derive(Debug, Clone, Copy, serde::Serialize)]
pub(crate) struct SymbolId(usize);

#[derive(Default, Debug)]
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

#[derive(Debug)]
pub(crate) enum Symbol {
    Function { ident: Ustr },
    Var { ident: Ustr },
}

impl Symbol {
    pub fn ident(&self) -> Ustr {
        match self {
            Self::Function { ident, .. } => *ident,
            Self::Var { ident, .. } => *ident,
        }
    }
}
