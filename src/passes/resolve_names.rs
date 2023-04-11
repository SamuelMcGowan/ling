use ustr::Ustr;

use crate::symbol_table::{Symbol, SymbolId, SymbolTable};
use crate::syntax::ast::*;

use super::visitor::Visitor;

#[derive(Debug)]
pub(crate) enum SymbolError {
    SymbolNotFound(Ustr),
    SymbolInUse(Ustr),
}

#[derive(Default)]
pub(crate) struct Resolver {
    table: SymbolTable,

    stack: Vec<SymbolEntry>,
    scopes: Vec<usize>,

    errors: Vec<SymbolError>,
}

impl Resolver {
    pub fn visit(ast: &mut Module) -> (SymbolTable, Vec<SymbolError>) {
        let mut resolver = Self::default();
        resolver.visit_module(ast);
        (resolver.table, resolver.errors)
    }

    fn declare_symbol(&mut self, symbol: Symbol) -> Ident {
        let ident = symbol.ident();

        if self.scopes.is_empty() && self.stack.iter().any(|sym| sym.ident == ident) {
            self.errors.push(SymbolError::SymbolInUse(ident));
            return Ident::Unresolved(ident);
        }

        let symbol_id = self.table.add(symbol);
        self.stack.push(SymbolEntry { ident, symbol_id });

        Ident::Resolved(symbol_id)
    }

    fn resolve_ident(&mut self, ident: Ustr) -> Ident {
        for entry in self.stack.iter().rev() {
            if entry.ident == ident {
                return Ident::Resolved(entry.symbol_id);
            }
        }
        self.errors.push(SymbolError::SymbolNotFound(ident));
        Ident::Unresolved(ident)
    }

    fn push_scope(&mut self) {
        self.scopes.push(self.stack.len());
    }

    fn pop_scope(&mut self) {
        let old_len = self.scopes.pop().unwrap();
        self.stack.truncate(old_len);
    }
}

impl Visitor for Resolver {
    fn visit_func(&mut self, func: &mut Func) {
        func.ident = self.declare_symbol(Symbol::Function {
            ident: func.ident.unresolved().unwrap(),
        });

        self.push_scope();

        for (ident, _ty) in &mut func.params {
            *ident = self.declare_symbol(Symbol::Var {
                ident: ident.unresolved().unwrap(),
            });
        }
        self.visit_block(&mut func.body);

        self.pop_scope();
    }

    fn visit_block(&mut self, block: &mut Block) {
        self.push_scope();
        self.walk_block(block);
        self.pop_scope();
    }

    fn visit_var(&mut self, var: &mut Var) {
        match var {
            Var::Simple(ident) => *ident = self.resolve_ident(ident.unresolved().unwrap()),
            Var::Field { expr: _, field: _ } => {
                // TODO
            }
        }
    }

    fn visit_declaration(&mut self, lhs: &mut Ident, rhs: &mut Expr) {
        // visit expression first so that identifier can't appear in expression.
        self.visit_expr(rhs);
        *lhs = self.declare_symbol(Symbol::Var {
            ident: lhs.unresolved().unwrap(),
        });
    }
}

#[derive(Debug)]
struct SymbolEntry {
    ident: Ustr,
    symbol_id: SymbolId,
}
