use ustr::Ustr;

use super::visitor::Visitor;
use crate::ast::*;
use crate::symbol_table::{Symbol, SymbolId, SymbolTable};

#[derive(Debug)]
pub(crate) enum SymbolError {
    SymbolNotFound(Ustr),
    SymbolInUse(Ustr),
    WrongKind(Ustr),
}

pub(crate) struct Resolver {
    table: SymbolTable,

    builtins: Vec<SymbolEntry>,
    globals: Vec<SymbolEntry>,
    locals: Vec<SymbolEntry>,

    scopes: Vec<usize>,

    errors: Vec<SymbolError>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut r = Self {
            table: SymbolTable::default(),

            builtins: vec![],
            globals: vec![],
            locals: vec![],

            scopes: vec![],

            errors: vec![],
        };

        r.declare_builtin(Symbol::TyStruct {
            ident: "int".into(),
        });
        r.declare_builtin(Symbol::TyStruct {
            ident: "uint".into(),
        });

        r
    }

    pub fn visit(ast: &mut Module) -> (SymbolTable, Vec<SymbolError>) {
        let mut resolver = Self::new();
        resolver.visit_module(ast);
        (resolver.table, resolver.errors)
    }

    fn declare(&mut self, symbol: Symbol) -> SymbolEntry {
        let ident = symbol.ident();

        let is_value = symbol.is_value();
        let symbol_id = self.table.add(symbol);

        SymbolEntry {
            ident,
            symbol_id,
            is_value,
        }
    }

    fn declare_builtin(&mut self, symbol: Symbol) {
        let entry = self.declare(symbol);
        self.builtins.push(entry);
    }

    #[must_use]
    fn declare_global(&mut self, symbol: Symbol) -> Ident {
        // check a global isn't already declared with this name.
        let ident = symbol.ident();
        if self.globals.iter().any(|entry| entry.ident == ident) {
            self.errors.push(SymbolError::SymbolInUse(ident));
            return Ident::Unresolved(ident);
        }

        let entry = self.declare(symbol);
        self.globals.push(entry);

        Ident::Resolved(entry.symbol_id)
    }

    #[must_use]
    fn declare_local(&mut self, symbol: Symbol) -> Ident {
        let entry = self.declare(symbol);
        self.locals.push(entry);

        Ident::Resolved(entry.symbol_id)
    }

    #[must_use]
    fn resolve(&mut self, ident: Ustr, value: bool) -> Ident {
        let Some(entry) = self.resolve_entry(ident) else {
            self.errors.push(SymbolError::SymbolNotFound(ident));
            return Ident::Unresolved(ident);
        };

        if entry.is_value != value {
            self.errors.push(SymbolError::WrongKind(ident));
            return Ident::Unresolved(ident);
        }

        Ident::Resolved(entry.symbol_id)
    }

    fn resolve_entry(&self, ident: Ustr) -> Option<&SymbolEntry> {
        fn find_entry(ident: Ustr, entries: &[SymbolEntry]) -> Option<&SymbolEntry> {
            entries.iter().rev().find(|entry| entry.ident == ident)
        }

        find_entry(ident, &self.locals)
            .or_else(|| find_entry(ident, &self.globals))
            .or_else(|| find_entry(ident, &self.builtins))
    }

    fn push_scope(&mut self) {
        self.scopes.push(self.locals.len());
    }

    fn pop_scope(&mut self) {
        let start_len = self.scopes.pop().unwrap();
        self.locals.truncate(start_len);
    }
}

impl Visitor for Resolver {
    fn visit_func(&mut self, func: &mut Func) {
        func.ident = self.declare_global(Symbol::Function {
            ident: func.ident.unresolved().unwrap(),
        });

        self.push_scope();

        for ident in &mut func.ty_params {
            *ident = self.declare_local(Symbol::TyParam {
                ident: ident.unresolved().unwrap(),
            });
        }

        for (ident, ty) in &mut func.params {
            *ident = self.declare_local(Symbol::Var {
                ident: ident.unresolved().unwrap(),
            });
            self.visit_ty(ty);
        }
        self.visit_ty(&mut func.ret_ty);

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
            Var::Simple(ident) => *ident = self.resolve(ident.unresolved().unwrap(), true),
            Var::Field { expr: _, field: _ } => {
                // TODO
            }
        }
    }

    fn visit_declaration(&mut self, lhs: &mut Ident, rhs: &mut Expr) {
        // visit expression first so that identifier can't appear in expression.
        self.visit_expr(rhs);
        *lhs = self.declare_local(Symbol::Var {
            ident: lhs.unresolved().unwrap(),
        });
    }

    fn visit_ty(&mut self, ty: &mut Ty) {
        match ty {
            Ty::Unit => {}
            Ty::Constructed { ident, params } => {
                *ident = self.resolve(ident.unresolved().unwrap(), false);
                for param in params {
                    self.visit_ty(param);
                }
            }
        }
    }
}

impl Symbol {
    fn is_value(&self) -> bool {
        matches!(self, Symbol::Var { .. } | Symbol::Function { .. })
    }
}

#[derive(Debug, Clone, Copy)]
struct SymbolEntry {
    ident: Ustr,
    symbol_id: SymbolId,
    is_value: bool,
}

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;

    use super::{Resolver, SymbolError};
    use crate::parser::test_parse;
    use crate::symbol_table::SymbolTable;

    fn test_resolve(source: &str) -> (SymbolTable, Vec<SymbolError>) {
        let (mut ast, mismatched_brackets, parse_errors) = test_parse(source, |p| p.parse_module());

        if !mismatched_brackets.is_empty() {
            panic!("mismatched brackets: {mismatched_brackets:?}");
        }

        if !parse_errors.is_empty() {
            panic!("parse errors: {parse_errors:?}");
        }

        Resolver::visit(&mut ast)
    }

    #[test]
    fn simple() {
        assert_debug_snapshot!(test_resolve("func foo() {}"));
    }

    #[test]
    fn duplicate_global() {
        assert_debug_snapshot!(test_resolve("func foo() { foo() } func foo() {}"));
    }

    #[test]
    fn shadowed_local() {
        assert_debug_snapshot!(test_resolve("func foo() { let a = 12; let a = 12; }"));
    }

    #[test]
    fn duplicate_global_shadowed_local() {
        assert_debug_snapshot!(test_resolve(
            "func foo() { let a = 12; let a = 12; } func foo() {}"
        ));
    }

    #[test]
    fn global_shadowing_builtin() {
        assert_debug_snapshot!(test_resolve("func uint() { uint() }"))
    }

    #[test]
    fn argument() {
        assert_debug_snapshot!(test_resolve("func foo(a: uint) { a } func a() {}"));
    }

    #[test]
    fn shadow_argument() {
        assert_debug_snapshot!(test_resolve("func foo(a: uint) { let a = 12; }"))
    }

    #[test]
    fn func_generic() {
        assert_debug_snapshot!(test_resolve("func foo[A, B]() {}"));
    }

    #[test]
    fn assign_to_generic() {
        assert_debug_snapshot!(test_resolve("func foo[A]() { A = 12; }"));
    }

    #[test]
    fn resolution() {
        assert_debug_snapshot!(test_resolve("func foo(a: uint) { let b = a; let c = b; }"));
    }

    #[test]
    fn unknown_var() {
        assert_debug_snapshot!(test_resolve("func foo() { bloop() }"))
    }
}
