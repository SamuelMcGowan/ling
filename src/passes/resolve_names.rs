use ustr::Ustr;

use super::visitor::Visitor;
use crate::ast::*;
use crate::symbol_table::{Symbol, SymbolId, SymbolKind, SymbolTable};

#[derive(Debug)]
pub(crate) enum SymbolError {
    SymbolNotFound(Ustr),
    GlobalShadowed(Ustr),
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

        r.declare_builtin("int", SymbolKind::TyStruct);
        r.declare_builtin("uint", SymbolKind::TyStruct);

        r
    }

    pub fn visit(ast: &mut Module) -> (SymbolTable, Vec<SymbolError>) {
        let mut resolver = Self::new();
        resolver.visit_module(ast);
        (resolver.table, resolver.errors)
    }

    fn declare(&mut self, ident: Ustr, kind: SymbolKind) -> SymbolEntry {
        let is_value = kind.is_value();

        let symbol_id = self.table.add(Symbol { ident, kind });

        SymbolEntry {
            ident,
            symbol_id,
            is_value,
        }
    }

    fn declare_builtin(&mut self, ident: impl Into<Ustr>, kind: SymbolKind) {
        let entry = self.declare(ident.into(), kind);
        self.builtins.push(entry);
    }

    #[must_use]
    fn declare_global(&mut self, ident: Ustr, kind: SymbolKind) -> Ident {
        // check a global isn't already declared with this name.
        if self.globals.iter().any(|entry| entry.ident == ident) {
            self.errors.push(SymbolError::GlobalShadowed(ident));
            return Ident::Unresolved(ident);
        }

        let entry = self.declare(ident, kind);
        self.globals.push(entry);

        Ident::Resolved(entry.symbol_id)
    }

    #[must_use]
    fn declare_local(&mut self, ident: Ustr, kind: SymbolKind) -> Ident {
        let entry = self.declare(ident, kind);
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

impl Resolver {
    fn forward_declare_item(&mut self, item: &mut Item) {
        match item {
            Item::Func(func) => {
                func.ident =
                    self.declare_global(func.ident.unresolved().unwrap(), SymbolKind::Function);
            }
            Item::Struct(strukt) => {
                strukt.ident =
                    self.declare_global(strukt.ident.unresolved().unwrap(), SymbolKind::TyStruct);
            }
            Item::Enum(eenum) => {
                eenum.ident =
                    self.declare_global(eenum.ident.unresolved().unwrap(), SymbolKind::TyEnum)
            }
            Item::Dummy => {}
        }
    }

    fn declare_ty_params(&mut self, ty_params: &mut [Ident]) {
        for ident in ty_params {
            *ident = self.declare_local(ident.unresolved().unwrap(), SymbolKind::TyParam);
        }
    }
}

impl Visitor for Resolver {
    fn walk_module(&mut self, module: &mut Module) {
        for item in &mut module.items {
            self.forward_declare_item(item);
        }

        for item in &mut module.items {
            self.visit_item(item);
        }
    }

    fn visit_func(&mut self, func: &mut Func) {
        // already declared

        self.push_scope();

        self.declare_ty_params(&mut func.ty_params);

        for (ident, ty) in &mut func.params {
            *ident = self.declare_local(ident.unresolved().unwrap(), SymbolKind::Var);
            self.visit_ty(ty);
        }
        self.visit_ty(&mut func.ret_ty);

        self.visit_block(&mut func.body);

        self.pop_scope();
    }

    fn visit_struct(&mut self, strukt: &mut Struct) {
        // already declared

        self.push_scope();

        self.declare_ty_params(&mut strukt.ty_params);

        for field in &mut strukt.fields {
            self.visit_struct_field(field);
        }

        self.pop_scope();
    }

    fn visit_struct_field(&mut self, field: &mut StructField) {
        self.visit_ty(&mut field.ty);
    }

    fn visit_enum(&mut self, eenum: &mut Enum) {
        // already declared

        self.push_scope();

        self.declare_ty_params(&mut eenum.ty_params);

        for variant in &mut eenum.variants {
            self.visit_enum_variant(variant);
        }

        self.pop_scope();
    }

    fn visit_enum_variant(&mut self, variant: &mut EnumVariant) {
        self.visit_enum_variant_kind(&mut variant.kind);
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
        *lhs = self.declare_local(lhs.unresolved().unwrap(), SymbolKind::Var);
    }

    fn visit_ty(&mut self, ty: &mut Ty) {
        match ty {
            Ty::Tuple(fields) => {
                for field in fields {
                    self.visit_ty(field)
                }
            }
            Ty::Constructed { ident, params } => {
                *ident = self.resolve(ident.unresolved().unwrap(), false);
                for param in params {
                    self.visit_ty(param);
                }
            }
        }
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
    use crate::ast::Module;
    use crate::parser::test_parse;
    use crate::symbol_table::SymbolTable;

    fn test_resolve(source: &str) -> (Module, SymbolTable, Vec<SymbolError>) {
        let (mut ast, mismatched_brackets, parse_errors) = test_parse(source, |p| p.parse_module());

        if !mismatched_brackets.is_empty() {
            panic!("mismatched brackets: {mismatched_brackets:?}");
        }

        if !parse_errors.is_empty() {
            panic!("parse errors: {parse_errors:?}");
        }

        let (table, errors) = Resolver::visit(&mut ast);

        (ast, table, errors)
    }

    #[test]
    fn simple() {
        assert_debug_snapshot!(test_resolve("func foo() {}"));
    }

    #[test]
    fn shadowed_global() {
        assert_debug_snapshot!(test_resolve("func foo() { foo() } func foo() {}"));
    }

    #[test]
    fn shadowed_local() {
        assert_debug_snapshot!(test_resolve("func foo() { let a = 12; let a = 12; }"));
    }

    #[test]
    fn shadowed_global_shadowed_local() {
        assert_debug_snapshot!(test_resolve(
            "func foo() { let a = 12; let a = 12; } func foo() {}"
        ));
    }

    #[test]
    fn local_shadowing_global() {
        assert_debug_snapshot!(test_resolve("func foo() { let foo = 12; let a = foo; }"))
    }

    #[test]
    fn global_shadowing_builtin() {
        assert_debug_snapshot!(test_resolve("func uint() { uint() }"))
    }

    #[test]
    fn forward_declaration() {
        assert_debug_snapshot!(test_resolve("func a() { a(); b(); } func b() {}"))
    }

    #[test]
    fn argument() {
        assert_debug_snapshot!(test_resolve("func foo(a: uint) { a } func a() {}"));
    }

    #[test]
    fn argument_shadowed() {
        assert_debug_snapshot!(test_resolve("func foo(a: uint) { let a = 12; }"))
    }

    #[test]
    fn func_generic() {
        assert_debug_snapshot!(test_resolve("func foo[A, B]() {}"));
    }
    #[test]
    fn strukt() {
        assert_debug_snapshot!(test_resolve("data Person { name: string, age: uint }"));
    }

    #[test]
    fn eenum() {
        assert_debug_snapshot!(test_resolve("enum Result[T, E] { Ok(T), Err(E), }"));
    }

    #[test]
    fn tuple() {
        assert_debug_snapshot!(test_resolve("func a() -> (uint, uint) {}"));
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
    fn scopes() {
        assert_debug_snapshot!(test_resolve("func foo() { loop { let a = 12; } a;}"));
    }

    #[test]
    fn unknown_var() {
        assert_debug_snapshot!(test_resolve("func foo() { bloop() }"))
    }
}
