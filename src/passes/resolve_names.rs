use std::collections::hash_map::Entry;
use std::collections::HashMap;

use ustr::Ustr;

use super::visitor::Visitor;
use crate::ast::*;
use crate::diagnostic::{DiagnosticReporter, SymbolError};
use crate::symbol_table::{Def, Symbol, SymbolId, SymbolKind, SymbolTable};

pub(crate) struct Resolver<'a> {
    table: SymbolTable,

    builtins: HashMap<Ustr, SymbolEntry>,
    globals: HashMap<Ustr, SymbolEntry>,

    locals: Vec<SymbolEntry>,
    scopes: Vec<usize>,

    diagnostics: DiagnosticReporter<'a>,
}

impl<'a> Resolver<'a> {
    pub fn new(diagnostics: DiagnosticReporter<'a>) -> Self {
        let mut r = Self {
            table: SymbolTable::default(),

            builtins: HashMap::new(),
            globals: HashMap::new(),
            locals: vec![],

            scopes: vec![],

            diagnostics,
        };

        r.declare_builtin("int", SymbolKind::TyStruct(Def::Builtin));
        r.declare_builtin("uint", SymbolKind::TyStruct(Def::Builtin));

        r
    }

    pub fn visit(ast: &mut Module, diagnostics: DiagnosticReporter<'a>) -> SymbolTable {
        let mut resolver = Self::new(diagnostics);
        resolver.visit_module(ast);
        resolver.table
    }

    fn declare_builtin(&mut self, ident: impl Into<Ustr>, kind: SymbolKind) {
        let ident = ident.into();
        let entry = self.table.add_and_get_entry(ident, kind);
        self.builtins.insert(ident, entry);
    }

    #[must_use]
    fn declare_global(&mut self, ident: Ustr, kind: SymbolKind) -> Ident {
        let symbol_entry = self.table.add_and_get_entry(ident, kind);

        match self.globals.entry(ident) {
            Entry::Occupied(_) => {
                self.diagnostics.report(SymbolError::GlobalShadowed(ident));
            }
            Entry::Vacant(vacant) => {
                vacant.insert(symbol_entry);
            }
        }

        Ident::Resolved(symbol_entry.symbol_id)
    }

    #[must_use]
    fn declare_local(&mut self, ident: Ustr, kind: SymbolKind) -> Ident {
        let entry = self.table.add_and_get_entry(ident, kind);
        self.locals.push(entry);

        Ident::Resolved(entry.symbol_id)
    }

    #[must_use]
    fn resolve(&mut self, ident: Ustr, value: bool) -> Ident {
        let Some(entry) = self.resolve_entry(ident) else {
            self.diagnostics.report(SymbolError::SymbolNotFound(ident));
            return Ident::Unresolved(ident);
        };

        let symbol_id = entry.symbol_id;

        if entry.is_value != value {
            self.diagnostics.report(SymbolError::WrongKind {
                ident,
                should_be_value: value,
            });
        }

        Ident::Resolved(symbol_id)
    }

    fn resolve_entry(&self, ident: Ustr) -> Option<&SymbolEntry> {
        let find_local_entry = || self.locals.iter().rev().find(|entry| entry.ident == ident);
        find_local_entry()
            .or_else(|| self.globals.get(&ident))
            .or_else(|| self.builtins.get(&ident))
    }

    fn push_scope(&mut self) {
        self.scopes.push(self.locals.len());
    }

    fn pop_scope(&mut self) {
        let start_len = self.scopes.pop().unwrap();
        self.locals.truncate(start_len);
    }
}

impl SymbolTable {
    fn add_and_get_entry(&mut self, ident: Ustr, kind: SymbolKind) -> SymbolEntry {
        let is_value = kind.is_value();

        let symbol_id = self.add(Symbol { ident, kind });

        SymbolEntry {
            ident,
            symbol_id,
            is_value,
        }
    }
}

impl Resolver<'_> {
    fn define_item(&mut self, id: SymbolId, kind: SymbolKind) {
        let symbol = self.table.get_mut(id).unwrap();
        symbol.kind = kind;
    }

    fn declare_ty_params(&mut self, ty_params: &mut [Ident]) {
        for ident in ty_params {
            *ident = self.declare_local(ident.unresolved().unwrap(), SymbolKind::TyParam);
        }
    }
}

impl Visitor for Resolver<'_> {
    fn walk_module(&mut self, module: &mut Module) {
        // forward declare
        for item in &mut module.items {
            let (ident, kind) = match item {
                Item::Func(func) => (&mut func.ident, SymbolKind::Function(Def::Undefined)),
                Item::Struct(strukt) => (&mut strukt.ident, SymbolKind::TyStruct(Def::Undefined)),
                Item::Enum(eenum) => (&mut eenum.ident, SymbolKind::TyEnum(Def::Undefined)),
                Item::Dummy => unreachable!(),
            };

            *ident = self.declare_global(ident.unresolved().unwrap(), kind);
        }

        for item in &mut module.items {
            // visit
            self.visit_item(item);

            // define, moving AST into symbol tree
            let item_owned = std::mem::take(item);
            let (ident, kind) = match item_owned {
                Item::Func(func) => (func.ident, SymbolKind::Function(Def::Ast(func))),
                Item::Struct(strukt) => (strukt.ident, SymbolKind::TyStruct(Def::Ast(strukt))),
                Item::Enum(eenum) => (eenum.ident, SymbolKind::TyEnum(Def::Ast(eenum))),
                Item::Dummy => unreachable!(),
            };
            self.define_item(ident.resolved().unwrap(), kind);
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
    use insta::assert_ron_snapshot;

    use super::Resolver;
    use crate::diagnostic::DiagnosticOutput;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::source::db::with_test_module;
    use crate::symbol_table::SymbolTable;
    use crate::token_tree::TokenList;

    fn test_resolve(source: &str) -> (SymbolTable, DiagnosticOutput) {
        with_test_module(source, |source, mut diagnostics| {
            let lexer = Lexer::new(source);
            let tokens = TokenList::from_lexer(lexer, diagnostics.borrow());

            let mut parser = Parser::new(tokens.into_iter(), diagnostics.borrow());
            let mut ast = parser.parse_module();

            if diagnostics.had_errors() {
                // TODO: pass symbol to resolver rather than waiting for it to return one
                return SymbolTable::default();
            }

            Resolver::visit(&mut ast, diagnostics)
        })
    }

    #[test]
    fn simple() {
        assert_ron_snapshot!(test_resolve("func foo() {}"));
    }

    #[test]
    fn shadowed_global() {
        assert_ron_snapshot!(test_resolve("func foo() { foo() } func foo() { foo() }"));
    }

    #[test]
    fn shadowed_local() {
        assert_ron_snapshot!(test_resolve("func foo() { let a = 12; let a = 12; }"));
    }

    #[test]
    fn shadowed_global_shadowed_local() {
        assert_ron_snapshot!(test_resolve(
            "func foo() { let a = 12; let a = 12; } func foo() {}"
        ));
    }

    #[test]
    fn local_shadowing_global() {
        assert_ron_snapshot!(test_resolve("func foo() { let foo = 12; let a = foo; }"))
    }

    #[test]
    fn global_shadowing_builtin() {
        assert_ron_snapshot!(test_resolve("func uint() { uint() }"))
    }

    #[test]
    fn forward_declaration() {
        assert_ron_snapshot!(test_resolve("func a() { a(); b(); } func b() {}"))
    }

    #[test]
    fn argument() {
        assert_ron_snapshot!(test_resolve("func foo(a: uint) { a } func a() {}"));
    }

    #[test]
    fn argument_shadowed() {
        assert_ron_snapshot!(test_resolve("func foo(a: uint) { let a = 12; }"))
    }

    #[test]
    fn func_generic() {
        assert_ron_snapshot!(test_resolve("func foo[A, B]() {}"));
    }
    #[test]
    fn strukt() {
        assert_ron_snapshot!(test_resolve("data Person { name: string, age: uint }"));
    }

    #[test]
    fn eenum() {
        assert_ron_snapshot!(test_resolve("enum Result[T, E] { Ok(T), Err(E), }"));
    }

    #[test]
    fn tuple() {
        assert_ron_snapshot!(test_resolve("func a() -> (uint, uint) {}"));
    }

    #[test]
    fn assign_to_generic() {
        assert_ron_snapshot!(test_resolve("func foo[A]() { A = 12; }"));
    }

    #[test]
    fn resolution() {
        assert_ron_snapshot!(test_resolve("func foo(a: uint) { let b = a; let c = b; }"));
    }

    #[test]
    fn scopes() {
        assert_ron_snapshot!(test_resolve("func foo() { loop { let a = 12; } a;}"));
    }

    #[test]
    fn unknown_var() {
        assert_ron_snapshot!(test_resolve("func foo() { bloop() }"))
    }
}
