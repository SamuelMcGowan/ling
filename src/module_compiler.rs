use std::collections::HashMap;

use codespan_reporting::files::Files;

use crate::diagnostic::DiagnosticOutput;
use crate::lexer::Lexer;
use crate::source::{ModulePath, ModuleSourceDb};
use crate::parser::Parser;
use crate::passes::resolve_names::Resolver;
use crate::symbol_table::SymbolTable;
use crate::token_tree::TokenList;

const FILE_EXT: &str = "ling";
const MODULE_SEP: &str = "::";

pub struct ModuleCompiler {
    module_src_db: ModuleSourceDb,
    diagnostics: DiagnosticOutput,

    modules: HashMap<ModulePath, ResolvedModule>,
}

impl ModuleCompiler {
    pub fn get(&mut self, path: ModulePath) -> Option<&ResolvedModule> {
        self.get_mut(path).map(|module| &*module)
    }

    pub fn get_mut(&mut self, path: ModulePath) -> Option<&mut ResolvedModule> {
        // Can't use entry due to borrowing self mutably.
        #[allow(clippy::map_entry)]
        if !self.modules.contains_key(&path) {
            let module = self.compile_module(path)?;
            self.modules.insert(path, module);
        }

        self.modules.get_mut(&path)
    }

    fn compile_module(&mut self, path: ModulePath) -> Option<ResolvedModule> {
        let module_id = self.module_src_db.load(path)?;
        let source = self.module_src_db.source(module_id).unwrap();

        let mut diagnostics = self.diagnostics.reporter(&self.module_src_db, module_id);

        let lexer = Lexer::new(source);
        let tokens = TokenList::from_lexer(lexer, diagnostics.borrow());

        let mut parser = Parser::new(tokens.into_iter(), diagnostics.borrow());
        let mut ast = parser.parse_module();

        if diagnostics.had_errors() {
            return None;
        }

        let symbols = Resolver::visit(&mut ast, diagnostics);

        Some(ResolvedModule { symbols })
    }
}

pub struct ResolvedModule {
    symbols: SymbolTable,
}
