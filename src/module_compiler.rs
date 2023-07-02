use std::collections::HashMap;

use crate::lexer::token::Token;
use crate::lexer::Lexer;
use crate::parser::{ParseError, Parser};
use crate::passes::resolve_names::{Resolver, SymbolError};
use crate::source::ModulePath;
use crate::symbol_table::SymbolTable;
use crate::token_stream::TokenStream;

const FILE_EXT: &str = "ling";
const MODULE_SEP: &str = "::";

pub struct ModuleCompiler {
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
        let path = path.into_path_buf();
        let source = std::fs::read_to_string(path).ok()?;

        let lexer = Lexer::new(&source);
        let (tokens, mismatched_brackets) = TokenStream::from_lexer(lexer);

        let mut parse_errors = vec![];
        let mut parser = Parser::new(tokens.into_iter(), &mut parse_errors);

        let mut ast = parser.parse_module();

        let (symbols, name_errors) = Resolver::visit(&mut ast);

        Some(ResolvedModule {
            symbols,

            mismatched_brackets,
            parse_errors,
            name_errors,
        })
    }
}

pub struct ResolvedModule {
    symbols: SymbolTable,

    // TODO: handle errors better. Maybe have diagnostics at this point.
    mismatched_brackets: Vec<Token>,
    parse_errors: Vec<ParseError>,
    name_errors: Vec<SymbolError>,
}
