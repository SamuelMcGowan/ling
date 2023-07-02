use std::collections::HashMap;

use ustr::Ustr;

use crate::lexer::token::Token;
use crate::lexer::Lexer;
use crate::parser::{ParseError, Parser};
use crate::passes::resolve_names::{Resolver, SymbolError};
use crate::symbol_table::SymbolTable;
use crate::token_stream::TokenStream;

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
        let source = std::fs::read_to_string(path.0.as_str()).ok()?;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModulePath(Ustr);

impl ModulePath {
    pub fn new<'a>(parts: impl Iterator<Item = &'a str>) -> Self {
        let mut s = String::new();
        for part in parts {
            s.push_str(part);
        }
        Self(Ustr::from(&s))
    }
}

pub struct ResolvedModule {
    symbols: SymbolTable,

    // TODO: handle errors better. Maybe have diagnostics at this point.
    mismatched_brackets: Vec<Token>,
    parse_errors: Vec<ParseError>,
    name_errors: Vec<SymbolError>,
}
