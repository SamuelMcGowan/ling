mod ast;
mod chunk;
mod constants;
mod diagnostic;
mod lexer;
mod module_compiler;
mod parser;
mod passes;
mod source;
mod symbol_table;
mod token_tree;
mod value;

use anyhow::{bail, Context, Result};
use codespan_reporting::files::Files;

use crate::diagnostic::DiagnosticOutput;
use crate::lexer::Lexer;
use crate::passes::resolve_names::Resolver;
use crate::source::{ModulePath, SourceDb};

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();

    match args.as_slice() {
        [path] => {
            let source = std::fs::read_to_string(path).context("couldn't read input file")?;
            run_source("app", &source);
        }
        _ => bail!("expected one argument `path`"),
    }

    Ok(())
}

// TODO: use module compiler
fn run_source(name: &str, source: &str) {
    use ron::ser::{to_string_pretty, PrettyConfig};

    let mut source_db = SourceDb::new("");

    let source_id = source_db.add(ModulePath::root(name), source);
    let source = source_db.source(source_id).unwrap();

    let mut diagnostic_output = DiagnosticOutput::default();
    let mut diagnostics = diagnostic_output.reporter(&source_db, source_id);

    let lexer = Lexer::new(source);
    let tokens = token_tree::TokenList::from_lexer(lexer, diagnostics.borrow());

    let mut parser = parser::Parser::new(tokens.into_iter(), diagnostics);
    let mut ast = parser.parse_module();

    let (symbols, name_errors) = Resolver::visit(&mut ast);

    let ast_ron = to_string_pretty(&ast, PrettyConfig::default()).unwrap();
    println!("AST: {ast_ron}",);
    println!("SYMBOLS: {symbols:#?}\n");

    println!("NAME ERRORS: {name_errors:?}");
}
