mod syntax;

mod ast;
mod chunk;
mod constants;
mod lexer;
mod parser;
mod passes;
mod symbol_table;
mod value;

use anyhow::{bail, Context, Result};

use crate::passes::resolve_names::Resolver;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().skip(1).collect();

    match args.as_slice() {
        [path] => {
            let source = std::fs::read_to_string(path).context("couldn't read input file")?;
            run_source(&source);
        }
        _ => bail!("expected one argument `path`"),
    }

    Ok(())
}

fn run_source(source: &str) {
    use ron::ser::{to_string_pretty, PrettyConfig};

    let lexer = lexer::Lexer::new(source);
    let (tokens, mismatched_brackets) = syntax::token_stream::TokenStream::from_lexer(lexer);

    let mut errors = vec![];
    let mut parser = parser::Parser::new(tokens.into_iter(), &mut errors);

    let mut ast = parser.parse_module();

    let (symbols, name_errors) = Resolver::visit(&mut ast);

    let ast_ron = to_string_pretty(&ast, PrettyConfig::default()).unwrap();
    println!("AST: {ast_ron}",);
    println!("SYMBOLS: {symbols:#?}\n");

    println!("MISMATCHED BRACKETS: {mismatched_brackets:#?}");
    println!("NAME ERRORS: {name_errors:?}");
    println!("ERRORS: {errors:#?}");
}
