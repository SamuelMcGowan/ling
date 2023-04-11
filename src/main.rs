pub(crate) mod syntax;

pub(crate) mod chunk;
mod constants;
pub mod passes;
pub(crate) mod symbol_table;
pub(crate) mod value;

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

    let lexer = syntax::lexer::Lexer::new(source);
    let (tokens, mismatched_brackets) = syntax::token_stream::TokenStream::from_lexer(lexer);

    let mut errors = vec![];
    let mut parser = syntax::parser::Parser::new(tokens.into_iter(), &mut errors);

    let mut ast = parser.parse_module();

    let (symbols, name_errors) = Resolver::visit(&mut ast);

    let ast_ron = to_string_pretty(&ast, PrettyConfig::default()).unwrap();
    println!("AST: {ast_ron}",);
    println!("SYMBOLS: {symbols:#?}\n");

    println!("MISMATCHED BRACKETS: {mismatched_brackets:#?}");
    println!("NAME ERRORS: {name_errors:?}");
    println!("ERRORS: {errors:#?}");
}
