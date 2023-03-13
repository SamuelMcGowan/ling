pub(crate) mod syntax;

pub(crate) mod chunk;
mod constants;
pub(crate) mod value;

use anyhow::{bail, Context, Result};

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
    let lexer = syntax::lexer::Lexer::new(source);
    // let tokens = lexer.by_ref().collect();

    // println!("TOKENS");
    // for token in &tokens {
    //     println!("  {token:?}");
    // }

    // println!("CONSTANTS");
    // for (i, constant) in lexer.constants().iter().enumerate() {
    //     println!("  {i}\t{constant:?}");
    // }

    let (token_stream, mismatched_brackets) = syntax::token_stream::TokenStream::from_lexer(lexer);
    let tokens = token_stream.into_iter();

    let mut errors = vec![];
    let mut parser = syntax::parser2::Parser::new(tokens, &mut errors);

    let ast = parser.parse_module();

    println!("MISMATCHED BRACKETS: {mismatched_brackets:#?}");
    println!("AST: {ast:#?}");
    println!("ERRORS: {errors:#?}");
}
