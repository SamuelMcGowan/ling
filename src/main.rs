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
    let mut lexer = syntax::lexer::Lexer::new(source);

    println!("TOKENS");
    for token in lexer.by_ref() {
        println!("  {token:?}");
    }

    println!("CONSTANTS");
    for (i, constant) in lexer.constants().iter().enumerate() {
        println!("  {i}\t{constant:?}");
    }
}
