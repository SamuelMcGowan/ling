pub(crate) mod parser;

pub(crate) mod chunk;
pub(crate) mod value;

use anyhow::{bail, Context, Result};
use chunk::Chunk;

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
    let mut chunk = Chunk::default();
    let mut context = parser::ParseContext::new(source, &mut chunk);

    println!("TOKENS:");
    for token in context.lex_tokens() {
        println!("  {token:?}");
    }

    println!("\n{chunk}");
}
