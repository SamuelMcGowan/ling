pub mod lexer;
pub mod parser;
pub mod token;

pub mod source;

use crate::chunk::Chunk;

#[derive(Debug)]
pub(crate) struct ParseContext<'a> {
    pub(crate) source: &'a str,
    pub(crate) chunk: &'a mut Chunk,
}

impl<'a> ParseContext<'a> {
    pub fn new(source: &'a str, chunk: &'a mut Chunk) -> Self {
        Self { source, chunk }
    }
}
