pub mod lexer;
pub mod span;
pub mod token;

use std::str::Chars;

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

#[derive(Debug)]
pub(crate) struct Cursor<'a> {
    string: &'a str,
    chars: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(string: &'a str) -> Self {
        Self {
            string,
            chars: string.chars(),
        }
    }

    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn eat(&mut self, c: char) -> bool {
        if self.peek() == Some(c) {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn as_str_all(&self) -> &'a str {
        self.string
    }

    pub fn as_str_remaining(&self) -> &'a str {
        self.chars.as_str()
    }

    pub fn byte_pos(&self) -> usize {
        self.string.len() - self.chars.as_str().len()
    }
}

impl Iterator for Cursor<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.chars.size_hint()
    }
}
