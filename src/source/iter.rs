use std::str::Chars;

#[derive(Debug)]
pub(crate) struct SourceIter<'a> {
    string: &'a str,
    chars: Chars<'a>,
}

impl<'a> SourceIter<'a> {
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

impl Iterator for SourceIter<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.chars.size_hint()
    }
}
