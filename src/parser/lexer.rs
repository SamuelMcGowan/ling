use super::span::Span;
use super::token::{tkind, Token, TokenKind};
use super::ParseContext;

impl<'a> ParseContext<'a> {
    pub fn lex_token(&mut self) -> Option<Token> {
        loop {
            let start_pos = self.cursor.byte_pos();
            let c = self.cursor.next()?;

            let kind = match c {
                c if c.is_ascii_whitespace() => continue,
                '#' => {
                    while !matches!(self.cursor.next(), Some('\n') | None) {}
                    continue;
                }

                '(' => tkind!(punct LParen),
                ')' => tkind!(punct RParen),
                '{' => tkind!(punct LBrace),
                '}' => tkind!(punct RBrace),
                '[' => tkind!(punct LBracket),
                ']' => tkind!(punct RBracket),

                '.' => tkind!(punct Dot),
                ',' => tkind!(punct Comma),
                ':' => tkind!(punct Colon),
                ';' => tkind!(punct Semicolon),

                '-' if self.cursor.eat('>') => tkind!(punct Arrow),
                '=' if self.cursor.eat('>') => tkind!(punct FatArrow),

                '+' if self.cursor.eat('=') => tkind!(punct AddEqual),
                '-' if self.cursor.eat('=') => tkind!(punct SubEqual),
                '*' if self.cursor.eat('=') => tkind!(punct MulEqual),
                '/' if self.cursor.eat('=') => tkind!(punct DivEqual),
                '%' if self.cursor.eat('=') => tkind!(punct ModEqual),
                '^' if self.cursor.eat('=') => tkind!(punct PowEqual),

                '+' => tkind!(punct Add),
                '-' => tkind!(punct Sub),
                '*' => tkind!(punct Mul),
                '/' => tkind!(punct Div),
                '%' => tkind!(punct Mod),
                '^' => tkind!(punct Pow),

                '>' if self.cursor.eat('=') => tkind!(punct GtEqual),
                '<' if self.cursor.eat('=') => tkind!(punct LtEqual),
                '=' if self.cursor.eat('=') => tkind!(punct EqualEqual),

                '>' => tkind!(punct Gt),
                '<' => tkind!(punct Lt),
                '=' => tkind!(punct Equal),

                _ if is_ident_start(c) => self.lex_ident(start_pos),

                _ => tkind!(error UnexpectedChar(c)),
            };

            let span = Span::new(start_pos, self.cursor.byte_pos());

            return Some(Token { span, kind });
        }
    }

    fn lex_ident(&mut self, start_pos: usize) -> TokenKind {
        while matches!(self.cursor.peek(), Some(c) if is_ident(c)) {
            self.cursor.next();
        }

        let lexeme = &self.cursor.as_str_all()[start_pos..self.cursor.byte_pos()];

        match lexeme {
            "func" => tkind!(kwd Func),
            "return" => tkind!(kwd Return),

            "type" => tkind!(kwd Type),
            "enum" => tkind!(kwd Enum),
            "data" => tkind!(kwd Data),
            "typeclass" => tkind!(kwd Typeclass),
            "impl" => tkind!(kwd Impl),

            "let" => tkind!(kwd Let),
            "match" => tkind!(kwd Match),

            "if" => tkind!(kwd If),
            "elif" => tkind!(kwd Elif),
            "else" => tkind!(kwd Else),

            "loop" => tkind!(kwd Loop),
            "while" => tkind!(kwd While),

            _ => tkind!(ident lexeme),
        }
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn punct_single() {
        check_tokens(
            "+-:;",
            &[
                tkind!(punct Add),
                tkind!(punct Sub),
                tkind!(punct Colon),
                tkind!(punct Semicolon),
            ],
        );
    }

    #[test]
    fn punct_double() {
        check_tokens(
            "->==+=",
            &[
                tkind!(punct Arrow),
                tkind!(punct EqualEqual),
                tkind!(punct AddEqual),
            ],
        );
    }

    #[test]
    fn whitespace() {
        check_tokens(" .\r\n. ", &[tkind!(punct Dot), tkind!(punct Dot)]);
    }

    #[test]
    fn comment() {
        check_tokens(
            ".# comment\r\n.# comment",
            &[tkind!(punct Dot), tkind!(punct Dot)],
        );
    }

    #[test]
    fn comment_empty() {
        check_tokens(".#\r\n.#", &[tkind!(punct Dot), tkind!(punct Dot)]);
    }

    #[test]
    fn keyword() {
        check_tokens("else", &[tkind!(kwd Else)]);
    }

    #[test]
    fn ident() {
        check_tokens("else_", &[tkind!(ident "else_")])
    }

    fn check_tokens(s: &str, t: &[TokenKind]) {
        let mut context = ParseContext::new(s);

        let mut tokens = vec![];
        while let Some(token) = context.lex_token() {
            tokens.push(token.kind);
        }

        assert_eq!(&tokens, t)
    }
}
