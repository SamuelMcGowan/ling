use ustr::Ustr;

use super::span::Span;
use super::token::{tkind, Token, TokenKind};
use super::{Cursor, ParseContext};
use crate::value::Value;

impl<'a> ParseContext<'a> {
    pub fn lex_tokens(&mut self) -> Lexer<'_, 'a> {
        Lexer {
            cursor: Cursor::new(self.source),
            context: self,
        }
    }
}

pub(crate) struct Lexer<'ctx, 'a> {
    cursor: Cursor<'a>,
    context: &'ctx mut ParseContext<'a>,
}

impl Lexer<'_, '_> {
    fn lex_token(&mut self) -> Option<Token> {
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
                '0'..='9' => self.lex_number(c as u8),
                '"' => self.lex_string(),

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

            "true" => self.add_constant(Value::Bool(true)),
            "false" => self.add_constant(Value::Bool(false)),

            _ => tkind!(ident lexeme),
        }
    }

    fn lex_string(&mut self) -> TokenKind {
        let mut s = String::new();

        let mut unterminated_string = true;
        let mut invalid_escape = false;

        while let Some(c) = self.cursor.next() {
            let c = match c {
                '"' => {
                    unterminated_string = false;
                    break;
                }
                '\\' => match self.cursor.next() {
                    Some(c) => match c {
                        'n' => '\n',
                        'r' => '\n',
                        't' => '\t',
                        '0' => '\0',
                        _ => {
                            invalid_escape = true;
                            c
                        }
                    },
                    None => return tkind!(error UnterminatedString),
                },
                _ => c,
            };
            s.push(c);
        }

        if unterminated_string {
            return tkind!(error UnterminatedString);
        }
        if invalid_escape {
            return tkind!(error InvalidEscape);
        }

        let interned = Ustr::from(&s);
        self.add_constant(Value::String(interned))
    }

    fn lex_number(&mut self, first: u8) -> TokenKind {
        let int_digits = self.lex_digits(Some(first));

        if self.cursor.eat('.') {
            let fraction_digits = self.lex_digits(None);

            let mut exponent_overflow = false;
            let mut exponent_missing_digits = false;

            let exponent: i32 = if self.cursor.eat('e') || self.cursor.eat('E') {
                let sign = if self.cursor.eat('-') {
                    -1
                } else {
                    self.cursor.eat('+');
                    1
                };

                let exponent_digits = self.lex_digits(None);

                let exponent = parse_int(&exponent_digits).and_then(|n| n.try_into().ok());
                let exponent: i32 = match exponent {
                    Some(exponent) => exponent,
                    None => {
                        exponent_overflow = true;
                        0
                    }
                };

                exponent_missing_digits = exponent_digits.is_empty();

                exponent * sign
            } else {
                0
            };

            if exponent_overflow {
                return tkind!(error IntegerOverflow);
            }

            // integer won't have missing digits
            if fraction_digits.is_empty() || exponent_missing_digits {
                return tkind!(error MissingDigits);
            }

            let int_digits = trim_leading_zeros(&int_digits).iter();
            let fraction_digits = trim_trailing_zeros(&fraction_digits).iter();

            let f: f64 = minimal_lexical::parse_float(int_digits, fraction_digits, exponent);
            self.add_constant(Value::Float(f))
        } else {
            let Some(n) = parse_int(&int_digits) else {
                return tkind!(error IntegerOverflow);
            };
            self.add_constant(Value::Integer(n))
        }
    }

    fn lex_digits(&mut self, first: Option<u8>) -> Vec<u8> {
        let mut digits = vec![];
        digits.extend(first);
        loop {
            match self.cursor.peek() {
                Some(c @ '0'..='9') => {
                    digits.push(c as u8);
                    self.cursor.next();
                }
                Some('_') => continue,
                _ => break,
            }
        }
        digits
    }

    fn add_constant(&mut self, value: Value) -> TokenKind {
        let idx = self.context.chunk.add_constant(value);
        TokenKind::Const(idx)
    }
}

impl Iterator for Lexer<'_, '_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn parse_int(digits: &[u8]) -> Option<u64> {
    let mut n = 0u64;
    for &digit in digits {
        let digit = (digit - b'0') as u64;
        n = n.checked_mul(10)?;
        n = n.checked_add(digit)?;
    }
    Some(n)
}

fn trim_leading_zeros(mut digits: &[u8]) -> &[u8] {
    while let [b'0', rest @ ..] = digits {
        digits = rest;
    }
    digits
}

fn trim_trailing_zeros(mut digits: &[u8]) -> &[u8] {
    while let [rest @ .., b'0'] = digits {
        digits = rest;
    }
    digits
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chunk::{Chunk, ConstIdx};

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

    #[test]
    fn literals() {
        check_constant("true", Value::Bool(true));
        check_constant("false", Value::Bool(false));
    }

    #[test]
    fn string() {
        check_constant("\"hello\"", Value::String(Ustr::from("hello")));
        check_constant("\"\"", Value::String(Ustr::from("")));
    }

    #[test]
    fn string_unterminated() {
        check_tokens("\"hello", &[tkind!(error UnterminatedString)]);
        check_tokens("\"", &[tkind!(error UnterminatedString)]);
    }

    #[test]
    fn integer() {
        check_constant("123", Value::Integer(123));
    }

    #[test]
    fn float() {
        check_constant("12.34e-5", Value::Float(12.34e-5));
    }

    #[test]
    fn missing_digits() {
        check_tokens("12.", &[tkind!(error MissingDigits)]);
        check_tokens("12.1e", &[tkind!(error MissingDigits)]);
    }

    fn check_tokens(s: &str, t: &[TokenKind]) {
        let mut chunk = Chunk::default();
        let mut context = ParseContext::new(s, &mut chunk);
        let tokens: Vec<_> = context.lex_tokens().map(|token| token.kind).collect();
        assert_eq!(&tokens, t)
    }

    fn check_constant(s: &str, value: Value) {
        let mut chunk = Chunk::default();
        let mut context = ParseContext::new(s, &mut chunk);

        let tokens: Vec<_> = context.lex_tokens().map(|token| token.kind).collect();
        assert_eq!(&tokens, &[tkind!(constant 0)]);

        let value_found = chunk.get_constant(ConstIdx(0)).expect("constant not found");
        assert!(value_found.is_equal(&value));
    }
}
