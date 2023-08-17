use std::io::{self, Read};

use num::{BigInt, Zero};

use crate::{
    chars::{chars, Chars},
    types::LispErr,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    OpenParen,
    CloseParen,
    Symbol(String),
    String(String),
    Character(char),
    Number(BigInt),
    Quote,
    Backquote,
    Comma,
    CommaAt,
    HashQuote,
}

/// Takes a file-like object and transforms it into a list of tokens.
pub fn lex(file: impl Read) -> Result<Vec<Token>, LispErr> {
    let mut vals = Vec::new();
    let mut chars = chars(file);
    while let Option::Some(c) = next_skip_whitespace(&mut chars) {
        let chr = c?;
        vals.push(match chr {
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            '?' => Token::Character(parse_char(&mut chars)?),
            '\'' => Token::Quote,
            '"' => Token::String(parse_string(&mut chars)?),
            '`' => Token::Backquote,
            ',' => {
                if let Option::Some(c) = chars.peek() {
                    if c? == '@' {
                        let _ = chars.next();
                        Token::CommaAt
                    } else {
                        Token::Comma
                    }
                } else {
                    Token::Comma
                }
            }
            ';' => {
                parse_comment(&mut chars)?;
                continue;
            }
            '\\' => todo!(),
            '#' => parse_hashtag(&mut chars)?,
            c @ '0'..='9' => Token::Number(parse_number(&mut chars, c)?),
            c => Token::Symbol(parse_symbol(&mut chars, c)?),
        })
    }
    Ok(vals)
}

fn next_skip_whitespace(chars: &mut Chars<impl Read>) -> Option<Result<char, io::Error>> {
    loop {
        match chars.peek()? {
            Ok(c) if c.is_whitespace() => {
                chars.next()?.unwrap();
            }
            Ok(_) => return chars.next(),
            Err(e) => return Some(Err(e)),
        }
    }
}

fn parse_char(chars: &mut Chars<impl Read>) -> Result<char, LispErr> {
    match chars.next().unwrap()? {
        '\\' => parse_char_backslash(chars),
        '(' | ')' | '[' | ']' | ';' | '"' => Err(LispErr::new(
            "Invalid character literal: requires backslash".to_string(),
        )),
        c => Ok(c),
    }
}

fn parse_char_backslash(chars: &mut Chars<impl Read>) -> Result<char, LispErr> {
    match chars.next().unwrap()? {
        c @ ('\\' | '(' | ')' | '[' | ']' | ';' | '"' | '|' | '\'' | '`' | '#' | '.' | ',') => {
            Ok(c)
        }
        '0' => Ok('\0'),
        'a' => Ok('\x07'),
        'b' => Ok('\x08'),
        't' => Ok('\t'),
        'n' => Ok('\n'),
        'v' => Ok('\x0B'),
        'f' => Ok('\x0C'),
        'r' => Ok('\r'),
        'e' => Ok('\x1b'),
        's' => Ok(' '),
        'd' => Ok('\x7F'),
        'x' => parse_hex_literal(chars),
        'u' => parse_unicode_literal(chars),
        'U' => parse_long_unicode_literal(chars),
        c => Err(LispErr::new(format!("Invalid character escape: \\{c}"))),
    }
}

fn parse_hex_literal(chars: &mut Chars<impl Read>) -> Result<char, LispErr> {
    let chr1 = chars.next().ok_or_else(chr_eof_err)??;
    let chr2 = chars.next().ok_or_else(chr_eof_err)??;
    let sum = chr_to_hex(chr1)? * 16 + chr_to_hex(chr2)?;
    char::from_u32(sum).ok_or_else(|| illegal_character(sum))
}

fn parse_unicode_literal(chars: &mut Chars<impl Read>) -> Result<char, LispErr> {
    let chr1 = chars.next().ok_or_else(chr_eof_err)??;
    let chr2 = chars.next().ok_or_else(chr_eof_err)??;
    let chr3 = chars.next().ok_or_else(chr_eof_err)??;
    let chr4 = chars.next().ok_or_else(chr_eof_err)??;
    let sum = chr_to_hex(chr1)? * 0x1000
        + chr_to_hex(chr2)? * 0x100
        + chr_to_hex(chr3)? * 0x10
        + chr_to_hex(chr4)?;
    char::from_u32(sum).ok_or_else(|| illegal_character(sum))
}

fn parse_long_unicode_literal(chars: &mut Chars<impl Read>) -> Result<char, LispErr> {
    let chr1 = chars.next().ok_or_else(chr_eof_err)??;
    let chr2 = chars.next().ok_or_else(chr_eof_err)??;
    let chr3 = chars.next().ok_or_else(chr_eof_err)??;
    let chr4 = chars.next().ok_or_else(chr_eof_err)??;
    let chr5 = chars.next().ok_or_else(chr_eof_err)??;
    let chr6 = chars.next().ok_or_else(chr_eof_err)??;
    let sum = chr_to_hex(chr1)? * 0x100000
        + chr_to_hex(chr2)? * 0x10000
        + chr_to_hex(chr3)? * 0x1000
        + chr_to_hex(chr4)? * 0x100
        + chr_to_hex(chr5)? * 0x10
        + chr_to_hex(chr6)?;
    char::from_u32(sum).ok_or_else(|| illegal_character(sum))
}

fn chr_to_hex(chr: char) -> Result<u32, LispErr> {
    chr.to_digit(16).ok_or_else(|| invalid_hex_digit(chr))
}

fn chr_eof_err() -> LispErr {
    LispErr::new("Unexpected EOF in character literal".to_string())
}

fn invalid_hex_digit(digit: char) -> LispErr {
    LispErr::new(format!("Invalid hex digit in character literal: {digit}"))
}

fn illegal_character(sum: u32) -> LispErr {
    LispErr::new(format!("Illegal character literal: {sum:x}"))
}

fn parse_comment(chars: &mut Chars<impl Read>) -> Result<(), io::Error> {
    for c in chars.by_ref() {
        if c? == '\n' {
            return Ok(());
        }
    }
    Ok(())
}

fn parse_number(chars: &mut Chars<impl Read>, first: char) -> Result<BigInt, io::Error> {
    if first == '0' {
        match chars.peek() {
            Option::None => Ok(BigInt::zero()),
            Option::Some(c) => match c? {
                'x' => parse_base(chars, 16),
                'o' => parse_base(chars, 8),
                'b' => parse_base(chars, 2),
                c if is_symbol_char(c) => parse_decimal(chars, first),
                _ => Ok(BigInt::zero()),
            },
        }
    } else {
        parse_decimal(chars, first)
    }
}

fn parse_decimal(chars: &mut Chars<impl Read>, first: char) -> Result<BigInt, io::Error> {
    // TODO: Negative numbers
    let mut text = vec![first.to_digit(10).unwrap() as u8];
    while let Option::Some(chr) = chars.peek() {
        let chr = chr?;
        if is_symbol_char(chr) {
            text.push(chr.to_digit(10).unwrap() as u8);
            chars.next().unwrap()?;
        } else {
            break;
        }
    }
    Ok(BigInt::from_radix_be(num::bigint::Sign::Plus, &text, 10).unwrap())
}

fn parse_base(chars: &mut Chars<impl Read>, radix: u32) -> Result<BigInt, io::Error> {
    // TODO: Negative numbers
    chars.next().unwrap()?;
    let mut text = Vec::new();
    while let Option::Some(chr) = chars.peek() {
        let chr = chr?;
        if is_symbol_char(chr) {
            text.push(chr.to_digit(radix).unwrap() as u8);
            chars.next().unwrap()?;
        } else {
            break;
        }
    }
    Ok(BigInt::from_radix_be(num::bigint::Sign::Plus, &text, radix).unwrap())
}

fn parse_string(chars: &mut Chars<impl Read>) -> Result<String, LispErr> {
    let mut text = String::new();
    while let Option::Some(chr) = chars.next() {
        let chr = chr?;
        match chr {
            '"' => return Ok(text),
            '\\' => text.push(parse_char_backslash(chars)?),
            c => text.push(c),
        }
    }
    Err(LispErr::new("Unexpected EOF".to_string()))
}

fn parse_symbol(chars: &mut Chars<impl Read>, first: char) -> Result<String, io::Error> {
    let mut symbol = String::from(first);
    while let Option::Some(chr) = chars.peek() {
        let chr = chr?;
        if !is_symbol_char(chr) {
            break;
        }
        let _ = chars.next();
        symbol.push(chr);
    }
    Ok(symbol)
}

fn is_symbol_char(chr: char) -> bool {
    !r#"()[];"'|`#.,\? "#.contains(chr) && !chr.is_whitespace()
}

fn parse_hashtag(chars: &mut Chars<impl Read>) -> Result<Token, LispErr> {
    match chars.next().unwrap()? {
        '\'' => Ok(Token::HashQuote),
        c => Err(LispErr::new(format!(
            "Unexpected character following hashtag: {c}"
        ))),
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use num::{BigInt, One, Zero};

    use crate::{
        lexer::{lex, Token},
        types::LispErr,
    };

    #[test]
    fn parse_empty() -> Result<(), LispErr> {
        assert_eq!(lex(Cursor::new(""))?, Vec::new());
        Ok(())
    }

    #[test]
    fn parse_nil() -> Result<(), LispErr> {
        assert_eq!(
            lex(Cursor::new("()"))?,
            vec![Token::OpenParen, Token::CloseParen]
        );
        assert_eq!(
            lex(Cursor::new("( )"))?,
            vec![Token::OpenParen, Token::CloseParen]
        );
        Ok(())
    }

    #[test]
    fn parse_char() -> Result<(), LispErr> {
        let chars = [
            ("?a", 'a'),              // Simple character
            (r"?\\", '\\'),           // Backslash character
            (r#"?\""#, '"'),          // Quote character
            ("?.", '.'),              // Period without backslash
            (r"?\.", '.'),            // Period with backslash
            (r"?\a", '\x07'),         // Special escaped char
            (r"?\n", '\n'),           // Newline char
            (r"?\;", ';'),            // Semicolon (comment char)
            (r"?\0", '\0'),           // Null character
            (r"?\x01", '\x01'),       // Arbitrary hex literal
            (r"?\x61", 'a'),          // Hex literal mapping onto printable ASCII
            (r"?\u00b0", '\u{00b0}'), // Unicode literal
            (r"?\U0000B0", '\u{b0}'), // Big unicode literal
        ];

        for (text, chr) in chars {
            assert_eq!(lex(Cursor::new(text))?, vec![Token::Character(chr)])
        }
        Ok(())
    }

    #[test]
    fn parse_symbol() -> Result<(), LispErr> {
        let symbols = [
            "abc",       // Simple symbol
            "a123",      // Numbers
            "a-b-c",     // Hyphens
            "\u{2014}a", // Unicode
            "+",         // Plus
            "a/b",       // Forward slashes
            "a\u{3071}", // More unicode (non-leading)
            "lambda",    // Symbol used as a builtin
            "t",         // Magic symbol for true
            "nil",       // Magic symbol for nil
        ];

        for symbol in symbols {
            assert_eq!(
                lex(Cursor::new(symbol))?,
                vec![Token::Symbol(symbol.into())]
            );
        }
        Ok(())
    }

    #[test]
    fn parse_quote() -> Result<(), LispErr> {
        assert_eq!(lex(Cursor::new("'"))?, vec![Token::Quote]);
        assert_eq!(
            lex(Cursor::new("'a"))?,
            vec![Token::Quote, Token::Symbol("a".to_string())]
        );
        Ok(())
    }

    #[test]
    fn parse_num() -> Result<(), LispErr> {
        let vals = [
            ("0", BigInt::zero()),
            ("1", BigInt::one()),
            ("123456789", BigInt::from(123456789)),
            ("1000000000000000", BigInt::from(1000000000000000u128)),
            ("0x1", BigInt::one()),
            ("0xa", BigInt::from(0xa)),
        ];

        for (text, val) in vals {
            assert_eq!(lex(Cursor::new(text))?, vec![Token::Number(val)]);
        }
        Ok(())
    }

    #[test]
    fn parse_str() -> Result<(), LispErr> {
        let vals = [
            (r#""a""#, "a"),             // Basic string
            (r#""\"""#, "\""),           // String containing escaped quote
            (r#""test 1""#, "test 1"),   // Longer string with spaces
            (r#"";""#, ";"),             // String containing comment char
            (r#""'symbol""#, "'symbol"), // String containing single quote
            (r#""\n\x61""#, "\na"),      // String containing escaped chars
            (r#""\0""#, "\0"),           // String containing null char
            (r#""\u2014""#, "\u{2014}"), // String containing unicode escape
            (r#""\x00\x22""#, "\0\""),   // String containing hex-escaped dangerous chars
        ];

        for (syntax, result) in vals {
            assert_eq!(
                lex(Cursor::new(syntax))?,
                vec![Token::String(result.to_string())]
            );
        }
        Ok(())
    }

    #[test]
    fn test_bug() -> Result<(), LispErr> {
        assert_eq!(
            lex(Cursor::new("(symbol-function 'print)"))?,
            vec![
                Token::OpenParen,
                Token::Symbol("symbol-function".to_string()),
                Token::Quote,
                Token::Symbol("print".to_string()),
                Token::CloseParen
            ]
        );
        Ok(())
    }
}
