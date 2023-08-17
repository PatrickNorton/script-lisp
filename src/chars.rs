use std::{
    io::{self, Bytes, Read},
    iter::FusedIterator,
};

/// A character-based iterator over something implementing [`Read`].
#[derive(Debug)]
pub struct Chars<R> {
    inner: Bytes<R>,
    peeked: Option<char>,
}

/// Create an iterator over the characters of the given [`Read`] object.
pub fn chars<R: Read>(reader: R) -> Chars<R> {
    Chars {
        inner: reader.bytes(),
        peeked: None,
    }
}

impl<R: Read> Chars<R> {
    pub fn peek(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.peeked {
            Some(c) => Some(Ok(c)),
            None => match self.read_next() {
                c @ (None | Some(Err(_))) => c,
                Some(Ok(chr)) => {
                    self.peeked = Some(chr);
                    Some(Ok(chr))
                }
            },
        }
    }

    fn read_next(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.inner.next()? {
            Err(e) => Some(Err(e)),
            Ok(c @ 0..=0x7f) => Some(Ok(c as char)),
            Ok(c @ 0x80..=0xBF) => Some(Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Invalid UTF-8 continuation byte {c:#04x}"),
            ))),
            Ok(c @ 0xC0..=0xDF) => self.read_two(c),
            Ok(c @ 0xE0..=0xEF) => self.read_three(c),
            Ok(c @ 0xF0..=0xF4) => self.read_four(c),
            Ok(c @ 0xF5..=0xFF) => Some(Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Invalid UTF-8 byte {c:#04x}"),
            ))),
        }
    }

    fn read_two(&mut self, first: u8) -> Option<<Self as Iterator>::Item> {
        match self.inner.next()? {
            Ok(second) => Some(Ok(str_to_char(
                std::str::from_utf8(&[first, second]).unwrap(),
            ))),
            Err(e) => Some(Err(e)),
        }
    }

    fn read_three(&mut self, first: u8) -> Option<<Self as Iterator>::Item> {
        let second = match self.inner.next()? {
            Ok(s) => s,
            Err(e) => return Some(Err(e)),
        };
        let third = match self.inner.next()? {
            Ok(s) => s,
            Err(e) => return Some(Err(e)),
        };
        Some(Ok(str_to_char(
            std::str::from_utf8(&[first, second, third]).unwrap(),
        )))
    }

    fn read_four(&mut self, first: u8) -> Option<<Self as Iterator>::Item> {
        let second = match self.inner.next()? {
            Ok(s) => s,
            Err(e) => return Some(Err(e)),
        };
        let third = match self.inner.next()? {
            Ok(s) => s,
            Err(e) => return Some(Err(e)),
        };
        let fourth = match self.inner.next()? {
            Ok(s) => s,
            Err(e) => return Some(Err(e)),
        };
        Some(Ok(str_to_char(
            std::str::from_utf8(&[first, second, third, fourth]).unwrap(),
        )))
    }
}

impl<R: Read> Iterator for Chars<R> {
    type Item = io::Result<char>;

    fn next(&mut self) -> Option<Self::Item> {
        self.peeked.take().map(Ok).or_else(|| self.read_next())
    }
}

impl<R: Read + FusedIterator> FusedIterator for Chars<R> {}

fn str_to_char(string: &str) -> char {
    debug_assert!(!string.is_empty());
    let mut chars = string.chars();
    let chr = chars.next().unwrap();
    debug_assert_eq!(chars.next(), None);
    chr
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::chars::chars;

    #[test]
    fn single_char() {
        for chr in '\0'..='\u{10FFFF}' {
            match chars(Cursor::new(chr.to_string())).next() {
                None => panic!(
                    "Unexpected end of iteration when parsing char {chr} ({:#x})",
                    chr as u32
                ),
                Some(Err(e)) => panic!("Unexpected error {e}"),
                Some(Ok(res)) => assert_eq!(res, chr),
            }
        }
    }

    #[test]
    fn peek_single_char() {
        for chr in '\0'..='\u{10FFFF}' {
            let mut chars = chars(Cursor::new(chr.to_string()));
            match chars.peek() {
                None => panic!(
                    "Unexpected end of iteration when parsing char {chr} ({:#x})",
                    chr as u32
                ),
                Some(Err(e)) => panic!("Unexpected error {e}"),
                Some(Ok(res)) => assert_eq!(res, chr),
            }
            match chars.next() {
                None => panic!(
                    "Unexpected end of iteration when parsing char {chr} ({:#x})",
                    chr as u32
                ),
                Some(Err(e)) => panic!("Unexpected error {e}"),
                Some(Ok(res)) => assert_eq!(res, chr),
            }
        }
    }

    #[test]
    fn empty_input() {
        assert!(matches!(chars(Cursor::new("")).next(), None));
    }
}
