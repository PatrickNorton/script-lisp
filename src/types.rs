use std::{
    borrow::Cow,
    collections::HashMap,
    error::Error,
    fmt::{Display, Write},
    io,
    rc::Rc,
};

use itertools::Itertools;
use num::{BigInt, BigRational};

use crate::env::LispEnv;

pub type NativeFn = fn(&mut LispEnv, &[LispObject]) -> Result<LispObject, LispErr>;

#[derive(Debug, Clone)]
pub enum LispObject {
    Symbol(Rc<str>),
    Character(char),
    Number(BigInt),
    Float(f64),
    Rational(BigRational),
    Vec(Vec<LispObject>),
    String(String),
    NativeFn(NativeFn),
    Function(LispFunction),
    True,
}

#[derive(Debug)]
pub struct LispErr {
    reason: String,
    source: Option<Box<dyn Error + 'static>>,
}

#[derive(Debug, Clone)]
pub struct LispFunction {
    pub params: Rc<LispParams>,
    pub body: Rc<LispObject>,
}

#[derive(Debug)]
pub struct LispParams {
    values: Vec<(String, ParamTag)>,
}

#[derive(Debug)]
enum ParamTag {
    Normal,
    Optional,
    Rest,
    Keyword,
}

pub type LispSpForm = fn(&mut LispEnv, &[LispObject]) -> Result<LispObject, LispErr>;

impl LispObject {
    pub const fn nil() -> Self {
        Self::Vec(Vec::new())
    }

    pub fn is_symbol_named(&self, name: &str) -> bool {
        if let Self::Symbol(s) = self {
            &**s == name
        } else {
            false
        }
    }
}

impl LispFunction {
    pub fn new(params: LispParams, body: LispObject) -> Self {
        Self {
            params: Rc::new(params),
            body: Rc::new(body),
        }
    }
}

impl LispErr {
    pub const fn new(reason: String) -> Self {
        Self {
            reason,
            source: None,
        }
    }
}

impl LispParams {
    pub fn parse(val: &LispObject) -> Result<Self, LispErr> {
        match val {
            LispObject::Vec(vals) => {
                let mut args = Vec::new();
                let mut val_iter = vals.iter();
                let mut has_rest = false;
                while let Option::Some(val) = val_iter.next() {
                    if has_rest {
                        return Err(LispErr::new(
                            "&rest must be last argument in argument list".to_string(),
                        ));
                    }
                    let s = Self::ensure_symbol(val)?;
                    if s.starts_with('&') {
                        let symb = Self::ensure_symbol(val_iter.next().unwrap())?;
                        match s {
                            "&rest" | "&body" => {
                                has_rest = true;
                                args.push((symb.to_string(), ParamTag::Rest))
                            }
                            "&optional" => args.push((symb.to_string(), ParamTag::Optional)),
                            "&key" => args.push((symb.to_string(), ParamTag::Keyword)),
                            s => {
                                return Err(LispErr::new(format!(
                                    "Unknown tag in argument list: {}",
                                    s
                                )))
                            }
                        }
                    } else {
                        args.push((s.to_string(), ParamTag::Normal))
                    }
                }
                Ok(LispParams { values: args })
            }
            val => Err(LispErr::new(format!("Invalid argument list: {}", val))),
        }
    }

    pub fn to_frame(&self, vals: &[LispObject]) -> Result<HashMap<String, LispObject>, LispErr> {
        if self.values.is_empty() && vals.is_empty() {
            Ok(HashMap::new())
        } else {
            let mut map = HashMap::with_capacity(self.values.len());
            let mut vals = vals.iter();
            for (param, tag) in &self.values {
                match tag {
                    ParamTag::Normal => map.insert(param.clone(), vals.next().unwrap().clone()),
                    ParamTag::Optional => todo!(),
                    ParamTag::Rest => {
                        // Break here is valid b/c we know &rest is always last
                        map.insert(param.clone(), vals.cloned().collect());
                        break;
                    }
                    ParamTag::Keyword => todo!(),
                };
            }
            Ok(map)
        }
    }

    fn ensure_symbol(val: &LispObject) -> Result<&str, LispErr> {
        match val {
            LispObject::Symbol(s) => Ok(s),
            _ => Err(LispErr::new(format!("Expected symbol, got {}", val))),
        }
    }
}

impl FromIterator<LispObject> for LispObject {
    fn from_iter<T: IntoIterator<Item = LispObject>>(iter: T) -> Self {
        LispObject::Vec(iter.into_iter().collect())
    }
}

impl From<bool> for LispObject {
    fn from(value: bool) -> Self {
        if value {
            LispObject::True
        } else {
            LispObject::nil()
        }
    }
}

impl Display for LispObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LispObject::Symbol(s) => write!(f, "'{}", s),
            LispObject::Character(c) => display_char(*c, f),
            LispObject::Number(n) => write!(f, "{}", n),
            LispObject::Float(n) => write!(f, "{}", n),
            LispObject::Rational(n) => write!(f, "{}", n),
            LispObject::Vec(v) => {
                write!(f, "({})", v.iter().format(" "))
            }
            LispObject::String(s) => {
                write!(f, r#""{}""#, s.chars().map(escape_char).collect::<String>())
            }
            LispObject::NativeFn(_) => todo!(),
            LispObject::Function(_) => write!(f, "<anonymous function>"),
            LispObject::True => f.write_char('t'),
        }
    }
}

fn display_char(chr: char, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match chr {
        c @ ('\\' | '(' | ')' | '[' | ']' | ';' | '"' | '|' | '\'' | '`' | '#' | '.' | ',') => {
            write!(f, "?\\{}", c)
        }
        '\x07' => f.write_str(r"?\a"),
        '\x08' => f.write_str(r"?\b"),
        '\t' => f.write_str(r"?\t"),
        '\n' => f.write_str(r"?\n"),
        '\x0B' => f.write_str(r"?\v"),
        '\x0C' => f.write_str(r"?\f"),
        '\r' => f.write_str(r"?\r"),
        '\x1b' => f.write_str(r"?\e"),
        ' ' => f.write_str(r"?\s"),
        '\x7F' => f.write_str(r"?\d"),
        // TODO: Unicode escapes for unprintable characters
        c => write!(f, "?{c}"),
    }
}

fn escape_char(chr: char) -> Cow<'static, str> {
    // TODO? Use formatter instead of creating intermediate strings
    match chr {
        '\\' => r"\\".into(),
        '"' => r#"\""#.into(),
        '\x07' => r"\a".into(),
        '\x08' => r"\b".into(),
        '\t' => r"\t".into(),
        '\n' => r"\n".into(),
        '\x0B' => r"\v".into(),
        '\x0C' => r"\f".into(),
        '\r' => r"\r".into(),
        '\x1b' => r"\e".into(),
        '\x7F' => r"\d".into(),
        // TODO: Unicode escapes for unprintable characters
        c => format!("{c}").into(),
    }
}

impl Display for LispErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.reason)
    }
}

impl Error for LispErr {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.source.as_deref()
    }
}

impl From<io::Error> for LispErr {
    fn from(value: io::Error) -> Self {
        Self {
            reason: value.to_string(),
            source: Some(Box::new(value)),
        }
    }
}
