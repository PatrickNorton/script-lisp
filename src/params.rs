use std::collections::{hash_map::Entry, HashMap};

use crate::{
    env::LispEnv,
    eval,
    types::{LispErr, LispObject},
};

/// A struct representing a parameter list in a function or similar.
///
/// # Creation
///
/// Parameter lists are created from a [`LispObject`] through the `parse`
/// method.
///
/// # Usage
///
/// The `to_frame` method takes a list of passed arguments and constructs a
/// frame with all the variables bound to their respective values.
#[derive(Debug)]
pub struct LispParams {
    normal_args: Vec<String>,
    optional_args: Vec<(String, Option<LispObject>)>,
    rest_arg: Option<String>,
    kwargs: Vec<(String, Option<LispObject>)>,
    rest_kwarg: Option<String>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum SkipTo {
    Optional,
    Rest,
    Kwargs,
    RestKwarg,
    End,
}

#[derive(Debug)]
struct ParamIter<T> {
    value: T,
}

impl LispParams {
    pub const fn empty() -> Self {
        Self {
            normal_args: Vec::new(),
            optional_args: Vec::new(),
            rest_arg: None,
            kwargs: Vec::new(),
            rest_kwarg: None,
        }
    }

    /// Parses a parameter list from a given [`LispObject`].
    ///
    /// # Syntax
    ///
    /// Parameter lists have the following syntax:
    /// ```text
    /// (*symbol
    ///   [&optional {symbol | (symbol form)}*]
    ///   [&rest symbol]
    ///   [&key {symbol | (symbol form)}*]
    ///   [&other-keys symbol])
    /// ```
    pub fn parse(env: &mut LispEnv, val: &LispObject) -> Result<Self, LispErr> {
        match val {
            LispObject::Vec(vals) => Self::parse_inner(env, vals),
            val => Err(LispErr::new(format!("Invalid argument list: {}", val))),
        }
    }

    fn parse_inner(env: &mut LispEnv, vals: &[LispObject]) -> Result<Self, LispErr> {
        // Shortcut for empty argument lists
        if vals.is_empty() {
            return Ok(Self::empty());
        }
        let mut index = 0;
        let mut skip_to = SkipTo::Optional;
        let normal_args = Self::parse_normal_args(vals, &mut index, &mut skip_to)?;
        let optional_args = if skip_to <= SkipTo::Optional {
            Self::parse_optional_args(env, vals, &mut index, &mut skip_to)?
        } else {
            Vec::new()
        };
        let rest_arg = if skip_to <= SkipTo::Rest {
            Some(Self::parse_rest_arg(vals, &mut index, &mut skip_to)?)
        } else {
            None
        };
        let kwargs = if skip_to <= SkipTo::Kwargs {
            Self::parse_keyword_args(env, vals, &mut index, &mut skip_to)?
        } else {
            Vec::new()
        };
        let rest_kwarg = if skip_to <= SkipTo::RestKwarg {
            Some(Self::parse_kw_rest_arg(vals, &mut index)?)
        } else {
            None
        };
        Ok(LispParams {
            normal_args,
            optional_args,
            rest_arg,
            kwargs,
            rest_kwarg,
        })
    }

    fn parse_normal_args(
        vals: &[LispObject],
        index: &mut usize,
        skip_to: &mut SkipTo,
    ) -> Result<Vec<String>, LispErr> {
        let mut normal_args = Vec::new();
        loop {
            match vals.get(*index) {
                None => {
                    *skip_to = SkipTo::End;
                    break;
                }
                Some(val) => match Self::ensure_symbol(val)? {
                    "&optional" => {
                        *skip_to = SkipTo::Optional;
                        break;
                    }
                    "&rest" | "&body" => {
                        *skip_to = SkipTo::Rest;
                        break;
                    }
                    "&key" => {
                        *skip_to = SkipTo::Kwargs;
                        break;
                    }
                    "&other-keys" => {
                        *skip_to = SkipTo::RestKwarg;
                        break;
                    }
                    val => normal_args.push(val.to_string()),
                },
            }
            *index += 1;
        }
        Ok(normal_args)
    }

    fn parse_optional_args(
        env: &mut LispEnv,
        vals: &[LispObject],
        index: &mut usize,
        skip_to: &mut SkipTo,
    ) -> Result<Vec<(String, Option<LispObject>)>, LispErr> {
        let mut optional_args = Vec::new();
        *index += 1;
        loop {
            match vals.get(*index) {
                None => {
                    *skip_to = SkipTo::End;
                    break;
                }
                Some(val) => {
                    let (name, default) = Self::ensure_symbol_or_default(val)?;
                    match name {
                        "&optional" => {
                            return Err(LispErr::new(
                                "Multiple uses of &optional in argument list".to_string(),
                            ))
                        }
                        "&rest" | "&body" => {
                            *skip_to = SkipTo::Rest;
                            break;
                        }
                        "&key" => {
                            *skip_to = SkipTo::Kwargs;
                            break;
                        }
                        "&other-keys" => {
                            *skip_to = SkipTo::RestKwarg;
                            break;
                        }
                        val => optional_args.push((val.to_string(), eval_optional(env, default)?)),
                    }
                }
            }
            *index += 1;
        }
        Ok(optional_args)
    }

    fn parse_rest_arg(
        vals: &[LispObject],
        index: &mut usize,
        skip_to: &mut SkipTo,
    ) -> Result<String, LispErr> {
        *index += 1;
        let sym = match vals.get(*index) {
            Some(s) => s,
            None => return Err(LispErr::new("Expected value after &rest".to_string())),
        };
        let rest_arg = match Self::ensure_symbol(sym)? {
            val @ ("&optional" | "&rest" | "&body" | "&key" | "&other-keys") => {
                return Err(LispErr::new(format!(
                    "Expected value for keyword argument, not reserved word {}",
                    val
                )))
            }
            val => val.to_string(),
        };
        *index += 1;
        match vals.get(*index) {
            Some(s) => match Self::ensure_symbol(s)? {
                "&optional" => {
                    return Err(LispErr::new(
                        "&optional tag must come before &rest".to_string(),
                    ))
                }
                t @ ("&rest" | "&body") => {
                    // FIXME: This message won't make sense if you use both
                    // &rest and &body
                    return Err(LispErr::new(format!(
                        "Multiple uses of {t} in argument list"
                    )));
                }
                "&key" => {
                    *skip_to = SkipTo::Kwargs;
                }
                "&other-keys" => {
                    *skip_to = SkipTo::RestKwarg;
                }
                _ => {
                    return Err(LispErr::new(
                        "Only one value is allowed for keyword argument".to_string(),
                    ))
                }
            },
            None => {
                *skip_to = SkipTo::End;
            }
        }
        Ok(rest_arg)
    }

    fn parse_keyword_args(
        env: &mut LispEnv,
        vals: &[LispObject],
        index: &mut usize,
        skip_to: &mut SkipTo,
    ) -> Result<Vec<(String, Option<LispObject>)>, LispErr> {
        let mut kwargs = Vec::new();
        *index += 1;
        loop {
            match vals.get(*index) {
                None => {
                    *skip_to = SkipTo::End;
                    break;
                }
                Some(val) => {
                    let (name, default) = Self::ensure_symbol_or_default(val)?;
                    match name {
                        t @ ("&rest" | "&body" | "&optional") => {
                            return Err(LispErr::new(format!("{t} tag must come before &key")))
                        }
                        "&key" => {
                            return Err(LispErr::new(
                                "Multiple uses of &key in argument list".to_string(),
                            ))
                        }
                        "&other-keys" => {
                            *skip_to = SkipTo::RestKwarg;
                            break;
                        }
                        val => kwargs.push((val.to_string(), eval_optional(env, default)?)),
                    }
                }
            }
            *index += 1;
        }
        Ok(kwargs)
    }

    fn parse_kw_rest_arg(vals: &[LispObject], index: &mut usize) -> Result<String, LispErr> {
        *index += 1;
        let sym = match vals.get(*index) {
            Some(s) => s,
            None => return Err(LispErr::new("Expected value after &other-keys".to_string())),
        };
        let result = match Self::ensure_symbol(sym)? {
            val @ ("&optional" | "&rest" | "&body" | "&key" | "&other-keys") => {
                return Err(LispErr::new(format!(
                    "Expected value for keyword argument, not reserved word {}",
                    val
                )))
            }
            val => val.to_string(),
        };
        *index += 1;
        if let Option::Some(s) = vals.get(*index) {
            match Self::ensure_symbol(s)? {
                t @ ("&optional" | "&rest" | "&body" | "&key") => {
                    return Err(LispErr::new(format!(
                        "{t} tag must come before &other-keys"
                    )))
                }
                "&other-keys" => {
                    return Err(LispErr::new(
                        "Multiple uses of &rest in argument list".to_string(),
                    ))
                }
                _ => {
                    return Err(LispErr::new(
                        "Only one value is allowed for &other-keys argument".to_string(),
                    ))
                }
            }
        }
        Ok(result)
    }

    /// Creates a frame (for use in a [`LispEnv`]) given a list of passed
    /// arguments.
    pub fn to_frame(&self, vals: &[LispObject]) -> Result<HashMap<String, LispObject>, LispErr> {
        let mut val_iter = ParamIter { value: vals.iter() };
        let mut frame = HashMap::new();
        let mut kwargs = HashMap::new();
        for arg in &self.normal_args {
            let val = Self::next_add_kwargs(&mut val_iter, &mut kwargs)?
                .ok_or_else(|| LispErr::new("Not enough arguments".to_string()))?;
            frame.insert(arg.clone(), val);
        }
        for (arg, default) in &self.optional_args {
            let val = Self::next_add_kwargs(&mut val_iter, &mut kwargs)?
                .unwrap_or_else(|| default.clone().unwrap_or_else(LispObject::nil));
            frame.insert(arg.clone(), val.clone());
        }
        if let Option::Some(name) = &self.rest_arg {
            let mut varargs = Vec::new();
            while let Option::Some(x) = Self::next_add_kwargs(&mut val_iter, &mut kwargs)? {
                varargs.push(x);
            }
            frame.insert(name.clone(), LispObject::Vec(varargs));
        } else if val_iter.next()?.is_some() {
            return Err(LispErr::new("Too many arguments".to_string()));
        }
        for (kwarg, default) in &self.kwargs {
            match kwargs.remove_entry(kwarg) {
                None => match default {
                    None => todo!("Error"),
                    Some(obj) => frame.insert(kwarg.clone(), obj.clone()),
                },
                Some((k, v)) => frame.insert(k, v),
            };
        }
        let remaining_kwargs = kwargs
            .into_iter()
            .map(|(k, v)| LispObject::Vec(vec![LispObject::Symbol(k.into()), v]))
            .collect();
        if let Option::Some(s) = &self.rest_kwarg {
            frame.insert(s.clone(), LispObject::Vec(remaining_kwargs));
        } else if !remaining_kwargs.is_empty() {
            return Err(LispErr::new("Unexpected keyword argument".to_string()));
        }
        Ok(frame)
    }

    fn next_add_kwargs<'a, I>(
        iter: &mut ParamIter<I>,
        kwargs: &mut HashMap<String, LispObject>,
    ) -> Result<Option<LispObject>, LispErr>
    where
        I: Iterator<Item = &'a LispObject>,
    {
        loop {
            match iter.next()? {
                Some((Some(kw), val)) => {
                    let entry = kwargs.entry(kw.to_string());
                    match entry {
                        Entry::Occupied(o) => {
                            return Err(LispErr::new(format!(
                                "Invalid double-entry of keyword argument {}",
                                o.key()
                            )))
                        }
                        Entry::Vacant(v) => v.insert(val.clone()),
                    };
                }
                Some((None, val)) => return Ok(Some(val.clone())),
                None => return Ok(None),
            }
        }
    }

    fn ensure_symbol(val: &LispObject) -> Result<&str, LispErr> {
        match val {
            LispObject::Symbol(s) => Ok(s),
            _ => Err(LispErr::new(format!("Expected symbol, got {}", val))),
        }
    }

    fn ensure_symbol_or_default(val: &LispObject) -> Result<(&str, Option<&LispObject>), LispErr> {
        match val {
            LispObject::Symbol(s) => Ok((s, None)),
            LispObject::Vec(v) => match v.as_slice() {
                [LispObject::Symbol(name), val] => Ok((name, Some(val))),
                _ => Err(LispErr::new(format!("Expected symbol, got {}", val))),
            },
            _ => Err(LispErr::new(format!("Expected symbol, got {}", val))),
        }
    }
}

fn eval_optional(
    env: &mut LispEnv,
    val: Option<&LispObject>,
) -> Result<Option<LispObject>, LispErr> {
    match val {
        Some(x) => eval(env, x).map(Some),
        None => Ok(None),
    }
}

impl<'a, T: Iterator<Item = &'a LispObject>> ParamIter<T> {
    fn next(&mut self) -> Result<Option<(Option<&'a str>, &'a LispObject)>, LispErr> {
        match self.value.next() {
            None => Ok(None),
            Some(LispObject::Symbol(s)) if s.starts_with(':') => {
                let value = self.value.next().ok_or_else(|| {
                    LispErr::new("Keyword tag cannot be last argument".to_string())
                })?;
                Ok(Some((Some(s), value)))
            }
            Some(next) => Ok(Some((None, next))),
        }
    }
}
