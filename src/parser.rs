use std::collections::VecDeque;

use crate::{
    lexer::Token,
    types::{LispErr, LispObject},
};

pub fn parse(mut file: VecDeque<Token>) -> Result<Vec<LispObject>, LispErr> {
    let mut result = Vec::new();
    while !file.is_empty() {
        result.push(parse_one(&mut file, 0)?);
    }
    Ok(result)
}

fn parse_one(values: &mut VecDeque<Token>, backquote_count: u64) -> Result<LispObject, LispErr> {
    match values.pop_front().unwrap() {
        Token::OpenParen => parse_paren(values, backquote_count),
        Token::CloseParen => Err(LispErr::new("Unmatched close parenthesis".into())),
        Token::Symbol(s) => Ok(LispObject::Symbol(s.as_str().into())),
        Token::String(s) => Ok(LispObject::String(s)),
        Token::Backquote => parse_backquote(values, backquote_count),
        Token::Comma => {
            if backquote_count == 0 {
                Err(LispErr::new("Invalid location for comma".to_string()))
            } else {
                Ok(LispObject::Vec(vec![
                    LispObject::Symbol(",".into()),
                    parse_one(values, backquote_count - 1)?,
                ]))
            }
        }
        Token::CommaAt => {
            if backquote_count == 0 {
                Err(LispErr::new("Invalid location for comma".to_string()))
            } else {
                Ok(LispObject::Vec(vec![
                    LispObject::Symbol(",@".into()),
                    parse_one(values, backquote_count - 1)?,
                ]))
            }
        }
        Token::Quote => parse_quote(values, backquote_count),
        Token::Character(c) => Ok(LispObject::Character(c)),
        Token::Number(n) => Ok(LispObject::Number(n)),
        Token::HashQuote => parse_hashquote(values, backquote_count),
    }
}

fn parse_paren(values: &mut VecDeque<Token>, backquote_count: u64) -> Result<LispObject, LispErr> {
    let mut result = Vec::new();
    while !matches!(values[0], Token::CloseParen) {
        result.push(parse_one(values, backquote_count)?);
    }
    values.pop_front();
    Ok(LispObject::Vec(result))
}

fn parse_quote(values: &mut VecDeque<Token>, backquote_count: u64) -> Result<LispObject, LispErr> {
    let inner = parse_one(values, backquote_count)?;
    Ok(LispObject::Vec(vec![
        LispObject::Symbol("quote".into()),
        inner,
    ]))
}

fn parse_hashquote(
    values: &mut VecDeque<Token>,
    backquote_count: u64,
) -> Result<LispObject, LispErr> {
    let inner = parse_one(values, backquote_count)?;
    Ok(LispObject::Vec(vec![
        LispObject::Symbol("symbol-function".into()),
        LispObject::Vec(vec![LispObject::Symbol("quote".into()), inner]),
    ]))
}

fn parse_backquote(
    values: &mut VecDeque<Token>,
    backquote_count: u64,
) -> Result<LispObject, LispErr> {
    let inner = parse_one(values, backquote_count + 1)?;
    Ok(expand_backquote(inner))
}

fn expand_backquote(value: LispObject) -> LispObject {
    expand_bq_inner(value).0
}

fn expand_bq_inner(value: LispObject) -> (LispObject, bool) {
    // TODO: Quote-folding
    // Turn (list (quote a) (quote b)) into (quote (a b))
    match value {
        LispObject::Vec(vals) => {
            if vals.is_empty() {
                (LispObject::nil(), false)
            } else if vals[0].is_symbol_named(",") {
                (vals[1].clone(), false) // TODO: Remove clone
            } else if vals[0].is_symbol_named(",@") {
                (vals[1].clone(), true)
            } else {
                let mut old_results = Vec::new();
                let mut result = vec![LispObject::Symbol("list".into())];
                for val in vals {
                    let (v, is_splat) = expand_bq_inner(val);
                    if is_splat {
                        if result.len() > 1 {
                            old_results.push(LispObject::Vec(result));
                        }
                        old_results.push(v);
                        result = vec![LispObject::Symbol("list".into())];
                    } else {
                        result.push(v);
                    }
                }
                if old_results.is_empty() {
                    (LispObject::Vec(result), false)
                } else {
                    old_results.insert(0, LispObject::Symbol("append".into()));
                    if result.len() > 1 {
                        old_results.push(LispObject::Vec(result));
                    }
                    (LispObject::Vec(old_results), false)
                }
            }
        }
        v => (
            LispObject::Vec(vec![LispObject::Symbol("quote".into()), v]),
            false,
        ),
    }
}
