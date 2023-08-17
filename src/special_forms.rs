use crate::{
    env::LispEnv,
    eval, macroexpand,
    types::{LispErr, LispFunction, LispObject, LispParams, LispSpForm},
};

use itertools::{process_results, put_back, Itertools};

pub fn get_special_form(symbol_name: &str) -> Option<(LispSpForm, LispSpForm)> {
    // TODO: Loop primitive (while?), file imports
    match symbol_name {
        "let" => Some((let_eval, let_macroexpand)),
        "cond" => Some((cond_eval, cond_macroexpand)),
        // Lambdas eval the same way whether in macroexpand or eval
        "lambda" => Some((lambda_eval, lambda_eval)),
        "eval" => todo!("eval not implemented yet"), // TODO: Make this a function?
        "progn" => Some((progn_eval, progn_macroexpand)),
        "quote" => Some((quote_eval, quote_macroexpand)),
        "error" => Some((error_eval, error_macroexpand)),
        "condition-case" => todo!("condition-case not implemented yet"),
        "defmacro" => Some((defmacro_eval, defmacro_macroexpand)),
        "while" => Some((while_eval, while_macroexpand)),
        _ => None,
    }
}

fn let_macroexpand(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    if args.is_empty() {
        return Err(LispErr::new("Invalid syntax for let".to_string()));
    }
    // Macroexpand all but first argument
    [
        Ok(LispObject::Symbol("let".into())),
        let_macroexpand_0(env, &args[0]),
    ]
    .into_iter()
    .chain(args[1..].iter().map(|arg| macroexpand(env, arg)))
    .collect()
}

fn let_macroexpand_0(env: &mut LispEnv, arg: &LispObject) -> Result<LispObject, LispErr> {
    if let LispObject::Vec(vars) = arg {
        vars.iter()
            .map(|x| match x {
                LispObject::Vec(vars) => match vars.as_slice() {
                    [var] => Ok(LispObject::Vec(vec![var.clone()])),
                    [var, val] => Ok(LispObject::Vec(vec![var.clone(), macroexpand(env, val)?])),
                    _ => Err(LispErr::new("Invalid syntax for let".to_string())),
                },
                s @ LispObject::Symbol(_) => Ok(s.clone()),
                _ => Err(LispErr::new("Invalid syntax for let".to_string())),
            })
            .collect()
    } else {
        Err(LispErr::new("Invalid syntax for let".to_string()))
    }
}

fn let_eval(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    if let LispObject::Vec(vars) = &args[0] {
        let frame = vars
            .iter()
            .map(|var| match var {
                LispObject::Vec(vars) => match vars.as_slice() {
                    [var] => Ok((let_ensure_symbol(var)?.to_string(), LispObject::nil())),
                    [var, val] => Ok((let_ensure_symbol(var)?.to_string(), eval(env, val)?)),
                    _ => Err(LispErr::new("Invalid syntax for let".to_string())),
                },
                LispObject::Symbol(s) => Ok((s.to_string(), LispObject::nil())),
                _ => Err(LispErr::new("Invalid syntax for let".to_string())),
            })
            .try_collect()?;
        env.add_frame(frame);
    } else {
        todo!("Error")
    }
    let result = progn_eval(env, &args[1..]);
    env.remove_frame();
    result
}

fn let_ensure_symbol(val: &LispObject) -> Result<&str, LispErr> {
    if let LispObject::Symbol(s) = val {
        Ok(s)
    } else {
        Err(LispErr::new("Invalid syntax for let".to_string()))
    }
}

fn cond_macroexpand(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    put_back(args.iter().map(|arg| {
        if let LispObject::Vec(vals) = arg {
            if vals.is_empty() {
                Err(LispErr::new("Invalid format for cond".to_string()))
            } else {
                Ok(LispObject::Vec(
                    vals.iter().map(|arg| macroexpand(env, arg)).try_collect()?,
                ))
            }
        } else {
            Err(LispErr::new("Invalid format for cond".to_string()))
        }
    }))
    .with_value(Ok(LispObject::Symbol("cond".into())))
    .collect()
}

fn cond_eval(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    for arg in args {
        if let LispObject::Vec(vals) = arg {
            if vals.is_empty() {
                return Err(LispErr::new("Invalid format for cond".to_string()));
            }
            if object_is_truthy(&eval(env, &vals[0])?) {
                return progn_eval(env, &vals[1..]);
            }
        } else {
            return Err(LispErr::new("Invalid format for cond".to_string()));
        }
    }
    Ok(LispObject::nil())
}

fn lambda_eval(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    let params = LispParams::parse(&args[0])?;
    let body = progn_macroexpand(env, &args[1..])?;
    Ok(LispObject::Function(LispFunction::new(params, body)))
}

fn progn_macroexpand(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    let mut result = args
        .iter()
        .map(|arg| macroexpand(env, arg))
        .collect::<Result<Vec<LispObject>, LispErr>>()?;
    result.insert(0, LispObject::Symbol("progn".into()));
    Ok(LispObject::Vec(result))
}

pub fn progn_eval(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    process_results(args.iter().map(|arg| eval(env, arg)), |iter| {
        iter.last().unwrap_or(LispObject::nil())
    })
}

fn quote_macroexpand(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    let mut vals = vec![LispObject::Symbol("quote".into())];
    vals.extend(args.iter().cloned());
    Ok(LispObject::Vec(vals))
}

fn quote_eval(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 1);
    Ok(args[0].clone())
}

fn error_macroexpand(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 1);
    let msg = macroexpand(env, &args[0])?;
    Ok(LispObject::Vec(vec![
        LispObject::Symbol("error".into()),
        msg,
    ]))
}

fn error_eval(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 1);
    let msg = eval(env, &args[0])?;
    Err(LispErr::new(msg.to_string()))
}

fn defmacro_macroexpand(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert!(args.len() >= 2);
    let name = match &args[0] {
        LispObject::Symbol(s) => s.to_string(),
        o => return Err(LispErr::new(format!("Invalid name for macro: {}", o))),
    };
    let arglist = LispParams::parse(&args[1])?;
    let body = &args[2..];
    env.add_macro(name, arglist, body.to_vec());
    Ok(LispObject::nil())
}

fn defmacro_eval(_: &mut LispEnv, _: &[LispObject]) -> Result<LispObject, LispErr> {
    Err(LispErr::new("Unexpected defmacro".to_string()))
}

fn while_macroexpand(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert!(!args.is_empty());
    let mut result = vec![LispObject::Symbol("while".into())];
    process_results(args.iter().map(|x| macroexpand(env, x)), |iter| {
        result.extend(iter)
    })?;
    Ok(LispObject::Vec(result))
}

fn while_eval(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert!(!args.is_empty());
    while object_is_truthy(&eval(env, &args[0])?) {
        progn_eval(env, &args[1..])?;
    }
    Ok(LispObject::nil())
}

fn object_is_truthy(value: &LispObject) -> bool {
    !matches!(value, LispObject::Vec(vals) if vals.is_empty())
}
