use std::sync::atomic::{AtomicUsize, Ordering};

use itertools::Itertools;
use num::{BigInt, BigRational, FromPrimitive, Integer, ToPrimitive, Zero};

use crate::{
    env::LispEnv,
    eval::{eval_fn, eval_named_fn},
    macroexpand::macroexpand,
    types::{LispErr, LispObject, NativeFn},
};

// TODO: Add argument count

// TODO: It would be nice to implement more of these in Lisp, but I'm not sure
// how many of them that's possible for
pub const NATIVE_FNS: &[(&str, NativeFn)] = &[
    ("setf", setf),
    ("setfn", setfn),
    ("gensym", gensym),
    ("macroexpand", macroexp),
    ("symbol-function", symbol_function),
    ("map", map),
    ("funcall", funcall),
    ("apply", apply),
    ("list", list),
    ("append", append),
    ("push", push),
    ("pop", pop),
    ("nth", nth),
    ("length", length),
    ("display", display),
    ("disps", disps), // TODO: Rename
    ("print", print),
    ("numberp", numberp),
    ("stringp", stringp),
    ("to-float", to_float),
    ("+", plus),
    ("-", minus),
    ("*", times),
    ("/", divn),
    ("floor", floor),
    ("ceiling", ceiling),
    ("truncate", truncate),
    ("=", equals),
    ("<", lt),
    ("zerop", zerop),
];

fn setf(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 2); // TODO: Multiple setfs?
    match &args[0] {
        LispObject::Symbol(symb) => env.set(symb.to_string(), args[1].clone()),
        s => todo!("Error/setf handler: {}", s),
    }
    Ok(args[1].clone())
}

fn setfn(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 2); // TODO: Multiple setfs?
    match &args[0] {
        LispObject::Symbol(symb) => env.setfn(symb.to_string(), args[1].clone()),
        _ => todo!("Error/setf handler"),
    }
    Ok(args[1].clone())
}

fn gensym(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert!(args.is_empty());
    // Should be enough to prevent overlap in practice -- if a computer creates
    // a usize's worth of temporary symbols it'll run out of memory first
    static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);
    Ok(LispObject::Symbol(
        format!("#:G{}", GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed)).into(),
    ))
}

fn macroexp(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 1);
    macroexpand(env, &args[0])
}

fn list(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    Ok(args.iter().cloned().collect())
}

fn append(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    let mut result = Vec::new();
    for arg in args {
        match arg {
            LispObject::Vec(v) => result.extend(v.clone()),
            LispObject::String(s) => result.extend(s.chars().map(LispObject::Character)),
            _ => return Err(LispErr::new("Expected iterable object".into())),
        }
    }
    Ok(LispObject::Vec(result))
}

fn symbol_function(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 1);
    match &args[0] {
        LispObject::Symbol(symb) => Ok(env.get_function(symb).unwrap().clone()),
        v => todo!("Unknown value {:?}", v),
    }
}

fn map(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 2);
    let callable = &args[0];
    match &args[1] {
        LispObject::Vec(v) => v
            .iter()
            .map(|val| funcall_obj(env, callable, std::slice::from_ref(val)))
            .collect(),
        LispObject::String(s) => s
            .chars()
            .map(|c| funcall_obj(env, callable, &[LispObject::Character(c)]))
            .collect(),
        v => Err(LispErr::new(format!("Invalid argument to map: {}", v))),
    }
}

fn funcall(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert!(!args.is_empty());
    funcall_obj(env, &args[0], &args[1..])
}

fn apply(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert!(!args.is_empty());
    // Match here to avoid cloning when possible
    match args {
        [] => unreachable!(),
        [func] => funcall_obj(env, func, &[]),
        [func, arg] => {
            let last_vals = match arg {
                LispObject::Vec(v) => v,
                _ => {
                    return Err(LispErr::new(
                        "Expected list as last argument to apply".to_string(),
                    ))
                }
            };
            funcall_obj(env, func, last_vals)
        }
        [func, args @ .., last] => {
            let last_vals = match last {
                LispObject::Vec(v) => v,
                _ => {
                    return Err(LispErr::new(
                        "Expected list as last argument to apply".to_string(),
                    ))
                }
            };
            let mut vals = args.to_vec();
            vals.extend_from_slice(last_vals);
            funcall_obj(env, func, &vals)
        }
    }
}

fn funcall_obj(
    env: &mut LispEnv,
    func: &LispObject,
    args: &[LispObject],
) -> Result<LispObject, LispErr> {
    match func {
        LispObject::Symbol(s) => eval_named_fn(env, s, args),
        LispObject::NativeFn(f) => f(env, args),
        LispObject::Function(f) => eval_fn(env, f, args),
        val => Err(LispErr::new(format!(
            "Not a callable image object: {}",
            val
        ))),
    }
}

// TODO? Add optimizations for macros (e.g. pushf)?

fn push(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    if let [val, LispObject::Vec(list)] = args {
        let mut new_list = list.clone();
        new_list.push(val.clone());
        Ok(LispObject::Vec(new_list))
    } else {
        todo!()
    }
}

fn pop(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    if let [LispObject::Vec(list)] = args {
        let mut new_list = list.clone();
        new_list.pop();
        Ok(LispObject::Vec(new_list))
    } else {
        todo!()
    }
}

fn nth(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    if let [LispObject::Number(idx), LispObject::Vec(list)] = args {
        idx.to_usize()
            .and_then(|x| list.get(x))
            .cloned()
            .ok_or_else(|| {
                LispErr::new(format!(
                    "Index {idx} out of range for list of size {}",
                    list.len()
                ))
            })
    } else {
        todo!()
    }
}

fn length(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    if let [LispObject::Vec(list)] = args {
        Ok(LispObject::Number(list.len().into()))
    } else {
        todo!()
    }
}

fn numberp(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    Ok(matches!(args[0], LispObject::Number(_)).into())
}

fn stringp(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    Ok(matches!(args[0], LispObject::String(_)).into())
}

fn display(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 1);
    println!("{}", args[0]);
    Ok(LispObject::nil())
}

fn disps(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 1);
    Ok(LispObject::String(args[0].to_string()))
}

fn print(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 1);
    match &args[0] {
        LispObject::Symbol(_) => todo!(),
        LispObject::Character(c) => print!("{}", c),
        LispObject::Number(n) => print!("{}", n),
        LispObject::Float(f) => print!("{}", f),
        LispObject::Rational(r) => print!("{}", r),
        LispObject::Vec(_) => todo!(),
        LispObject::String(s) => print!("{}", s),
        LispObject::NativeFn(_) => todo!(),
        LispObject::Function(_) => todo!(),
        LispObject::True => todo!(),
    }
    Ok(LispObject::nil())
}

fn to_float(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    assert_eq!(args.len(), 1);
    match &args[0] {
        LispObject::Number(n) => Ok(LispObject::Float(n.to_f64().unwrap())),
        _ => todo!("Error"),
    }
}

fn plus(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    match args {
        [LispObject::Number(a), LispObject::Number(b)] => Ok(LispObject::Number(a + b)),
        [LispObject::Float(a), LispObject::Float(b)] => Ok(LispObject::Float(a + b)),
        [LispObject::Rational(a), LispObject::Rational(b)] => Ok(LispObject::Rational(a + b)),
        _ => todo!("Mismatched types to +: {}", args.iter().format(", ")),
    }
}

fn minus(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    match args {
        [LispObject::Number(a), LispObject::Number(b)] => Ok(LispObject::Number(a - b)),
        [LispObject::Float(a), LispObject::Float(b)] => Ok(LispObject::Float(a - b)),
        [LispObject::Rational(a), LispObject::Rational(b)] => Ok(LispObject::Rational(a - b)),
        _ => todo!("Mismatched types to -"),
    }
}

fn times(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    match args {
        [LispObject::Number(a), LispObject::Number(b)] => Ok(LispObject::Number(a * b)),
        [LispObject::Float(a), LispObject::Float(b)] => Ok(LispObject::Float(a * b)),
        [LispObject::Rational(a), LispObject::Rational(b)] => Ok(LispObject::Rational(a * b)),
        _ => todo!("Mismatched types to *"),
    }
}

fn divn(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    // TODO: Check for zero
    match args {
        [LispObject::Number(a), LispObject::Number(b)] => {
            Ok(LispObject::Rational(BigRational::new(a.clone(), b.clone())))
        }
        [LispObject::Float(a), LispObject::Float(b)] => Ok(LispObject::Float(a / b)),
        [LispObject::Rational(a), LispObject::Rational(b)] => Ok(LispObject::Rational(a / b)),
        _ => todo!("Mismatched types to /"),
    }
}

fn floor(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    match args {
        [LispObject::Number(a)] => Ok(LispObject::Number(a.clone())),
        [LispObject::Float(a)] => Ok(LispObject::Number(BigInt::from_f64(a.floor()).unwrap())),
        [LispObject::Rational(a)] => Ok(LispObject::Number(a.floor().numer().clone())),
        [LispObject::Number(a), LispObject::Number(b)] => Ok(LispObject::Number(a.div_floor(b))),
        _ => todo!(),
    }
}

fn ceiling(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    match args {
        [LispObject::Number(a)] => Ok(LispObject::Number(a.clone())),
        [LispObject::Float(a)] => Ok(LispObject::Number(BigInt::from_f64(a.ceil()).unwrap())),
        [LispObject::Rational(a)] => Ok(LispObject::Number(a.ceil().numer().clone())),
        [LispObject::Number(a), LispObject::Number(b)] => Ok(LispObject::Number(a.div_ceil(b))),
        _ => todo!(),
    }
}

fn truncate(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    match args {
        [LispObject::Number(a)] => Ok(LispObject::Number(a.clone())),
        [LispObject::Float(a)] => Ok(LispObject::Number(BigInt::from_f64(a.trunc()).unwrap())),
        [LispObject::Rational(a)] => Ok(LispObject::Number(a.trunc().numer().clone())),
        [LispObject::Number(a), LispObject::Number(b)] => Ok(LispObject::Number(a / b)),
        _ => todo!(),
    }
}

fn lt(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    match args {
        [LispObject::Number(a), LispObject::Number(b)] => Ok((a < b).into()),
        [LispObject::Float(a), LispObject::Float(b)] => Ok((a < b).into()),
        [LispObject::Rational(a), LispObject::Rational(b)] => Ok((a < b).into()),
        _ => todo!(),
    }
}

fn zerop(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    match args {
        [LispObject::Number(x)] => Ok(x.is_zero().into()),
        [LispObject::Float(f)] => Ok(f.is_zero().into()),
        [LispObject::Rational(r)] => Ok(r.is_zero().into()),
        _ => todo!(),
    }
}

fn equals(_: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    // TODO? Compare int and float
    match args {
        [LispObject::Number(a), LispObject::Number(b)] => Ok((a == b).into()),
        [LispObject::Float(a), LispObject::Float(b)] => Ok((a == b).into()),
        _ => todo!("Mismatched types to ="),
    }
}
