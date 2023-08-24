use std::rc::Rc;

use itertools::Itertools;

use crate::{
    env::LispEnv,
    special_forms::get_special_form,
    types::{LispErr, LispFunction, LispObject},
};

pub fn eval(env: &mut LispEnv, code: &LispObject) -> Result<LispObject, LispErr> {
    match code {
        LispObject::Symbol(s) => eval_symbol(env, s),
        c @ LispObject::Character(_) => Ok(c.clone()),
        n @ LispObject::Number(_) => Ok(n.clone()),
        f @ LispObject::Float(_) => Ok(f.clone()),
        r @ LispObject::Rational(_) => Ok(r.clone()),
        LispObject::Vec(vals) => eval_vec(env, vals),
        s @ LispObject::String(_) => Ok(s.clone()),
        f @ LispObject::NativeFn(_) => Ok(f.clone()),
        f @ LispObject::Function(_) => Ok(f.clone()),
        LispObject::True => Ok(LispObject::True),
    }
}

fn eval_symbol(env: &mut LispEnv, symbol_name: &Rc<str>) -> Result<LispObject, LispErr> {
    if &**symbol_name == "t" {
        Ok(LispObject::True)
    } else if &**symbol_name == "nil" {
        Ok(LispObject::nil())
    } else if symbol_name.starts_with([':', '&']) {
        Ok(LispObject::Symbol(symbol_name.clone()))
    } else if let Option::Some(obj) = env.get_symbol(symbol_name) {
        Ok(obj.clone())
    } else {
        Err(LispErr::new(format!("Name {} is unbound", symbol_name)))
    }
}

pub fn eval_vec(env: &mut LispEnv, vals: &[LispObject]) -> Result<LispObject, LispErr> {
    match vals {
        [] => Ok(LispObject::nil()),
        [name, args @ ..] => {
            if let LispObject::Symbol(name) = name {
                eval_named_fn(env, name, args)
            } else {
                Err(LispErr::new(format!(
                    "Invalid function name: must be a symbol, not {}",
                    name
                )))
            }
        }
    }
}

pub fn eval_named_fn(
    env: &mut LispEnv,
    name: &str,
    args: &[LispObject],
) -> Result<LispObject, LispErr> {
    // FIXME: Deal with macros
    if let Option::Some(obj) = env.get_function(name) {
        let obj = obj.clone(); // TODO: Remove clone here
        let evaled_args: Vec<LispObject> = args.iter().map(|x| eval(env, x)).try_collect()?;
        match obj {
            LispObject::NativeFn(func) => func(env, &evaled_args),
            LispObject::Function(obj) => eval_fn(env, &obj, &evaled_args),
            _ => Err(LispErr::new("Invalid function".into())),
        }
    } else if let Option::Some((form, _)) = get_special_form(name) {
        form(env, args)
    } else {
        Err(LispErr::new(format!("Name {} is unbound", name)))
    }
}

pub fn eval_fn(
    env: &mut LispEnv,
    func: &LispFunction,
    vals: &[LispObject],
) -> Result<LispObject, LispErr> {
    env.with_lexical((*func.env).clone(), |env| {
        env.add_frame(func.params.to_frame(vals)?);
        let result = eval(env, &func.body);
        env.remove_frame();
        result
    })
}
