use crate::{
    env::LispEnv,
    special_forms::{get_special_form, progn_eval},
    types::{LispErr, LispObject},
};

pub fn macroexpand(env: &mut LispEnv, arg: &LispObject) -> Result<LispObject, LispErr> {
    match arg {
        LispObject::Vec(args) => macroexpand_vec(env, args),
        x => Ok(x.clone()),
    }
}

fn macroexpand_vec(env: &mut LispEnv, args: &[LispObject]) -> Result<LispObject, LispErr> {
    match args {
        [] => Ok(LispObject::nil()),
        vals @ [LispObject::Symbol(x), rest @ ..] => {
            if let Option::Some((args, body)) = env.get_macro(x) {
                let frame = args.to_frame(rest)?;
                let body = body.clone(); // TODO: Remove clone
                env.add_frame(frame);
                let result = progn_eval(env, &body);
                env.remove_frame();
                result.and_then(|x| macroexpand(env, &x))
            } else if let Option::Some((_, mac)) = get_special_form(x) {
                mac(env, rest)
            } else {
                vals.iter().map(|x| macroexpand(env, x)).collect()
            }
        }
        vals => Ok(vals.iter().cloned().collect()),
    }
}
