use std::{fs::File, path::Path};

use crate::{
    env::LispEnv,
    eval::eval,
    lexer::lex,
    macroexpand::macroexpand,
    native_fns::NATIVE_FNS,
    parser::parse,
    types::{LispErr, LispObject},
};

mod chars;
mod env;
mod eval;
mod lexer;
mod macroexpand;
mod native_fns;
mod parser;
mod special_forms;
mod types;

fn main() -> Result<(), LispErr> {
    let mut env = LispEnv::new();
    env.add_functions(
        NATIVE_FNS
            .iter()
            .map(|(name, val)| (name.to_string(), LispObject::NativeFn(*val)))
            .collect(),
    );
    run_file(&mut env, "./lib/builtins.scl")?;
    run_file(&mut env, "./lib/test.scl")
}

fn run_file(env: &mut LispEnv, file: impl AsRef<Path>) -> Result<(), LispErr> {
    let parsed = parse(lex(File::open(file)?)?.into())?;
    for expr in parsed {
        let expanded = macroexpand(env, &expr)?;
        eval(env, &expanded)?;
    }
    Ok(())
}
