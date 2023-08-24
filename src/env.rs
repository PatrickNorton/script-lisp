use std::{
    collections::{hash_map::Entry, HashMap},
    mem::take,
    rc::Rc,
};

use crate::{params::LispParams, types::LispObject};

#[derive(Debug)]
pub struct LispEnv {
    frames: LispLexical,
    variables: HashMap<String, (LispObject, bool)>,
    functions: HashMap<String, LispObject>,
    macros: HashMap<String, (LispParams, Vec<LispObject>)>,
}

#[derive(Debug, Clone)]
pub struct LispLexical {
    frames: Vec<Rc<HashMap<String, LispObject>>>,
}

impl LispEnv {
    pub fn new() -> Self {
        Self {
            frames: LispLexical::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            macros: HashMap::new(),
        }
    }

    pub fn get_symbol(&self, symbol: &str) -> Option<&LispObject> {
        self.frames
            .get_symbol(symbol)
            .or_else(|| self.variables.get(symbol).map(|(x, _)| x))
    }

    pub fn get_function(&self, symbol: &str) -> Option<&LispObject> {
        self.functions.get(symbol)
    }

    pub fn add_functions(&mut self, vals: HashMap<String, LispObject>) {
        self.functions.extend(vals)
    }

    pub fn add_macro(&mut self, name: String, args: LispParams, body: Vec<LispObject>) {
        self.macros.insert(name, (args, body));
    }

    pub fn get_macro(&self, name: &str) -> Option<&(LispParams, Vec<LispObject>)> {
        self.macros.get(name)
    }

    pub fn add_frame(&mut self, vals: HashMap<String, LispObject>) {
        self.frames.add_frame(vals);
    }

    pub fn remove_frame(&mut self) {
        self.frames.remove_frame();
    }

    pub fn set(&mut self, symbol: String, val: LispObject) {
        if let Option::Some((symbol, val)) = self.frames.try_set(symbol, val) {
            if let Entry::Occupied(mut e) = self.variables.entry(symbol) {
                // FIXME: Check if variable is mutable beforehand
                e.insert((val, false));
            } else {
                todo!("Error")
            }
        }
    }

    pub fn setfn(&mut self, symbol: String, val: LispObject) {
        self.functions.insert(symbol, val);
    }

    pub fn defvar(&mut self, symbol: String, val: LispObject, is_const: bool) {
        self.variables.insert(symbol, (val, is_const));
    }

    pub fn clone_lexical(&self) -> LispLexical {
        self.frames.clone()
    }

    pub fn with_lexical<T>(
        &mut self,
        frames: LispLexical,
        func: impl FnOnce(&mut LispEnv) -> T,
    ) -> T {
        // NOTE: This temporarily leaves `self` in an unreasonable state
        // (everything except the lexical environment is empty); it is important
        // to make sure that all fields get rewritten from the temporary
        // `LispEnv` at the end of the function.
        let mut new_env = Self {
            frames,
            variables: take(&mut self.variables),
            functions: take(&mut self.functions),
            macros: take(&mut self.macros),
        };
        let result = func(&mut new_env);
        self.variables = new_env.variables;
        self.functions = new_env.functions;
        self.macros = new_env.macros;
        result
    }
}

impl LispLexical {
    pub const fn new() -> Self {
        Self { frames: Vec::new() }
    }

    pub fn get_symbol(&self, symbol: &str) -> Option<&LispObject> {
        self.frames.iter().rev().find_map(|frame| frame.get(symbol))
    }

    pub fn add_frame(&mut self, vals: HashMap<String, LispObject>) {
        self.frames.push(Rc::new(vals));
    }

    pub fn remove_frame(&mut self) {
        self.frames
            .pop()
            .expect("Cannot remove frame when no frames exist");
    }

    pub fn try_set(&mut self, symbol: String, val: LispObject) -> Option<(String, LispObject)> {
        for frame in self.frames.iter_mut().rev() {
            if frame.contains_key(&symbol) {
                Rc::make_mut(frame).insert(symbol, val);
                return None;
            }
        }
        Some((symbol, val))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{params::LispParams, types::LispObject};

    use super::LispEnv;

    #[test]
    fn add_frames() {
        let mut env = LispEnv::new();
        env.add_frame(HashMap::from([("test".to_string(), LispObject::nil())]));
        assert!(env.get_symbol("test").is_some_and(|x| x.is_nil()));
        env.add_frame(HashMap::from([
            ("test".to_string(), LispObject::Symbol("foo".into())),
            ("test2".to_string(), LispObject::nil()),
        ]));
        assert!(env
            .get_symbol("test")
            .is_some_and(|x| x.is_symbol_named("foo")));
        assert!(env.get_symbol("test2").is_some_and(|x| x.is_nil()));
        env.remove_frame();
        assert!(env.get_symbol("test").is_some_and(|x| x.is_nil()));
    }

    #[test]
    fn test_functions() {
        let mut env = LispEnv::new();
        env.add_functions(HashMap::from([("test".to_string(), LispObject::nil())]));
        assert!(env.get_function("test").is_some_and(|x| x.is_nil()));
    }

    #[test]
    fn test_macros() {
        let mut env = LispEnv::new();
        env.add_macro("test".to_string(), LispParams::empty(), Vec::new());
        assert!(env.get_macro("test").is_some_and(|(_, x)| x.is_empty()));
    }

    #[test]
    #[ignore]
    fn test_with_env() {
        todo!()
    }
}
