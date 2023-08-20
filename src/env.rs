use std::collections::{hash_map::Entry, HashMap};

use crate::{params::LispParams, types::LispObject};

#[derive(Debug)]
pub struct LispEnv {
    frames: Vec<HashMap<String, LispObject>>,
    variables: HashMap<String, LispObject>,
    functions: HashMap<String, LispObject>,
    macros: HashMap<String, (LispParams, Vec<LispObject>)>,
}

impl LispEnv {
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            macros: HashMap::new(),
        }
    }

    pub fn get_symbol(&self, symbol: &str) -> Option<&LispObject> {
        self.frames
            .iter()
            .find_map(|frame| frame.get(symbol))
            .or_else(|| self.variables.get(symbol))
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
        self.frames.push(vals);
    }

    pub fn remove_frame(&mut self) {
        self.frames
            .pop()
            .expect("Cannot remove frame when no frames exist");
    }

    pub fn set(&mut self, symbol: String, val: LispObject) {
        for frame in self.frames.iter_mut().rev() {
            // We do it this way because the other way would require a clone of
            // the symbol for each iteration, which is much slower in practice.
            #[allow(clippy::map_entry)]
            if frame.contains_key(&symbol) {
                frame.insert(symbol, val);
                return;
            }
        }
        if let Entry::Occupied(mut e) = self.variables.entry(symbol) {
            e.insert(val);
        } else {
            todo!("Error")
        }
    }

    pub fn setfn(&mut self, symbol: String, val: LispObject) {
        self.functions.insert(symbol, val);
    }
}
