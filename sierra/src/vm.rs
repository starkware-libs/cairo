use crate::{extensions::Registry, graph::*};
use std::collections::HashMap;
use Result::*;

struct Executer<'a> {
    prog: &'a Program,
    func: &'a Function,
    reg: &'a Registry,
}

type ExecuterMap<'a> = HashMap<String, Executer<'a>>;

pub enum Error {
    MissingFunctionCall,
}

impl Executer<'_> {
    fn exec(
        self: &Self,
        func_execs: &ExecuterMap,
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<Vec<i64>>, Error> {
        Ok(inputs)
    }
}

pub fn run(prog: &Program, name: &str, inputs: Vec<Vec<i64>>) -> Result<Vec<Vec<i64>>, Error> {
    let registry = Registry::new(prog);
    let mut func_execs = ExecuterMap::new();
    for f in &prog.funcs {
        func_execs.insert(
            f.name.clone(),
            Executer {
                prog: prog,
                func: f,
                reg: &registry,
            },
        );
    }
    match func_execs.get(name) {
        None => Err(Error::MissingFunctionCall),
        Some(exec) => exec.exec(&func_execs, inputs),
    }
}
