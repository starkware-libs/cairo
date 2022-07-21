use crate::{
    edit_state::{put_results, take_args, Error as EditError},
    extensions::{Error as ExtError, Registry},
    graph::*,
};
use std::collections::HashMap;
use Result::*;

struct Executer<'a> {
    prog: &'a Program,
    func: &'a Function,
    reg: &'a Registry,
}

type ExecuterMap<'a> = HashMap<String, Executer<'a>>;

#[derive(Debug, PartialEq)]
pub enum Error {
    MissingFunctionCall(String),
    TypeInfo(ExtError, Type),
    Extension(ExtError, String),
    EditState(BlockId, EditError),
    FunctionNonTupleInput(String),
    FunctionArgsSizeMismatch(String, usize),
}

impl Executer<'_> {
    fn exec(
        self: &Self,
        func_execs: &ExecuterMap,
        inputs: Vec<Vec<i64>>,
    ) -> Result<Vec<i64>, Error> {
        if inputs.len() != 1 {
            println!("{:?}", inputs);
            return Err(Error::FunctionNonTupleInput(self.func.name.clone()));
        }
        let mut vars = HashMap::<Identifier, Vec<i64>>::new();
        let mut offset = 0;
        for v in &self.func.args {
            let size = self
                .reg
                .get_type_info(&v.ty)
                .map_err(|e| Error::TypeInfo(e, v.ty.clone()))?
                .size;
            if offset + size > inputs[0].len() {
                return Err(Error::FunctionArgsSizeMismatch(
                    self.func.name.clone(),
                    inputs[0].len(),
                ));
            }
            vars.insert(
                v.id.clone(),
                inputs[0][offset..offset + size]
                    .iter()
                    .map(|v| *v)
                    .collect(),
            );
            offset += size;
        }
        let exec = |ext: &Extension, inputs| match func_execs.get(&ext.name) {
            None => self
                .reg
                .exec(ext, inputs)
                .map_err(|e| Error::Extension(e, ext.to_string())),
            Some(e) => Ok((vec![e.exec(func_execs, inputs)?], 0)),
        };
        let mut b = self.func.entry;
        loop {
            let block = &self.prog.blocks[b.0];
            for invc in &block.invocations {
                let (nvars, args_info) =
                    take_args(vars, invc.args.iter()).map_err(|e| Error::EditState(b, e))?;
                let (results_info, _) = exec(&invc.ext, args_info)?;
                vars = put_results(nvars, izip!(invc.results.iter(), results_info.into_iter()))
                    .map_err(|e| Error::EditState(b, e))?;
            }

            match &block.exit {
                BlockExit::Return(ref_ids) => {
                    let (_, used_vars) =
                        take_args(vars, ref_ids.iter()).map_err(|e| Error::EditState(b, e))?;
                    return Ok(used_vars.into_iter().flatten().collect());
                }
                BlockExit::Jump(j) => {
                    let (nvars, args_info) =
                        take_args(vars, j.args.iter()).map_err(|e| Error::EditState(b, e))?;
                    let (results_info, chosen_branch) = exec(&j.ext, args_info)?;
                    let chosen_branch = &j.branches[chosen_branch];
                    vars = put_results(
                        nvars,
                        izip!(chosen_branch.exports.iter(), results_info.into_iter()),
                    )
                    .map_err(|e| Error::EditState(b, e))?;
                    b = match chosen_branch.target {
                        BranchTarget::Block(next) => next,
                        BranchTarget::Fallthrough => BlockId(b.0 + 1),
                    };
                }
            }
        }
    }
}

pub fn run(prog: &Program, name: &str, inputs: Vec<i64>) -> Result<Vec<i64>, Error> {
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
        None => Err(Error::MissingFunctionCall(name.to_string())),
        Some(exec) => exec.exec(&func_execs, vec![inputs]),
    }
}
