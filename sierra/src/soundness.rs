use crate::{
    context::Context,
    edit_state::{put_results, take_args},
    error::Error,
    extensions::*,
    graph::*,
    ref_value::{mem_reducer, MemLocation, RefValue},
};
use std::collections::HashMap;
use Result::*;

pub fn validate(prog: &Program) -> Result<(), Error> {
    let mut block_start_states = vec![None; prog.blocks.len()];
    let h = Helper {
        prog: prog,
        reg: Registry::new(prog),
    };
    for f in &prog.funcs {
        let mut last = -2;
        let mut vars = VarStates::new();
        f.args.iter().try_for_each(|v| {
            let ti = h
                .reg
                .get_type_info(&v.ty)
                .map_err(|e| Error::TypeInfo(e, v.ty.clone()))?;
            last -= ti.size as i64;
            vars.insert(
                v.id.clone(),
                VarInfo {
                    ty: v.ty.clone(),
                    ref_val: if ti.size > 0 {
                        RefValue::Final(MemLocation::Local(last))
                    } else {
                        RefValue::Transient
                    },
                },
            );
            Ok(())
        })?;
        h.calc_block_infos(
            f.entry,
            BlockInfo {
                start_vars: vars,
                start_ctxt: Context {
                    local_cursur: 0,
                    local_allocated: false,
                    temp_used: false,
                    temp_cursur: 0,
                    temp_invalidated: false,
                },
                res_types: &f.res_types,
            },
            &mut block_start_states,
        )?;
    }
    h.validate(block_start_states)
}

type VarStates = HashMap<Identifier, VarInfo>;

#[derive(Debug, Clone)]
struct BlockInfo<'a> {
    start_vars: VarStates,
    start_ctxt: Context,
    res_types: &'a Vec<Type>,
}

struct Helper<'a> {
    pub prog: &'a Program,
    pub reg: Registry,
}

impl Helper<'_> {
    fn following_blocks_info<'a>(
        self: &Self,
        block_id: BlockId,
        block_info: BlockInfo<'a>,
    ) -> Result<Vec<(BlockId, BlockInfo<'a>)>, Error> {
        let BlockInfo {
            start_vars,
            start_ctxt,
            res_types,
        } = block_info;
        let mut vars = start_vars;
        let mut ctxt = start_ctxt;
        let block = &self.prog.blocks[block_id.0];
        for invc in &block.invocations {
            let (nvars, args_info) =
                take_args(vars, invc.args.iter()).map_err(|e| Error::EditState(block_id, e))?;
            let (mut states, fallthrough) = self
                .reg
                .transform(
                    &invc.ext,
                    PartialStateInfo {
                        vars: args_info,
                        context: ctxt,
                    },
                )
                .map_err(|e| Error::Extension(e, invc.to_string()))?;
            if states.len() != 1 {
                return Err(Error::ExtensionBranchesMismatch(invc.to_string()));
            }
            match fallthrough {
                Some(0) => {}
                _ => {
                    return Err(Error::ExtensionFallthroughMismatch(invc.to_string()));
                }
            }
            let PartialStateInfo {
                vars: results_info,
                context: nctxt,
            } = states.remove(0);
            if results_info.len() != invc.results.len() {
                return Err(Error::ExtensionResultSizeMismatch(invc.to_string()));
            }
            ctxt = handle_temp_invalidation(&nvars, nctxt)?;
            vars = put_results(nvars, izip!(invc.results.iter(), results_info.into_iter()))
                .map_err(|e| Error::EditState(block_id, e))?;
        }

        match &block.exit {
            BlockExit::Return(ref_ids) => {
                let (vars, used_vars) =
                    take_args(vars, ref_ids.iter()).map_err(|e| Error::EditState(block_id, e))?;
                let mut res_mem: Option<(MemLocation, usize)> = None;
                for (id, v, ty) in izip!(ref_ids.iter(), used_vars.iter(), res_types.iter()) {
                    if v.ty != *ty {
                        return Err(Error::FunctionReturnTypeMismatch(block_id, id.clone()));
                    }
                    let ti = self
                        .reg
                        .get_type_info(ty)
                        .map_err(|e| Error::TypeInfo(e, ty.clone()))?;
                    if ti.size == 0 {
                        continue;
                    }
                    let loc = match v.ref_val {
                        RefValue::Final(MemLocation::Temp(offset)) => Ok(MemLocation::Temp(offset)),
                        _ => Err(Error::FunctionReturnLocationMismatch(block_id, id.clone())),
                    }?;
                    res_mem = Some(match res_mem {
                        None => Ok((loc, ti.size)),
                        Some(prev) => mem_reducer(prev, (loc, ti.size)).ok_or_else(|| {
                            Error::FunctionReturnLocationMismatch(block_id, id.clone())
                        }),
                    }?);
                }
                match res_mem {
                    Some((MemLocation::Temp(base), size))
                        if base + size as i64 != ctxt.temp_cursur as i64 =>
                    {
                        return Err(Error::FunctionReturnLocationNotEndOfTemp(
                            block_id,
                            base + size as i64,
                            ctxt.temp_cursur,
                        ));
                    }
                    _ => {}
                }
                if vars.is_empty() {
                    Ok(vec![])
                } else {
                    Err(Error::FunctionRemainingOwnedObjects(
                        vars.into_keys().collect(),
                    ))
                }
            }
            BlockExit::Jump(j) => {
                let (vars, args_info) =
                    take_args(vars, j.args.iter()).map_err(|e| Error::EditState(block_id, e))?;
                let (states, fallthrough) = self
                    .reg
                    .transform(
                        &j.ext,
                        PartialStateInfo {
                            vars: args_info,
                            context: ctxt,
                        },
                    )
                    .map_err(|e| Error::Extension(e, j.to_string()))?;
                if states.len() != j.branches.len() {
                    return Err(Error::ExtensionBranchesMismatch(j.to_string()));
                }
                match fallthrough {
                    Some(i) if j.branches[i].target != BranchTarget::Fallthrough => {
                        return Err(Error::ExtensionFallthroughMismatch(j.to_string()));
                    }
                    _ => {}
                }
                let mut next_states = vec![];
                for (
                    branch,
                    PartialStateInfo {
                        vars: results_info,
                        context: ctxt,
                    },
                ) in izip!(j.branches.iter(), states.into_iter())
                {
                    if results_info.len() != branch.exports.len() {
                        return Err(Error::ExtensionResultSizeMismatch(j.to_string()));
                    }
                    next_states.push((
                        match branch.target {
                            BranchTarget::Fallthrough => BlockId(block_id.0 + 1),
                            BranchTarget::Block(b) => b,
                        },
                        as_block_info(
                            put_results(
                                vars.clone(),
                                izip!(branch.exports.iter(), results_info.into_iter(),),
                            )
                            .map_err(|e| Error::EditState(block_id, e))?,
                            handle_temp_invalidation(&vars, ctxt)?,
                            res_types,
                        ),
                    ))
                }
                Ok(next_states)
            }
        }
    }

    fn calc_block_infos<'a>(
        self: &Self,
        block: BlockId,
        info: BlockInfo<'a>,
        results: &mut Vec<Option<BlockInfo<'a>>>,
    ) -> Result<(), Error> {
        if block.0 >= results.len() {
            return Err(Error::FunctionBlockOutOfBounds);
        }
        if results[block.0].is_some() {
            return Ok(());
        }
        results[block.0] = Some(info.clone());
        for (next_block, next_block_info) in self.following_blocks_info(block, info)? {
            self.calc_block_infos(next_block, next_block_info, results)?;
        }
        Ok(())
    }

    fn validate<'a>(self: &Self, block_infos: Vec<Option<BlockInfo<'a>>>) -> Result<(), Error> {
        for (b, block_info) in izip!((0..).map(|b| BlockId(b)), &block_infos) {
            let block_info = block_info
                .as_ref()
                .ok_or_else(|| Error::UnusedBlock(b.clone()))?;
            for (next_block, next_block_info) in
                self.following_blocks_info(b, block_info.clone())?
            {
                if next_block.0 >= block_infos.len() {
                    return Err(Error::FunctionBlockOutOfBounds);
                }
                validate_eq(
                    next_block,
                    &next_block_info,
                    block_infos[next_block.0]
                        .as_ref()
                        .ok_or_else(|| Error::UnusedBlock(b.clone()))?,
                )?;
            }
        }
        Ok(())
    }
}

fn validate_eq(block: BlockId, info: &BlockInfo, expected: &BlockInfo) -> Result<(), Error> {
    if info.res_types != expected.res_types {
        unreachable!()
    } else if info.start_vars.len() != expected.start_vars.len() {
        Err(Error::FunctionBlockIdentifiersMismatch(
            block,
            info.start_vars.keys().map(|x| x.clone()).collect(),
            expected.start_vars.keys().map(|x| x.clone()).collect(),
        ))
    } else {
        expected
            .start_vars
            .iter()
            .try_for_each(|(id, expected_var_state)| match info.start_vars.get(id) {
                None => Err(Error::FunctionBlockIdentifiersMismatch(
                    block,
                    info.start_vars.keys().map(|x| x.clone()).collect(),
                    expected.start_vars.keys().map(|x| x.clone()).collect(),
                )),
                Some(var_state) => {
                    if var_state.ty != expected_var_state.ty {
                        Err(Error::FunctionBlockIdentifierTypeMismatch(
                            block,
                            id.clone(),
                            expected_var_state.ty.clone(),
                            var_state.ty.clone(),
                        ))
                    } else if var_state.ref_val != expected_var_state.ref_val {
                        Err(Error::FunctionBlockIdentifierLocationMismatch(
                            block,
                            id.clone(),
                            expected_var_state.ref_val.clone(),
                            var_state.ref_val.clone(),
                        ))
                    } else {
                        Ok(())
                    }
                }
            })
    }
}

fn handle_temp_invalidation(vars: &VarStates, mut ctxt: Context) -> Result<Context, Error> {
    if ctxt.temp_invalidated {
        ctxt.temp_invalidated = false;
        ctxt.temp_used = true;
        for (id, var_state) in vars.iter() {
            match &var_state.ref_val {
                RefValue::Final(MemLocation::Temp(_))
                | RefValue::Op(MemLocation::Temp(_), _, _)
                | RefValue::Op(MemLocation::Local(_), _, MemLocation::Temp(_))
                | RefValue::OpWithConst(MemLocation::Temp(_), _, _) => {
                    return Err(Error::UsedTempMemoryInvalidated(id.clone()));
                }
                _ => {}
            }
        }
    }
    Ok(ctxt)
}

fn as_block_info<'a>(
    mut vars: VarStates,
    mut ctxt: Context,
    res_types: &'a Vec<Type>,
) -> BlockInfo {
    if ctxt.temp_cursur != 0 {
        let fix = |offset: &mut i64| {
            *offset -= ctxt.temp_cursur as i64;
        };
        for (_, v) in vars.iter_mut() {
            match &mut v.ref_val {
                RefValue::Final(MemLocation::Temp(offset)) => fix(offset),
                RefValue::Op(MemLocation::Temp(offset1), _, MemLocation::Temp(offset2)) => {
                    fix(offset1);
                    fix(offset2);
                }
                RefValue::Op(MemLocation::Local(_), _, MemLocation::Temp(offset)) => fix(offset),
                RefValue::Op(MemLocation::Temp(offset), _, MemLocation::Local(_)) => fix(offset),
                RefValue::OpWithConst(MemLocation::Temp(offset), _, _) => fix(offset),
                _ => {}
            }
        }
        ctxt.temp_cursur = 0;
    }
    BlockInfo {
        start_vars: vars,
        start_ctxt: ctxt,
        res_types: res_types,
    }
}

#[cfg(test)]
mod function {
    use super::*;
    use crate::{utils::gas_type, ProgramParser};

    #[test]
    fn empty() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse("Some@0[ap += unknown](gb: GasBuiltin, a: felt) -> (GasBuiltin, felt);")
                    .unwrap()
            ),
            Err(Error::FunctionBlockOutOfBounds)
        );
    }

    #[test]
    fn basic_return() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(&pp.parse(r#"
                split_gas<1, 2>(cost) -> (cost_for_next, cost);
                add<int>(a, b) -> (a_plus_b_deferred);
                store<Temp, int>(a_plus_b_deferred, cost_for_next) -> (a_plus_b);
                split_gas<1, 1>(cost) -> (cost_for_next, cost_for_last);
                sub<int>(c, d) -> (c_minus_d_deferred);
                store<Temp, int>(c_minus_d_deferred, cost_for_next) -> (c_minus_d);
                mul<int>(a_plus_b, c_minus_d) -> (a_plus_b_mul_c_minus_d_deferred);
                store<Temp, int>(a_plus_b_mul_c_minus_d_deferred, cost_for_last) -> (a_plus_b_mul_c_minus_d);
                return(a_plus_b_mul_c_minus_d);

                Other@0[ap += 5](a: int, b: int, c: int, d: int, cost: Gas<3>) -> (int);"#).unwrap()),
            Ok(())
        );
    }

    #[test]
    fn inifinite_gas_take_or_return() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                store<Temp, GasBuiltin>(gb, store_cost) { fallthrough(gb) };
                get_gas<1, 1>(gb, get_gas_cost) { 0(gb, get_gas_cost, store_cost) fallthrough(gb) };
                return(gb);
                split_gas<1, 1, 1>(cost) -> (get_gas_cost, jump_cost, store_cost);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb, store_cost) -> (gb);
                jump(jump_cost) { 1() };

                Other@3[ap += unknown](gb: GasBuiltin, cost: Gas<3>) -> (GasBuiltin);"#
                )
                .unwrap()
            ),
            Ok(())
        );
    }

    #[test]
    fn inifinite_gas_take_or_return_bad_fallthrough() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                return(gb);
                store<Temp, GasBuiltin>(gb, store_cost) { fallthrough(gb) };
                get_gas<1, 1>(gb, get_gas_cost) { 1(gb, get_gas_cost, store_cost) 0(gb) };
                split_gas<1, 1, 1>(cost) -> (get_gas_cost, jump_cost, store_cost);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb, store_cost) -> (gb);
                jump(jump_cost) { 2() };
                
                Some@3[ap += unknown](gb: GasBuiltin, cost: Gas<3>) -> (GasBuiltin);"#
                )
                .unwrap()
            ),
            Err(Error::ExtensionFallthroughMismatch(
                "get_gas<1, 1>(gb, get_gas_cost) {\n1(gb, get_gas_cost, store_cost)\n0(gb)\n}"
                    .to_string()
            ))
        );
    }

    #[test]
    fn gas_mismatch() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                store<Temp, GasBuiltin>(gb, store_cost) { fallthrough(gb) };
                get_gas<2, 1>(gb, get_gas_cost) { 0(gb, get_gas_cost, store_cost) fallthrough(gb) };
                return(gb);
                split_gas<1, 1, 1>(cost) -> (get_gas_cost, jump_cost, store_cost);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb, store_cost) -> (gb);
                jump(jump_cost) { 1() };

                Other@3[ap += unknown](gb: GasBuiltin, cost: Gas<3>) -> (GasBuiltin);"#
                )
                .unwrap()
            ),
            Err(Error::FunctionBlockIdentifierTypeMismatch(
                BlockId(1),
                Identifier("get_gas_cost".to_string()),
                gas_type(1),
                gas_type(2)
            ))
        );
    }
}
