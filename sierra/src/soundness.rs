use crate::{
    context::{Context, ResourceMap},
    edit_state::{put_results, take_args},
    error::Error,
    extensions::*,
    graph::*,
    ref_value::{mem_reducer, MemLocation, RefValue},
    side_effects::SideEffects,
};
use std::collections::HashMap;
use Result::*;

pub fn validate(prog: &Program) -> Result<(), Error> {
    let mut block_start_infos = vec![None; prog.blocks.len()];
    let mut block_forward_infos = vec![None; prog.blocks.len()];
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
            BlockStartInfo {
                vars: vars,
                ctxt: Context {
                    local_cursur: 0,
                    local_allocated: false,
                    temp_used: false,
                    temp_cursur: 0,
                    temp_invalidated: false,
                    resources: ResourceMap::from_iter(
                        f.side_effects
                            .resource_usages
                            .iter()
                            .map(|(id, usage)| (id.clone(), *usage as i64)),
                    ),
                },
            },
            &f.res_types,
            &mut block_start_infos,
            &mut block_forward_infos,
        )?;
    }
    h.validate(block_start_infos, block_forward_infos)
}

type VarStates = HashMap<Identifier, VarInfo>;

#[derive(Debug, Clone)]
struct BlockStartInfo {
    vars: VarStates,
    ctxt: Context,
}

#[derive(Debug, Clone)]
struct BlockForwardInfo<'a> {
    res_types: &'a Vec<Type>,
    effects: SideEffects,
}

struct Helper<'a> {
    pub prog: &'a Program,
    pub reg: Registry,
}

impl Helper<'_> {
    fn following_blocks_info(
        self: &Self,
        block_id: BlockId,
        start_info: BlockStartInfo,
        res_types: &Vec<Type>,
    ) -> Result<Vec<(Option<BlockId>, BlockStartInfo, SideEffects)>, Error> {
        let BlockStartInfo { mut vars, mut ctxt } = start_info;
        let mut side_effects = SideEffects {
            ap_change: Some(0),
            local_writes: 0,
            resource_usages: ResourceMap::new(),
        };
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
                        context: ctxt.clone(),
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
            side_effects = side_effects.add(&SideEffects::new(&ctxt, &nctxt));
            ctxt = normalize_context(&nvars, nctxt)?;
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
                if !vars.is_empty() {
                    Err(Error::FunctionRemainingOwnedObjects(
                        vars.into_keys().collect(),
                    ))
                }else if ctxt.resources.values().any(|v|*v < 0){
                    Err(Error::FunctionRanOutOfResources(ctxt.resources.clone().into_iter().collect()))
                } else {
                    Ok(vec![(None, BlockStartInfo { vars, ctxt }, side_effects)])
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
                            context: ctxt.clone(),
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
                        context: nctxt,
                    },
                ) in izip!(j.branches.iter(), states.into_iter())
                {
                    if results_info.len() != branch.exports.len() {
                        return Err(Error::ExtensionResultSizeMismatch(j.to_string()));
                    }
                    let mut side_effects = side_effects.clone();
                    side_effects = side_effects.add(&SideEffects::new(&ctxt, &nctxt));
                    next_states.push((
                        Some(match branch.target {
                            BranchTarget::Fallthrough => BlockId(block_id.0 + 1),
                            BranchTarget::Block(b) => b,
                        }),
                        as_block_start_info(
                            put_results(
                                vars.clone(),
                                izip!(branch.exports.iter(), results_info.into_iter(),),
                            )
                            .map_err(|e| Error::EditState(block_id, e))?,
                            normalize_context(&vars, nctxt)?,
                        ),
                        side_effects,
                    ))
                }
                Ok(next_states)
            }
        }
    }

    fn calc_block_infos<'a>(
        self: &Self,
        block: BlockId,
        start_info: BlockStartInfo,
        res_types: &'a Vec<Type>,
        bsis: &mut Vec<Option<BlockStartInfo>>,
        bfis: &mut Vec<Option<BlockForwardInfo<'a>>>,
    ) -> Result<(), Error> {
        if block.0 >= bsis.len() {
            return Err(Error::FunctionBlockOutOfBounds);
        }
        if bsis[block.0].is_some() {
            return Ok(());
        }
        bsis[block.0] = Some(start_info.clone());
        let mut combined_effects: Option<SideEffects> = None;
        for (next_block, next_start_info, effects) in
            self.following_blocks_info(block, start_info, res_types)?
        {
            let effects = effects.add(&match next_block {
                None => SideEffects {
                    ap_change: Some(0),
                    local_writes: 0,
                    resource_usages: ResourceMap::new(),
                },
                Some(next_block) => {
                    self.calc_block_infos(next_block, next_start_info, res_types, bsis, bfis)?;
                    match &bfis[next_block.0] {
                        // cycle detected - ap change unknown!
                        None => SideEffects {
                            ap_change: None,
                            local_writes: 0,
                            resource_usages: ResourceMap::new(),
                        },
                        Some(next_forward_info) => next_forward_info.effects.clone(),
                    }
                }
            });
            combined_effects = Some(match combined_effects {
                None => effects,
                Some(combined) => combined.converge(&effects),
            });
        }
        bfis[block.0] = Some(BlockForwardInfo {
            res_types: res_types,
            effects: combined_effects.unwrap(),
        });
        Ok(())
    }

    fn validate<'a>(
        self: &Self,
        bsis: Vec<Option<BlockStartInfo>>,
        bfis: Vec<Option<BlockForwardInfo<'a>>>,
    ) -> Result<(), Error> {
        for (b, si, fi) in izip!((0..).map(|b| BlockId(b)), &bsis, &bfis) {
            let si = si.as_ref().ok_or_else(|| Error::UnusedBlock(b.clone()))?;
            let fi = fi.as_ref().ok_or_else(|| Error::UnusedBlock(b.clone()))?;
            for (next_block, next_si, _effects) in
                self.following_blocks_info(b, si.clone(), fi.res_types)?
            {
                if let Some(next_block) = next_block {
                    if next_block.0 >= bsis.len() {
                        return Err(Error::FunctionBlockOutOfBounds);
                    }
                    let exp_si = bsis[next_block.0]
                        .as_ref()
                        .ok_or_else(|| Error::UnusedBlock(b.clone()))?;
                    let next_fi = bfis[next_block.0]
                        .as_ref()
                        .ok_or_else(|| Error::UnusedBlock(b.clone()))?;
                    validate_eq(next_block, &next_si, exp_si)?;
                    if fi.res_types != next_fi.res_types {
                        return Err(Error::FunctionBlockReturnTypesMismatch(
                            next_block,
                            fi.res_types.clone(),
                            next_fi.res_types.clone(),
                        ));
                    }
                }
            }
        }
        for f in &self.prog.funcs {
            let bfi = &bfis[f.entry.0]
                .as_ref()
                .ok_or_else(|| Error::UnusedBlock(f.entry.clone()))?;
            if f.res_types != *bfi.res_types {
                return Err(Error::FunctionBlockReturnTypesMismatch(
                    f.entry,
                    f.res_types.clone(),
                    bfi.res_types.clone(),
                ));
            }
            // Adding the extra ap change per function call.
            let func_side_effects = SideEffects {
                ap_change: Some(2),
                local_writes: 0,
                resource_usages: ResourceMap::from([(Identifier("gas".to_string()), 2)]),
            }
            .add(&bfi.effects);
            if let Some(expected_ap_change) = &f.side_effects.ap_change {
                if Some(*expected_ap_change) != func_side_effects.ap_change {
                    return Err(Error::FunctionReturnApChangeMismatch(
                        f.name.clone(),
                        func_side_effects.ap_change,
                    ));
                }
            }
            for (id, expected_usage) in &f.side_effects.resource_usages {
                let usage = func_side_effects.resource_usages.get(&id).unwrap_or(&0);
                if *expected_usage as i64 != *usage {
                    return Err(Error::FunctionReturnResourceUsageMismatch(
                        f.name.clone(),
                        id.clone(),
                        *usage,
                    ));
                }
            }
        }
        Ok(())
    }
}

fn validate_eq(block: BlockId, si: &BlockStartInfo, exp_si: &BlockStartInfo) -> Result<(), Error> {
    if si.ctxt != exp_si.ctxt {
        Err(Error::FunctionBlockContextMismatch(
            block,
            si.ctxt.clone(),
            exp_si.ctxt.clone(),
        ))
    } else if si.vars.len() != exp_si.vars.len() {
        Err(Error::FunctionBlockIdentifiersMismatch(
            block,
            si.vars.keys().map(|x| x.clone()).collect(),
            exp_si.vars.keys().map(|x| x.clone()).collect(),
        ))
    } else {
        exp_si.vars.iter().try_for_each(
            |(
                id,
                VarInfo {
                    ty: exp_ty,
                    ref_val: exp_ref_val,
                },
            )| match si.vars.get(id) {
                None => Err(Error::FunctionBlockIdentifiersMismatch(
                    block,
                    si.vars.keys().map(|x| x.clone()).collect(),
                    exp_si.vars.keys().map(|x| x.clone()).collect(),
                )),
                Some(VarInfo { ty, ref_val }) => {
                    if ty != exp_ty {
                        Err(Error::FunctionBlockIdentifierTypeMismatch(
                            block,
                            id.clone(),
                            exp_ty.clone(),
                            ty.clone(),
                        ))
                    } else if ref_val != exp_ref_val {
                        Err(Error::FunctionBlockIdentifierLocationMismatch(
                            block,
                            id.clone(),
                            exp_ref_val.clone(),
                            ref_val.clone(),
                        ))
                    } else {
                        Ok(())
                    }
                }
            },
        )
    }
}

fn normalize_context(vars: &VarStates, mut ctxt: Context) -> Result<Context, Error> {
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

fn as_block_start_info(mut vars: VarStates, mut ctxt: Context) -> BlockStartInfo {
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
    BlockStartInfo { vars, ctxt }
}

#[cfg(test)]
mod function {
    use super::*;
    use crate::ProgramParser;

    #[test]
    fn empty() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse("Some@0[ap += unknown, gas -= 2](gb: GasBuiltin, a: felt) -> (GasBuiltin, felt);")
                    .unwrap()
            ),
            Err(Error::FunctionBlockOutOfBounds)
        );
    }

    #[test]
    fn basic_return() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                add<int>(a, b) -> (a_plus_b);
                store<Temp, int>(a_plus_b) -> (a_plus_b);
                sub<int>(c, d) -> (c_minus_d);
                store<Temp, int>(c_minus_d) -> (c_minus_d);
                mul<int>(a_plus_b, c_minus_d) -> (a_plus_b_mul_c_minus_d);
                store<Temp, int>(a_plus_b_mul_c_minus_d) -> (a_plus_b_mul_c_minus_d);
                return(a_plus_b_mul_c_minus_d);

                Other@0[ap += 5, gas -= 5](a: int, b: int, c: int, d: int) -> (int);"#
                )
                .unwrap()
            ),
            Ok(())
        );
    }

    #[test]
    fn basic_return_gas_not_stated() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                add<int>(a, b) -> (a_plus_b);
                store<Temp, int>(a_plus_b) -> (a_plus_b);
                sub<int>(c, d) -> (c_minus_d);
                store<Temp, int>(c_minus_d) -> (c_minus_d);
                mul<int>(a_plus_b, c_minus_d) -> (a_plus_b_mul_c_minus_d);
                store<Temp, int>(a_plus_b_mul_c_minus_d) -> (a_plus_b_mul_c_minus_d);
                return(a_plus_b_mul_c_minus_d);

                Other@0[ap += 5](a: int, b: int, c: int, d: int) -> (int);"#
                )
                .unwrap()
            ),
            Err(Error::FunctionRanOutOfResources(vec![(Identifier("gas".to_string()), -3)]))
        );
    }

    #[test]
    fn basic_return_gas_overuse() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                add<int>(a, b) -> (a_plus_b);
                store<Temp, int>(a_plus_b) -> (a_plus_b);
                sub<int>(c, d) -> (c_minus_d);
                store<Temp, int>(c_minus_d) -> (c_minus_d);
                mul<int>(a_plus_b, c_minus_d) -> (a_plus_b_mul_c_minus_d);
                store<Temp, int>(a_plus_b_mul_c_minus_d) -> (a_plus_b_mul_c_minus_d);
                return(a_plus_b_mul_c_minus_d);

                Other@0[ap += 5, gas -= 4](a: int, b: int, c: int, d: int) -> (int);"#
                )
                .unwrap()
            ),
            Err(Error::FunctionReturnResourceUsageMismatch(
                "Other".to_string(),
                Identifier("gas".to_string()),
                5
            ))
        );
    }

    #[test]
    fn basic_return_unknown_change() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                add<int>(a, b) -> (a_plus_b);
                store<Temp, int>(a_plus_b) -> (a_plus_b);
                sub<int>(c, d) -> (c_minus_d);
                store<Temp, int>(c_minus_d) -> (c_minus_d);
                mul<int>(a_plus_b, c_minus_d) -> (a_plus_b_mul_c_minus_d);
                store<Temp, int>(a_plus_b_mul_c_minus_d) -> (a_plus_b_mul_c_minus_d);
                return(a_plus_b_mul_c_minus_d);

                Other@0[ap += unknown, gas -= 5](a: int, b: int, c: int, d: int) -> (int);"#
                )
                .unwrap()
            ),
            Ok(())
        );
    }

    #[test]
    fn basic_return_wrong_ap_change() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                add<int>(a, b) -> (a_plus_b);
                store<Temp, int>(a_plus_b) -> (a_plus_b);
                sub<int>(c, d) -> (c_minus_d);
                store<Temp, int>(c_minus_d) -> (c_minus_d);
                mul<int>(a_plus_b, c_minus_d) -> (a_plus_b_mul_c_minus_d);
                store<Temp, int>(a_plus_b_mul_c_minus_d) -> (a_plus_b_mul_c_minus_d);
                return(a_plus_b_mul_c_minus_d);

                Other@0[ap += 4, gas -= 5](a: int, b: int, c: int, d: int) -> (int);"#
                )
                .unwrap()
            ),
            Err(Error::FunctionReturnApChangeMismatch(
                "Other".to_string(),
                Some(5)
            ))
        );
    }

    #[test]
    fn inifinite_gas_take_or_return() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                store<Temp, GasBuiltin>(gb) { fallthrough(gb) };
                get_gas<2>(gb) { 0(gb) fallthrough(gb) };
                return(gb);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb) -> (gb);
                jump() { 1() };

                Other@3[ap += unknown, gas -= 5](gb: GasBuiltin) -> (GasBuiltin);"#
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
                store<Temp, GasBuiltin>(gb) { fallthrough(gb) };
                get_gas<2>(gb) { 1(gb) 0(gb) };
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb) -> (gb);
                jump() { 2() };
                
                Some@3[ap += unknown, gas -= 5](gb: GasBuiltin) -> (GasBuiltin);"#
                )
                .unwrap()
            ),
            Err(Error::ExtensionFallthroughMismatch(
                "get_gas<2>(gb) {\n1(gb)\n0(gb)\n}".to_string()
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
                store<Temp, GasBuiltin>(gb) { fallthrough(gb) };
                get_gas<3>(gb) { 0(gb) fallthrough(gb) };
                return(gb);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb) -> (gb);
                jump() { 1() };

                Other@3[ap += unknown, gas -= 5](gb: GasBuiltin) -> (GasBuiltin);"#
                )
                .unwrap()
            ),
            Err(Error::FunctionBlockContextMismatch(
                BlockId(1),
                Context {
                    local_cursur: 0,
                    local_allocated: false,
                    temp_used: true,
                    temp_cursur: 0,
                    temp_invalidated: false,
                    resources: ResourceMap::from([(Identifier("gas".to_string()), 4)])
                },
                Context {
                    local_cursur: 0,
                    local_allocated: false,
                    temp_used: true,
                    temp_cursur: 0,
                    temp_invalidated: false,
                    resources: ResourceMap::from([(Identifier("gas".to_string()), 3)])
                }
            ))
        );
    }
}
