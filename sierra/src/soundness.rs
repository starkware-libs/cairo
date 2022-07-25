use crate::{
    context::{Context, Effects},
    edit_state::{put_results, take_args},
    error::Error,
    extensions::{Error as ExtError, ExtensionEffects, Registry, VarInfo},
    graph::*,
    ref_value::{mem_reducer, MemLocation, RefValue},
};
use std::collections::HashMap;
use Result::*;

pub fn validate(prog: &Program) -> Result<(), Error> {
    let h = Helper {
        prog: prog,
        reg: Registry::new(prog),
    };
    let mut visited = vec![false; prog.blocks.len()];
    let mut block_basic_infos = vec![None; prog.blocks.len()];
    for f in &prog.funcs {
        h.calc_basic_info(
            f.entry,
            h.func_start_vars(f)?,
            Context {
                local_cursur: 0,
                temp_cursur: 0,
            },
            &f.res_types,
            &mut visited,
            &mut block_basic_infos,
        )?;
    }
    let block_future_effects = h.calc_block_future_effects(&block_basic_infos)?;
    h.validate(block_basic_infos, block_future_effects)
}

type VarStates = HashMap<Identifier, VarInfo>;

#[derive(Debug, Clone)]
enum NextEffects {
    Continue(Vec<(BlockId, Effects)>),
    Return(Effects),
}

#[derive(Debug, Clone)]
struct BlockBasicInfo<'a> {
    start_vars: VarStates,
    start_ctxt: Context,
    res_types: &'a Vec<Type>,
    next_effects: NextEffects,
}

struct Helper<'a> {
    pub prog: &'a Program,
    pub reg: Registry,
}

enum FollowingInfo {
    Blocks(Vec<(BlockId, VarStates, Context, Effects)>),
    Return(Effects),
}

impl Helper<'_> {
    fn func_start_vars<'a>(self: &Self, f: &'a Function) -> Result<VarStates, Error> {
        let mut last = -2;
        let mut vars = VarStates::new();
        f.args.iter().try_for_each(|v| {
            let ti = self
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
        Ok(vars)
    }

    fn get_following_info<'a>(
        self: &Self,
        block_id: BlockId,
        mut vars: VarStates,
        mut ctxt: Context,
        res_types: &'a Vec<Type>,
    ) -> Result<FollowingInfo, Error> {
        let block = &self.prog.blocks[block_id.0];
        let mut block_effects = Effects::none();
        for invc in &block.invocations {
            let (nvars, args_info) =
                take_args(vars, invc.args.iter()).map_err(|e| Error::EditState(block_id, e))?;
            let (mut effects, fallthrough) = self
                .reg
                .transform(&invc.ext, args_info, &ctxt)
                .map_err(|e| Error::Extension(e, invc.to_string()))?;
            if effects.len() != 1 {
                return Err(Error::ExtensionBranchesMismatch(invc.to_string()));
            }
            match fallthrough {
                Some(0) => {}
                _ => {
                    return Err(Error::ExtensionFallthroughMismatch(invc.to_string()));
                }
            }
            let ExtensionEffects {
                vars: results_info,
                effects,
            } = effects.remove(0);
            if results_info.len() != invc.results.len() {
                return Err(Error::ExtensionResultSizeMismatch(invc.to_string()));
            }
            block_effects = block_effects
                .add(&effects)
                .map_err(|e| Error::Extension(ExtError::MergeEffects(e), invc.to_string()))?;
            ctxt = normalize_context(&nvars, &effects, ctxt)?;
            vars = put_results(nvars, izip!(invc.results.iter(), results_info.into_iter()))
                .map_err(|e| Error::EditState(block_id, e))?;
        }

        match &block.exit {
            BlockExit::Return(ref_ids) => {
                let (vars, used_vars) =
                    take_args(vars, ref_ids.iter()).map_err(|e| Error::EditState(block_id, e))?;
                let mut res_mem: Option<(MemLocation, usize)> = None;
                for (id, v, ty) in izip!(ref_ids.iter(), used_vars.into_iter(), res_types.iter()) {
                    if v.ty != *ty {
                        return Err(Error::FunctionReturnTypeMismatch(block_id, id.clone()));
                    }
                    let ti = self
                        .reg
                        .get_type_info(&v.ty)
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
                } else {
                    Ok(FollowingInfo::Return(block_effects))
                }
            }
            BlockExit::Jump(j) => {
                let (vars, args_info) =
                    take_args(vars, j.args.iter()).map_err(|e| Error::EditState(block_id, e))?;
                let (states, fallthrough) = self
                    .reg
                    .transform(&j.ext, args_info, &ctxt)
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
                    ExtensionEffects {
                        vars: results_info,
                        effects,
                    },
                ) in izip!(j.branches.iter(), states.into_iter())
                {
                    if results_info.len() != branch.exports.len() {
                        return Err(Error::ExtensionResultSizeMismatch(j.to_string()));
                    }
                    let (vars, ctxt) = normalize_for_block_start(
                        put_results(
                            vars.clone(),
                            izip!(branch.exports.iter(), results_info.into_iter(),),
                        )
                        .map_err(|e| Error::EditState(block_id, e))?,
                        normalize_context(&vars, &effects, ctxt.clone())?,
                    );
                    next_states.push((
                        match branch.target {
                            BranchTarget::Fallthrough => BlockId(block_id.0 + 1),
                            BranchTarget::Block(b) => b,
                        },
                        vars,
                        ctxt,
                        block_effects.add(&effects).map_err(|e| {
                            Error::Extension(ExtError::MergeEffects(e), j.to_string())
                        })?,
                    ))
                }
                Ok(FollowingInfo::Blocks(next_states))
            }
        }
    }

    fn calc_basic_info<'a>(
        self: &Self,
        block_id: BlockId,
        vars: VarStates,
        ctxt: Context,
        res_types: &'a Vec<Type>,
        visited: &mut Vec<bool>,
        results: &mut Vec<Option<BlockBasicInfo<'a>>>,
    ) -> Result<(), Error> {
        if block_id.0 >= results.len() {
            return Err(Error::FunctionBlockOutOfBounds);
        }
        if visited[block_id.0] {
            return Ok(());
        }
        visited[block_id.0] = true;
        results[block_id.0] = Some(BlockBasicInfo {
            start_vars: vars.clone(),
            start_ctxt: ctxt.clone(),
            res_types: res_types,
            next_effects: match self.get_following_info(block_id, vars, ctxt, res_types)? {
                FollowingInfo::Return(effects) => Ok(NextEffects::Return(effects)),
                FollowingInfo::Blocks(nexts) => {
                    let mut options = vec![];
                    for (next_id, nvars, nctxt, effects) in nexts {
                        self.calc_basic_info(next_id, nvars, nctxt, res_types, visited, results)?;
                        options.push((next_id.clone(), effects));
                    }
                    Ok(NextEffects::Continue(options))
                }
            }?,
        });
        Ok(())
    }

    fn reverse_topological_ordering(self: &Self) -> Vec<BlockId> {
        let mut visited = vec![false; self.prog.blocks.len()];
        let mut rev_graph = vec![vec![]; self.prog.blocks.len()];
        let mut ret_blocks = vec![];
        for b in 0..self.prog.blocks.len() {
            self.rec_build_rev_graph(b, &mut visited, &mut rev_graph, &mut ret_blocks);
        }
        let mut visited = vec![false; self.prog.blocks.len()];
        let mut order = vec![];
        for b in ret_blocks {
            self.rec_reverse_preorder_travesal(b, &rev_graph, &mut visited, &mut order);
        }
        order
    }

    fn rec_build_rev_graph(
        self: &Self,
        b: usize,
        visited: &mut Vec<bool>,
        rev_graph: &mut Vec<Vec<BlockId>>,
        ret_blocks: &mut Vec<BlockId>,
    ) {
        if visited[b] {
            return;
        }
        visited[b] = true;
        match &self.prog.blocks[b].exit {
            BlockExit::Return(_) => {
                ret_blocks.push(BlockId(b));
            }
            BlockExit::Jump(j) => {
                for br in &j.branches {
                    match &br.target {
                        BranchTarget::Fallthrough => {
                            rev_graph[b + 1].push(BlockId(b));
                            self.rec_build_rev_graph(b + 1, visited, rev_graph, ret_blocks);
                        }
                        BranchTarget::Block(n) => {
                            rev_graph[n.0].push(BlockId(b));
                            self.rec_build_rev_graph(n.0, visited, rev_graph, ret_blocks);
                        }
                    }
                }
            }
        }
    }

    fn rec_reverse_preorder_travesal(
        self: &Self,
        b: BlockId,
        rev_graph: &Vec<Vec<BlockId>>,
        visited: &mut Vec<bool>,
        order: &mut Vec<BlockId>,
    ) {
        if visited[b.0] {
            return;
        }
        visited[b.0] = true;
        order.push(b);
        for prev in &rev_graph[b.0] {
            self.rec_reverse_preorder_travesal(*prev, rev_graph, visited, order);
        }
    }

    // Function can only be called in reverse topological order - since some next stage must exist.
    fn calc_block_future_effects_for_blocks<'a>(
        self: &Self,
        bis: &Vec<Option<BlockBasicInfo<'a>>>,
        ordered_blocks: &Vec<BlockId>,
        all_effects: &mut Vec<Option<Effects>>,
    ) -> Result<(), Error> {
        for b in ordered_blocks {
            if b.0 >= bis.len() {
                return Err(Error::FunctionBlockOutOfBounds);
            }
            let bi = bis[b.0]
                .as_ref()
                .ok_or_else(|| Error::UnusedBlock(b.clone()))?;
            let effects = match &bi.next_effects {
                NextEffects::Return(block_effects) => block_effects.clone(),
                NextEffects::Continue(nexts) => {
                    let mut merged_effects: Option<Effects> = None;
                    for (n_id, block_effects) in nexts {
                        match &all_effects[n_id.0] {
                            None => {}
                            Some(future_effects) => {
                                let effects = block_effects
                                    .add(future_effects)
                                    .map_err(|e| Error::MergeEffects(*b, e))?;
                                merged_effects = Some(match merged_effects {
                                    None => effects,
                                    Some(prev) => prev
                                        .converge(&effects)
                                        .map_err(|e| Error::MergeEffects(*b, e))?,
                                })
                            }
                        }
                    }
                    merged_effects.unwrap()
                }
            };
            all_effects[b.0] = Some(effects);
        }
        Ok(())
    }

    fn calc_block_future_effects<'a>(
        self: &Self,
        bis: &Vec<Option<BlockBasicInfo<'a>>>,
    ) -> Result<Vec<Option<Effects>>, Error> {
        let mut all_effects = vec![None; self.prog.blocks.len()];
        let ordering = self.reverse_topological_ordering();
        // First iteration - making sure all blocks has some calculation.
        self.calc_block_future_effects_for_blocks(bis, &ordering, &mut all_effects)?;
        // Second iteration - now actually calculating the correct values for cycles.
        self.calc_block_future_effects_for_blocks(bis, &ordering, &mut all_effects)?;
        Ok(all_effects)
    }

    fn validate<'a>(
        self: &Self,
        bis: Vec<Option<BlockBasicInfo<'a>>>,
        all_effects: Vec<Option<Effects>>,
    ) -> Result<(), Error> {
        for (b, bi, future_effect) in izip!((0..).map(|b| BlockId(b)), &bis, &all_effects) {
            let bi = bi.as_ref().ok_or_else(|| Error::UnusedBlock(b.clone()))?;
            let future_effect = future_effect
                .as_ref()
                .ok_or_else(|| Error::UnusedBlock(b.clone()))?;
            let found_effect = match self.get_following_info(
                b,
                bi.start_vars.clone(),
                bi.start_ctxt.clone(),
                bi.res_types,
            )? {
                FollowingInfo::Return(effects) => Ok(effects),
                FollowingInfo::Blocks(nexts) => {
                    let mut merged_effects: Option<Effects> = None;
                    for (n_id, nvars, nctxt, effects) in nexts {
                        let nbi = bis[n_id.0]
                            .as_ref()
                            .ok_or_else(|| Error::UnusedBlock(b.clone()))?;
                        validate_vars_eq(n_id, &nbi.start_vars, &nvars)?;
                        validate_ctxt_eq(n_id, &nbi.start_ctxt, &nctxt)?;
                        let nfuture_effect = all_effects[n_id.0]
                            .as_ref()
                            .ok_or_else(|| Error::UnusedBlock(b.clone()))?;
                        let effects = effects
                            .add(nfuture_effect)
                            .map_err(|e| Error::MergeEffects(b, e))?;
                        merged_effects = Some(match merged_effects {
                            None => effects,
                            Some(prev) => prev
                                .converge(&effects)
                                .map_err(|e| Error::MergeEffects(b, e))?,
                        });
                    }
                    Ok(merged_effects.unwrap())
                }
            }?;
            if found_effect != *future_effect {
                return Err(Error::FunctionBlockEffectsMismatch(
                    b,
                    found_effect,
                    future_effect.clone(),
                ));
            }
        }
        for f in &self.prog.funcs {
            let bi = bis[f.entry.0]
                .as_ref()
                .ok_or_else(|| Error::UnusedBlock(f.entry.clone()))?;
            validate_vars_eq(f.entry, &self.func_start_vars(f)?, &bi.start_vars)?;
            validate_ctxt_eq(
                f.entry,
                &Context {
                    local_cursur: 0,
                    temp_cursur: 0,
                },
                &bi.start_ctxt,
            )?;
            if f.res_types != *bi.res_types {
                return Err(Error::FunctionBlockReturnTypesMismatch(
                    f.entry,
                    f.res_types.clone(),
                    bi.res_types.clone(),
                ));
            }
            let future_effects = &all_effects[f.entry.0]
                .as_ref()
                .ok_or_else(|| Error::UnusedBlock(f.entry.clone()))?;
            let with_func = future_effects
                .add(&Effects::resource_usage(Identifier("gas".to_string()), 2))
                .unwrap()
                .add(&&Effects::ap_change(2))
                .unwrap();
            if f.side_effects.ap_change.is_some() && with_func.ap_change != f.side_effects.ap_change
            {
                return Err(Error::FunctionReturnApChangeMismatch(
                    f.name.clone(),
                    with_func.ap_change,
                ));
            }
            let expected: HashMap<Identifier, i64> = f
                .side_effects
                .resource_usages
                .iter()
                .map(|(id, v)| (id.clone(), *v as i64))
                .collect();
            for id in chain!(expected.keys(), with_func.resource_usages.keys()) {
                if expected.get(id).unwrap_or(&0) != with_func.resource_usages.get(id).unwrap_or(&0)
                {
                    return Err(Error::FunctionReturnResourceUsageMismatch(
                        f.name.clone(),
                        id.clone(),
                        *with_func.resource_usages.get(id).unwrap_or(&0),
                    ));
                }
            }
        }
        Ok(())
    }
}

fn validate_ctxt_eq(block: BlockId, ctxt1: &Context, ctxt2: &Context) -> Result<(), Error> {
    if ctxt1 == ctxt2 {
        Ok(())
    } else {
        Err(Error::FunctionBlockContextMismatch(
            block,
            ctxt1.clone(),
            ctxt2.clone(),
        ))
    }
}

fn validate_vars_eq(block: BlockId, vars1: &VarStates, vars2: &VarStates) -> Result<(), Error> {
    if vars1.len() != vars2.len() {
        Err(Error::FunctionBlockIdentifiersMismatch(
            block,
            vars1.keys().map(|x| x.clone()).collect(),
            vars2.keys().map(|x| x.clone()).collect(),
        ))
    } else {
        vars1.iter().try_for_each(
            |(
                id,
                VarInfo {
                    ty: ty1,
                    ref_val: ref_val1,
                },
            )| match vars2.get(id) {
                None => Err(Error::FunctionBlockIdentifiersMismatch(
                    block,
                    vars1.keys().map(|x| x.clone()).collect(),
                    vars2.keys().map(|x| x.clone()).collect(),
                )),
                Some(VarInfo {
                    ty: ty2,
                    ref_val: ref_val2,
                }) => {
                    if ty1 != ty2 {
                        Err(Error::FunctionBlockIdentifierTypeMismatch(
                            block,
                            id.clone(),
                            ty1.clone(),
                            ty2.clone(),
                        ))
                    } else if ref_val1 != ref_val2 {
                        Err(Error::FunctionBlockIdentifierLocationMismatch(
                            block,
                            id.clone(),
                            ref_val1.clone(),
                            ref_val2.clone(),
                        ))
                    } else {
                        Ok(())
                    }
                }
            },
        )
    }
}

fn normalize_context(
    vars: &VarStates,
    effects: &Effects,
    mut ctxt: Context,
) -> Result<Context, Error> {
    ctxt.local_cursur += effects.local_writes;
    match effects.ap_change {
        Some(ap_change) => {
            ctxt.temp_cursur += ap_change;
        }
        None => {
            ctxt.temp_cursur = 0;
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
    }
    Ok(ctxt)
}

fn normalize_for_block_start(mut vars: VarStates, mut ctxt: Context) -> (VarStates, Context) {
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
    (vars, ctxt)
}

#[cfg(test)]
mod function {
    use super::*;
    use crate::{context::ResourceMap, ProgramParser};

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
            Err(Error::FunctionReturnResourceUsageMismatch(
                "Other".to_string(),
                Identifier("gas".to_string()),
                5
            ))
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
            Err(Error::MergeEffects(
                BlockId(1),
                crate::context::Error::ResourceUsageMismatch(
                    ResourceMap::from([(Identifier("gas".to_string()), 0)]),
                    ResourceMap::from([(Identifier("gas".to_string()), 1)])
                )
            ))
        );
    }
}
