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
    let registry = Registry::new(prog);
    prog.funcs.iter().try_for_each(|f| {
        let mut last = -2;
        let mut vars = VarStates::new();
        f.args.iter().try_for_each(|v| {
            let ti = registry
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
        Helper {
            registry: &registry,
            blocks: &prog.blocks,
            res_types: &f.res_types,
        }
        .validate(
            f.entry,
            State {
                vars: vars,
                ctxt: Context {
                    local_cursur: 0,
                    local_allocated: false,
                    temp_used: false,
                    temp_cursur: 0,
                    temp_invalidated: false,
                },
            },
            &mut block_start_states,
        )
    })
}

type VarStates = HashMap<Identifier, VarInfo>;

#[derive(Debug, Clone)]
struct State {
    vars: VarStates,
    ctxt: Context,
}

struct Helper<'a> {
    pub registry: &'a Registry,
    pub blocks: &'a Vec<Block>,
    pub res_types: &'a Vec<Type>,
}

impl Helper<'_> {
    fn validate(
        self: &Self,
        block: BlockId,
        mut start_state: State,
        block_start_states: &mut Vec<Option<State>>,
    ) -> Result<(), Error> {
        if block.0 >= block_start_states.len() {
            return Err(Error::FunctionBlockOutOfBounds);
        }
        start_state = handle_block_start(start_state);
        match &block_start_states[block.0] {
            None => {
                block_start_states[block.0] = Some(start_state.clone());
            }
            Some(s) => {
                if s.vars.len() != start_state.vars.len() {
                    return Err(Error::FunctionBlockIdentifiersMismatch(
                        block,
                        s.vars.keys().map(|x| x.clone()).collect(),
                        start_state.vars.keys().map(|x| x.clone()).collect(),
                    ));
                }
                for (id, expected_var_state) in &s.vars {
                    match start_state.vars.get(id) {
                        None => Err(Error::FunctionBlockIdentifiersMismatch(
                            block,
                            s.vars.keys().map(|x| x.clone()).collect(),
                            start_state.vars.keys().map(|x| x.clone()).collect(),
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
                    }?;
                }
                return Ok(());
            }
        }
        let State { mut vars, mut ctxt } = start_state;
        for invc in &self.blocks[block.0].invocations {
            let (nvars, args_info) = take_args(vars, invc.args.iter())?;
            let (mut states, fallthrough) = self
                .registry
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
            ctxt = handle_temp_invalidation(&nvars, nctxt.clone())?;
            vars = put_results(nvars, izip!(invc.results.iter(), results_info.into_iter()))?;
        }

        match &self.blocks[block.0].exit {
            BlockExit::Return(ref_ids) => {
                let (vars, used_vars) = take_args(vars, ref_ids.iter())?;
                let mut res_mem: Option<(MemLocation, usize)> = None;
                for (id, v, ty) in izip!(ref_ids.iter(), used_vars.iter(), self.res_types.iter()) {
                    if v.ty != *ty {
                        return Err(Error::FunctionReturnTypeMismatch(block, id.clone()));
                    }
                    let ti = self
                        .registry
                        .get_type_info(ty)
                        .map_err(|e| Error::TypeInfo(e, ty.clone()))?;
                    if ti.size == 0 {
                        continue;
                    }
                    let loc = match v.ref_val {
                        RefValue::Final(MemLocation::Temp(offset)) => Ok(MemLocation::Temp(offset)),
                        _ => Err(Error::FunctionReturnLocationMismatch(block, id.clone())),
                    }?;
                    res_mem = Some(match res_mem {
                        None => Ok((loc, ti.size)),
                        Some(prev) => mem_reducer(prev, (loc, ti.size)).ok_or_else(|| {
                            Error::FunctionReturnLocationMismatch(block, id.clone())
                        }),
                    }?);
                }
                match res_mem {
                    Some((MemLocation::Temp(base), size))
                        if base + size as i64 != ctxt.temp_cursur as i64 =>
                    {
                        return Err(Error::FunctionReturnLocationNotEndOfTemp(
                            block,
                            base + size as i64,
                            ctxt.temp_cursur,
                        ));
                    }
                    _ => {}
                }
                if vars.is_empty() {
                    Ok(())
                } else {
                    return Err(Error::FunctionRemainingOwnedObjects(
                        vars.into_keys().collect(),
                    ));
                }
            }
            BlockExit::Jump(j) => {
                let (vars, args_info) = take_args(vars, j.args.iter())?;
                let (states, fallthrough) = self
                    .registry
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
                    let ctxt = handle_temp_invalidation(&vars, ctxt)?;
                    self.validate(
                        match branch.target {
                            BranchTarget::Fallthrough => BlockId(block.0 + 1),
                            BranchTarget::Block(b) => b,
                        },
                        State {
                            vars: put_results(
                                vars.clone(),
                                izip!(branch.exports.iter(), results_info.into_iter(),),
                            )?,
                            ctxt: ctxt,
                        },
                        block_start_states,
                    )?;
                }
                Ok(())
            }
        }
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

fn handle_block_start(mut state: State) -> State {
    if state.ctxt.temp_cursur == 0 {
        return state;
    }
    let fix = |offset: &mut i64| {
        *offset -= state.ctxt.temp_cursur as i64;
    };
    for (_, v) in state.vars.iter_mut() {
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
    state.ctxt.temp_cursur = 0;
    state
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
                &pp.parse("Some@0(gb: GasBuiltin, a: felt) -> (GasBuiltin, felt);")
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

                Other@0(a: int, b: int, c: int, d: int, cost: Gas<3>) -> (int);"#).unwrap()),
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

                Other@3(gb: GasBuiltin, cost: Gas<3>) -> (GasBuiltin);"#
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
                
                Some@3(gb: GasBuiltin, cost: Gas<3>) -> (GasBuiltin);"#
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

                Other@3(gb: GasBuiltin, cost: Gas<3>) -> (GasBuiltin);"#
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

    #[test]
    fn fibonacci_using_jump() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                # 0
                split_gas<9, 1>(cost) -> (cost, jump_cost);
                jump_nz<int>(n, jump_cost) { 2(n) fallthrough() };
                # 1
                split_gas<7, 1, 1>(cost) -> (cost, push_gb, push_one);
                refund_gas<7>(gb, cost) -> (gb);
                store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
                constant_num<int, 1>() -> (one);
                store<Temp, int>(one, push_one) -> (one);
                return(gb, one);
                # 2
                split_gas<7, 1, 1>(cost) -> (cost, push_n, jump_cost);
                unwrap_nz<int>(n) -> (n);
                add<int, -1>(n) -> (n);
                store<Temp, int>(n, push_n) -> (n);
                jump_nz<int>(n, jump_cost) { 4(n) fallthrough() };
                # 3
                split_gas<5, 1, 1>(cost) -> (cost, push_gb, push_one);
                refund_gas<5>(gb, cost) -> (gb);
                store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
                constant_num<int, 1>() -> (one);
                store<Temp, int>(one, push_one) -> (one);
                return(gb, one);
                # 4
                split_gas<1, 1, 1, 1, 1, 2>(cost) -> (
                    push_b, push_n, push_gb, push_a, get_gas_cost, final_cost
                );
                constant_num<int, 1>() -> (b);
                store<Temp, int>(b, push_b) -> (b);
                move<NonZero<int>>(n) -> (n);
                store<Temp, NonZero<int>>(n, push_n) -> (n);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
                constant_num<int, 1>() -> (a);
                store<Temp, int>(a, push_a) { fallthrough(a) };
                # 5
                get_gas<1, 1, 1, 1, 1>(gb, get_gas_cost) {
                    7(gb, push_n, push_gb, push_a, jump_cost, get_gas_cost)
                    fallthrough(gb)
                };
                # 6
                ignore_num<int>(a) -> ();
                ignore_num<int>(b) -> ();
                unwrap_nz<int>(n) -> (n);
                ignore_num<int>(n) -> ();
                split_gas<1, 1>(final_cost) -> (push_gb, push_err);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
                constant_num<int, -1>() -> (err);
                store<Temp, int>(err, push_err) -> (err);
                return(gb, err);
                # 7
                duplicate_num<int>(a) -> (a, prev_a);
                add<int>(a, b) -> (a);
                rename<int>(prev_a) -> (b);
                unwrap_nz<int>(n) -> (n);
                add<int, -1>(n) -> (n);
                store<Temp, int>(n, push_n) -> (n);
                store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
                store<Temp, int>(a, push_a) -> (a);
                jump_nz<int>(n, jump_cost) { 5(n) fallthrough() };
                # 8
                ignore_num<int>(b) -> ();
                refund_gas<1>(gb, get_gas_cost) -> (gb);
                split_gas<1, 1>(final_cost) -> (push_gb, push_a);
                store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
                move<int>(a) -> (a);
                store<Temp, int>(a, push_a) -> (a);
                return(gb, a);

                Fibonacci@0(gb: GasBuiltin, n: int, cost: Gas<10>) -> (GasBuiltin, int);"#
                )
                .unwrap()
            ),
            Ok(())
        );
    }

    #[test]
    fn fibonacci_using_recursion() {
        let pp = ProgramParser::new();
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                # 0
                split_gas<7, 1>(cost) -> (cost, use_cost);
                alloc_locals(use_cost) -> ();
                constant_num<int, 1>() -> (one);
                split_gas<6, 1>(cost) -> (cost, use_cost);
                store<Temp, int>(one, use_cost) -> (one);
                split_gas<5, 1>(cost) -> (cost, use_cost);
                jump_nz<int>(n, use_cost) { 2(n) fallthrough() };
                # 1
                split_gas<3, 1, 1>(cost) -> (cost, push_res1, push_res2);
                refund_gas<3>(gb, cost) -> (gb);
                store<Temp, GasBuiltin>(gb, push_res1) -> (gb);
                move<int>(one) -> (one);
                store<Temp, int>(one, push_res2) -> (one);
                return(gb, one);
                # 2
                unwrap_nz<int>(n) -> (n);
                add<int, -1>(n) -> (n_1);
                split_gas<4, 1>(cost) -> (cost, use_cost);
                store<Temp, int>(n_1, use_cost) -> (n_1);
                split_gas<3, 1>(cost) -> (cost, use_cost);
                jump_nz<int>(n_1, use_cost) { 4(n_1) fallthrough() };
                # 3
                split_gas<1, 1, 1>(cost) -> (cost, push_res1, push_res2);
                refund_gas<1>(gb, cost) -> (gb);
                store<Temp, GasBuiltin>(gb, push_res1) -> (gb);
                move<int>(one) -> (one);
                store<Temp, int>(one, push_res2) -> (one);
                return(gb, one);
                # 4
                unwrap_nz<int>(n_1) -> (n_1);
                ignore_num<int>(one) -> ();
                split_gas<1, 1, 1>(cost) -> (get_gas_cost, use_cost, store_gb);
                get_gas<1, 8, 2, 8 ,2, 1, 1, 1, 1, 1>(gb, get_gas_cost) {
                    6(gb, dec_cost, call1_inner_cost, call1_outer_cost,
                      call2_inner_cost, call2_outer_cost, move_to_local_cost,
                      push_arg1, push_arg2, push_arg3, push_arg4)
                    fallthrough(gb)
                };
                # 5
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb, store_gb) -> (gb);
                ignore_num<int>(n_1) -> ();
                constant_num<int, -10000>() -> (minus);
                store<Temp, int>(minus, use_cost) -> (minus);
                return(gb, minus);
                # 6
                store<Temp, GasBuiltin>(gb, store_gb) -> (gb);
                duplicate_num<int>(n_1) -> (n_1, n_2);
                add<int, -1>(n_2) -> (n_2);
                store<Local, int>(n_2, dec_cost) -> (n_2);
                move<int>(n_1) -> (n_1);
                store<Temp, int>(n_1, push_arg1) -> (n_1);
                tuple_pack<GasBuiltin, int, Gas<8>>(gb, n_1, call1_inner_cost) -> (input);
                Fibonacci(input, call1_outer_cost) -> (output);
                tuple_unpack<GasBuiltin, int>(output) -> (gb, r1);
                move<int>(r1) -> (r1);
                store<Local, int>(r1, move_to_local_cost) -> (r1);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb, push_arg2) -> (gb);
                move<int>(n_2) -> (n_2);
                store<Temp, int>(n_2, push_arg3) -> (n_2);
                tuple_pack<GasBuiltin, int, Gas<8>>(gb, n_2, call2_inner_cost) -> (input);
                Fibonacci(input, call2_outer_cost) -> (output);
                tuple_unpack<GasBuiltin, int>(output) -> (gb, r2);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb, push_arg4) -> (gb);
                add<int>(r1, r2) -> (r);
                store<Temp, int>(r, use_cost) -> (r);
                return(gb, r);

                Fibonacci@0(gb: GasBuiltin, n: int, cost: Gas<8>) -> (GasBuiltin, int);"#
                )
                .unwrap()
            ),
            Ok(())
        );
    }

    #[test]
    fn collatz() {
        let pp = ProgramParser::new();
        // count = 0;
        // while n != 1 {
        //   n = n % 2 == 0 { n / 2 } else { 3 * n + 1 };
        //   count++;
        // }
        assert_eq!(
            validate(
                &pp.parse(
                    r#"
                # 0
                split_gas<1, 1, 1, 1, 1, 1, 1, 2>(cost) -> (
                    push_n, push_gb, push_counter,
                    jump_cost0, jump_cost7, push_n_1, get_gas_cost, final_cost
                );
                move<int>(n) -> (n);
                store<Temp, int>(n, push_n) -> (n);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
                constant_num<int, 0>() -> (counter);
                store<Temp, int>(counter, push_counter) -> (counter);
                jump(jump_cost0) { 7() };
                # 1
                unwrap_nz<int>(to_drop) -> (to_drop);
                ignore_num<int>(to_drop) -> ();
                get_gas<1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1>(gb, get_gas_cost) {
                    3(gb, push_parity, jump_cost4, cost1, cost2, push_gb1, push_gb2, push_counter,
                      jump_cost7, jump_cost3, push_n_1, get_gas_cost)
                    fallthrough(gb)
                };
                # 2
                ignore_num<int>(n) -> ();
                ignore_num<int>(counter) -> ();
                split_gas<1, 1>(final_cost) -> (push_gb, push_err);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
                constant_num<int, -1>() -> (err);
                store<Temp, int>(err, push_err) -> (err);
                return(gb, err);
                # 3
                duplicate_num<int>(n) -> (n, parity);
                mod<int, 2>(parity) -> (parity);
                store<Temp, int>(parity, push_parity) -> (parity);
                store<Temp, GasBuiltin>(gb, push_gb1) -> (gb);
                jump_nz<int>(parity, jump_cost3) { 5(to_drop) fallthrough() };
                # 4
                align_temps<1>(cost1) -> ();
                div<int, 2>(n) -> (n);
                store<Temp, int>(n, cost2) -> (n);
                move<GasBuiltin>(gb) -> (gb);
                store<Temp, GasBuiltin>(gb, push_gb2) -> (gb);
                jump(jump_cost4) { 6() };
                # 5
                unwrap_nz<int>(to_drop) -> (to_drop);
                ignore_num<int>(to_drop) -> ();
                mul<int, 3>(n) -> (n);
                store<Temp, int>(n, cost1) -> (n);
                add<int, 1>(n) -> (n);
                store<Temp, int>(n, cost2) -> (n);
                refund_gas<1>(gb, jump_cost4) -> (gb);
                store<Temp, GasBuiltin>(gb, push_gb2) { fallthrough(gb) };
                # 6
                add<int, 1>(counter) -> (counter);
                store<Temp, int>(counter, push_counter) { fallthrough(counter) };
                # 7
                duplicate_num<int>(n) -> (n, n_1);
                add<int, -1>(n_1) -> (n_1);
                store<Temp, int>(n_1, push_n_1) -> (n_1);
                jump_nz<int>(n_1, jump_cost7) { 1(to_drop) fallthrough() };
                # 8
                ignore_num<int>(n) -> ();
                refund_gas<1>(gb, get_gas_cost) -> (gb);
                split_gas<1, 1>(final_cost) -> (push_gb, push_counter);
                store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
                move<int>(counter) -> (counter);
                store<Temp, int>(counter, push_counter) -> (counter);
                return(gb, counter);

                Collatz@0(gb: GasBuiltin, n: int, cost: Gas<9>) -> (GasBuiltin, int);"#
                )
                .unwrap()
            ),
            Ok(())
        );
    }
}
