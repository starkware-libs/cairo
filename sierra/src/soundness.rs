use crate::{
    error::Error,
    extensions::*,
    graph::*,
    mem_state::*,
    next_state::{put_results, take_args},
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
            let ti = registry.get_type_info(&v.ty)?;
            last -= ti.size as i64;
            vars.insert(
                v.id.clone(),
                VarState {
                    ty: v.ty.clone(),
                    loc: if ti.size > 0 {
                        Location::Final(MemLocation::Local(last))
                    } else {
                        Location::Transient
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
                mem: MemState {
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

#[derive(Debug, Clone, PartialEq)]
struct VarState {
    ty: Type,
    loc: Location,
}

type VarStates = HashMap<Identifier, VarState>;

#[derive(Debug, Clone)]
struct State {
    vars: VarStates,
    mem: MemState,
}

fn fix_mem(vars: &VarStates, mut mem: MemState) -> Result<MemState, Error> {
    if mem.temp_invalidated {
        mem.temp_invalidated = false;
        mem.temp_used = true;
        for (id, var_state) in vars.iter() {
            match &var_state.loc {
                Location::Final(MemLocation::Temp(_))
                | Location::Add(MemLocation::Temp(_), _)
                | Location::Add(MemLocation::Local(_), MemLocation::Temp(_))
                | Location::AddConst(MemLocation::Temp(_), _) => {
                    return Err(Error::UsedTempMemoryInvalidated(id.clone()));
                }
                _ => {}
            }
        }
    }
    Ok(mem)
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
        let fix = |offset: &mut i64| {
            *offset -= start_state.mem.temp_cursur as i64;
        };
        for (_, v) in start_state.vars.iter_mut() {
            match &mut v.loc {
                Location::Final(MemLocation::Temp(offset)) => fix(offset),
                Location::Add(MemLocation::Temp(offset1), MemLocation::Temp(offset2)) => {
                    fix(offset1);
                    fix(offset2);
                }
                Location::Add(MemLocation::Local(_), MemLocation::Temp(offset)) => fix(offset),
                Location::Add(MemLocation::Temp(offset), MemLocation::Local(_)) => fix(offset),
                Location::AddConst(MemLocation::Temp(offset), _) => fix(offset),
                _ => {}
            }
        }
        start_state.mem.temp_cursur = 0;
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
                            } else if var_state.loc != expected_var_state.loc {
                                Err(Error::FunctionBlockIdentifierLocationMismatch(
                                    block,
                                    id.clone(),
                                    expected_var_state.loc.clone(),
                                    var_state.loc.clone(),
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
        let State { mut vars, mut mem } = start_state;
        for invc in &self.blocks[block.0].invocations {
            let (nvars, used_vars) = take_args(vars, invc.args.iter())?;
            let (arg_types, arg_locs): (Vec<_>, Vec<_>) =
                used_vars.into_iter().map(|v| (v.ty, v.loc)).unzip();
            let (sign, locs) = self.registry.get_mapping(&invc.ext, mem, arg_locs)?;
            if sign.args != arg_types {
                return Err(Error::ExtensionArgumentsMismatch(invc.to_string()));
            }
            if sign.results.len() != 1 || locs.len() != 1 {
                return Err(Error::ExtensionBranchesMismatch(invc.to_string()));
            }
            match sign.fallthrough {
                Some(0) => {}
                _ => {
                    return Err(Error::ExtensionFallthroughMismatch(invc.to_string()));
                }
            }
            let (nmem, locs) = &locs[0];
            if sign.results[0].len() != invc.results.len() || locs.len() != invc.results.len() {
                return Err(Error::ExtensionResultSizeMismatch(invc.to_string()));
            }
            mem = fix_mem(&nvars, nmem.clone())?;
            vars = put_results(
                nvars,
                izip!(invc.results.iter(), sign.results[0].iter(), locs.iter()).map(
                    |(id, ty, loc)| {
                        (
                            id,
                            VarState {
                                ty: ty.clone(),
                                loc: loc.clone(),
                            },
                        )
                    },
                ),
            )?;
        }

        match &self.blocks[block.0].exit {
            BlockExit::Return(ref_ids) => {
                let (vars, used_vars) = take_args(vars, ref_ids.iter())?;
                let mut mem_end = None;
                for (id, v, ty) in izip!(ref_ids.iter(), used_vars.iter(), self.res_types.iter()) {
                    if v.ty != *ty {
                        return Err(Error::FunctionReturnTypeMismatch(block, id.clone()));
                    }
                    let ti = self.registry.get_type_info(ty)?;
                    if ti.size == 0 {
                        continue;
                    }
                    match v.loc {
                        Location::Final(MemLocation::Temp(offset)) => {
                            match mem_end {
                                Some(prev) if prev != offset => {
                                    return Err(Error::FunctionReturnLocationMismatch(
                                        block,
                                        id.clone(),
                                    ));
                                }
                                _ => {}
                            }
                            mem_end = Some(offset + ti.size as i64);
                        }
                        _ => {
                            return Err(Error::FunctionReturnLocationMismatch(block, id.clone()));
                        }
                    }
                }
                match mem_end {
                    Some(mem_end) if mem_end != mem.temp_cursur as i64 => {
                        return Err(Error::FunctionReturnLocationNotEndOfTemp(
                            block,
                            mem_end,
                            mem.temp_cursur,
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
                let (vars, used_vars) = take_args(vars, j.args.iter())?;
                let (arg_types, arg_locs): (Vec<_>, Vec<_>) =
                    used_vars.into_iter().map(|v| (v.ty, v.loc)).unzip();
                let (sign, locs) = self.registry.get_mapping(&j.ext, mem, arg_locs)?;
                if sign.args != arg_types {
                    return Err(Error::ExtensionArgumentsMismatch(j.to_string()));
                }
                if sign.results.len() != j.branches.len() || locs.len() != j.branches.len() {
                    return Err(Error::ExtensionBranchesMismatch(j.to_string()));
                }
                match sign.fallthrough {
                    None => {}
                    Some(i) => {
                        if j.branches[i].target != BranchTarget::Fallthrough {
                            return Err(Error::ExtensionFallthroughMismatch(j.to_string()));
                        }
                    }
                }
                izip!(
                    j.branches.iter(),
                    sign.results.into_iter(),
                    locs.into_iter()
                )
                .try_for_each(|(branch, res_types, (mem, locs))| {
                    if branch.exports.len() != res_types.len() || locs.len() != res_types.len() {
                        return Err(Error::ExtensionResultSizeMismatch(j.to_string()));
                    }
                    let mem = fix_mem(&vars, mem.clone())?;
                    self.validate(
                        match branch.target {
                            BranchTarget::Fallthrough => BlockId(block.0 + 1),
                            BranchTarget::Block(b) => b,
                        },
                        State {
                            vars: put_results(
                                vars.clone(),
                                izip!(
                                    branch.exports.iter(),
                                    res_types.into_iter(),
                                    locs.into_iter()
                                )
                                .map(|(id, ty, loc)| (id, VarState { ty: ty, loc: loc })),
                            )?,
                            mem: mem,
                        },
                        block_start_states,
                    )
                })
            }
        }
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
                duplicate_num<int>(n) -> (n, n_dup);
                split_gas<9, 1>(cost) -> (cost, jump_cost);
                jump_nz<int>(n_dup, jump_cost) { 2() fallthrough() };
                # 1
                ignore_num<int>(n) -> ();
                split_gas<7, 1, 1>(cost) -> (cost, push_gb, push_one);
                refund_gas<7>(gb, cost) -> (gb);
                store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
                constant_num<int, 1>() -> (one);
                store<Temp, int>(one, push_one) -> (one);
                return(gb, one);
                # 2
                split_gas<7, 1, 1>(cost) -> (cost, push_n, jump_cost);
                add<int, -1>(n) -> (n);
                store<Temp, int>(n, push_n) -> (n);
                duplicate_num<int>(n) -> (n, n_dup);
                jump_nz<int>(n_dup, jump_cost) { 4() fallthrough() };
                # 3
                ignore_num<int>(n) -> ();
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
                move<int>(n) -> (n);
                store<Temp, int>(n, push_n) -> (n);
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
                add<int, -1>(n) -> (n);
                store<Temp, int>(n, push_n) -> (n);
                store<Temp, GasBuiltin>(gb, push_gb) -> (gb);
                store<Temp, int>(a, push_a) -> (a);
                duplicate_num<int>(n) -> (n, use);
                jump_nz<int>(use, jump_cost) { 5() fallthrough() };
                # 8
                ignore_num<int>(b) -> ();
                ignore_num<int>(n) -> ();
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
                duplicate_num<int>(n) -> (n, use);
                split_gas<5, 1>(cost) -> (cost, use_cost);
                jump_nz<int>(use, use_cost) { 2() fallthrough() };
                # 1
                ignore_num<int>(n) -> ();
                split_gas<3, 1, 1>(cost) -> (cost, push_res1, push_res2);
                refund_gas<3>(gb, cost) -> (gb);
                store<Temp, GasBuiltin>(gb, push_res1) -> (gb);
                move<int>(one) -> (one);
                store<Temp, int>(one, push_res2) -> (one);
                return(gb, one);
                # 2
                add<int, -1>(n) -> (n_1);
                split_gas<4, 1>(cost) -> (cost, use_cost);
                store<Temp, int>(n_1, use_cost) -> (n_1);
                duplicate_num<int>(n_1) -> (n_1, use);
                split_gas<3, 1>(cost) -> (cost, use_cost);
                jump_nz<int>(use, use_cost) { 4() fallthrough() };
                # 3
                ignore_num<int>(n_1) -> ();
                split_gas<1, 1, 1>(cost) -> (cost, push_res1, push_res2);
                refund_gas<1>(gb, cost) -> (gb);
                store<Temp, GasBuiltin>(gb, push_res1) -> (gb);
                move<int>(one) -> (one);
                store<Temp, int>(one, push_res2) -> (one);
                return(gb, one);
                # 4
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
}
