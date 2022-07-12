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
                    loc: Location::Local(last),
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
                    temp_offset: 0,
                    local_offset: 0,
                    ap_change: ApChange::Known(0),
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

struct Helper<'a> {
    pub registry: &'a Registry,
    pub blocks: &'a Vec<Block>,
    pub res_types: &'a Vec<Type>,
}

impl Helper<'_> {
    fn validate(
        self: &Self,
        block: BlockId,
        start_state: State,
        block_start_states: &mut Vec<Option<State>>,
    ) -> Result<(), Error> {
        if block.0 >= block_start_states.len() {
            return Err(Error::FunctionBlockOutOfBounds);
        }
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
            let (sign, locs) = self.registry.get_mapping(&invc.ext, mem, &arg_locs)?;
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
            mem = nmem.clone();
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
                if izip!(used_vars.iter(), self.res_types.iter()).any(|(v, ty)| v.ty != *ty) {
                    return Err(Error::FunctionReturnTypeMismatch(block));
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
                let (sign, locs) = self.registry.get_mapping(&j.ext, mem, &arg_locs)?;
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
                    let next_vars = put_results(
                        vars.clone(),
                        izip!(
                            branch.exports.iter(),
                            res_types.into_iter(),
                            locs.into_iter()
                        )
                        .map(|(id, ty, loc)| (id, VarState { ty: ty, loc: loc })),
                    )?;
                    self.validate(
                        match branch.target {
                            BranchTarget::Fallthrough => BlockId(block.0 + 1),
                            BranchTarget::Block(b) => b,
                        },
                        State {
                            vars: next_vars,
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
    use crate::{
        parser::BlocksParser,
        utils::{as_type, gas_builtin_type, gas_type, type_arg, val_arg},
    };

    fn as_id(name: &str) -> Identifier {
        Identifier(name.to_string())
    }

    fn typed(id: Identifier, ty: Type) -> TypedVar {
        TypedVar { id: id, ty: ty }
    }

    fn int_type() -> Type {
        as_type("int")
    }

    #[test]
    fn empty() {
        let bp = BlocksParser::new();
        assert_eq!(
            validate(&Program {
                blocks: bp.parse("").unwrap(),
                funcs: vec![Function {
                    name: "Some".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("a"), as_type("felt"))
                    ],
                    res_types: vec![gas_builtin_type(), as_type("felt")],
                    entry: BlockId(0),
                }]
            }),
            Err(Error::FunctionBlockOutOfBounds)
        );
    }

    #[test]
    fn basic_return() {
        let bp = BlocksParser::new();
        assert_eq!(
            validate(&Program {
                blocks:
                    bp.parse(r#"
                    split_gas<1, 2>(cost) -> (cost_for_next, cost);
                    add<int>(a, b) -> (a_plus_b_deferred);
                    enact_calc<int, 0>(a_plus_b_deferred, cost_for_next) -> (a_plus_b);
                    split_gas<1, 1>(cost) -> (cost_for_next, cost_for_last);
                    sub<int>(c, d) -> (c_minus_d_deferred);
                    enact_calc<int, 0>(c_minus_d_deferred, cost_for_next) -> (c_minus_d);
                    mul<int>(a_plus_b, c_minus_d) -> (a_plus_b_mul_c_minus_d_deferred);
                    enact_calc<int, 0>(a_plus_b_mul_c_minus_d_deferred, cost_for_last) -> (a_plus_b_mul_c_minus_d);
                    return(a_plus_b_mul_c_minus_d);
                    "#).unwrap(),
                funcs: vec![Function {
                    name: "Other".to_string(),
                    args: vec![
                        typed(as_id("a"), int_type()),
                        typed(as_id("b"), int_type()),
                        typed(as_id("c"), int_type()),
                        typed(as_id("d"), int_type()),
                        typed(as_id("cost"), gas_type(3)),
                    ],
                    res_types: vec![int_type()],
                    entry: BlockId(0),
                }]
            }),
            Ok(())
        );
    }

    #[test]
    fn inifinite_gas_take_or_return() {
        let bp = BlocksParser::new();
        assert_eq!(
            validate(&Program {
                blocks: bp
                    .parse(
                        r#"get_gas<1>(gb, cost) { 0(gb, cost) fallthrough(gb) };
                    return(gb);"#
                    )
                    .unwrap(),
                funcs: vec![Function {
                    name: "Some".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("cost"), gas_type(1)),
                    ],
                    res_types: vec![gas_builtin_type()],
                    entry: BlockId(0),
                }]
            }),
            Ok(())
        );
    }

    #[test]
    fn inifinite_gas_take_or_return_bad_fallthrough() {
        let bp = BlocksParser::new();
        assert_eq!(
            validate(&Program {
                blocks: bp
                    .parse(
                        r#"return(gb);
                             get_gas<1>(gb, cost) { 1(gb, cost) 0(gb) };"#
                    )
                    .unwrap(),
                funcs: vec![Function {
                    name: "Some".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("cost"), gas_type(1)),
                    ],
                    res_types: vec![gas_builtin_type()],
                    entry: BlockId(1),
                }]
            }),
            Err(Error::ExtensionFallthroughMismatch(
                "get_gas<1>(gb, cost) {\n1(gb, cost)\n0(gb)\n}".to_string()
            ))
        );
    }

    #[test]
    fn gas_mismatch() {
        let bp = BlocksParser::new();
        assert_eq!(
            validate(&Program {
                blocks: bp
                    .parse(
                        r#"get_gas<2>(gb, cost) { 0(gb, cost) fallthrough(gb) };
                    return(gb);"#
                    )
                    .unwrap(),
                funcs: vec![Function {
                    name: "Some".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("cost"), gas_type(1)),
                    ],
                    res_types: vec![gas_builtin_type()],
                    entry: BlockId(0),
                }]
            }),
            Err(Error::FunctionBlockIdentifierTypeMismatch(
                BlockId(0),
                as_id("cost"),
                gas_type(1),
                gas_type(2)
            ))
        );
    }

    #[test]
    fn fibonacci_using_jump() {
        let bp = BlocksParser::new();
        assert_eq!(
            validate(&Program {
                blocks: bp
                    .parse(
                        r#"
                    # 0
                    constant_num<int, 1>() -> (one);
                    split_gas<5, 1>(cost) -> (cost, use_cost);
                    enact_calc<int, 0>(one, use_cost) -> (one);
                    duplicate_num<int>(n) -> (n, use);
                    split_gas<4, 1>(cost) -> (cost, use_cost);
                    jump_nz<int>(use, use_cost) { 2() fallthrough() };
                    # 1
                    refund_gas<4>(gb, cost) -> (gb);
                    ignore_num<int>(n) -> ();
                    return(gb, one);
                    # 2
                    add<int, -1>(n) -> (n);
                    split_gas<3, 1>(cost) -> (cost, use_cost);
                    enact_calc<int, 0>(n, use_cost) -> (n);
                    duplicate_num<int>(n) -> (n, use);
                    split_gas<2, 1>(cost) -> (cost, use_cost);
                    jump_nz<int>(use, use_cost) { 4() fallthrough() };
                    # 3
                    refund_gas<2>(gb, cost) -> (gb);
                    ignore_num<int>(n) -> ();
                    return(gb, one);
                    # 4
                    duplicate_num<int>(one) { fallthrough(a, b) };
                    # 5
                    split_gas<1, 1>(cost) -> (split_gas_cost, use_cost);
                    get_gas<4>(gb, split_gas_cost) { 7(gb, cost) fallthrough(gb) };
                    # 6
                    ignore_num<int>(a) -> ();
                    ignore_num<int>(b) -> ();
                    ignore_num<int>(n) -> ();
                    constant_num<int, -1>() -> (minus);
                    enact_calc<int, 0>(minus, use_cost) -> (minus);
                    return(gb, minus);
                    # 7
                    duplicate_num<int>(a) -> (a, prev_a);
                    add<int>(a, b) -> (a);
                    duplicate_num<int>(prev_a) -> (b, tmp);
                    ignore_num<int>(tmp) -> ();
                    enact_calc<int, 0>(a, use_cost) -> (a);
                    add<int, -1>(n) -> (n);
                    split_gas<3, 1>(cost) -> (cost, use_cost);
                    enact_calc<int, 0>(n, use_cost) -> (n);
                    split_gas<2, 1>(cost) -> (cost, use_cost);
                    duplicate_num<int>(n) -> (n, use);
                    jump_nz<int>(use, use_cost) { 5() fallthrough() };
                    # 8
                    refund_gas<2>(gb, cost) -> (gb);
                    ignore_num<int>(n) -> ();
                    ignore_num<int>(b) -> ();
                    return(gb, a);"#
                    )
                    .unwrap(),
                funcs: vec![Function {
                    name: "Fibonacci".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("n"), int_type()),
                        typed(as_id("cost"), gas_type(6)),
                    ],
                    res_types: vec![gas_builtin_type(), int_type()],
                    entry: BlockId(0),
                }]
            }),
            Ok(())
        );
    }

    #[test]
    fn fibonacci_using_recursion() {
        let bp = BlocksParser::new();
        assert_eq!(
            validate(&Program {
                blocks: bp
                    .parse(
                        r#"
                    # 0
                    constant_num<int, 1>() -> (one);
                    split_gas<5, 1>(cost) -> (cost, use_cost);
                    enact_calc<int, 0>(one, use_cost) -> (one);
                    duplicate_num<int>(n) -> (n, use);
                    split_gas<4, 1>(cost) -> (cost, use_cost);
                    jump_nz<int>(use, use_cost) { 2() fallthrough() };
                    # 1
                    refund_gas<4>(gb, cost) -> (gb);
                    ignore_num<int>(n) -> ();
                    return(gb, one);
                    # 2
                    add<int, -1>(n) -> (n_1);
                    split_gas<3, 1>(cost) -> (cost, use_cost);
                    enact_calc<int, 0>(n_1, use_cost) -> (n_1);
                    duplicate_num<int>(n_1) -> (n_1, use);
                    split_gas<2, 1>(cost) -> (cost, use_cost);
                    jump_nz<int>(use, use_cost) { 4() fallthrough() };
                    # 3
                    refund_gas<2>(gb, cost) -> (gb);
                    ignore_num<int>(n_1) -> ();
                    return(gb, one);
                    # 4
                    ignore_num<int>(one) -> ();
                    split_gas<1, 1>(cost) -> (split_gas_cost, use_cost);
                    get_gas<1, 6, 2, 6 ,2>(gb, split_gas_cost) {
                        6(gb, dec_cost, call1_inner_cost, call1_outer_cost,
                          call2_inner_cost, call2_outer_cost)
                        fallthrough(gb)
                    };
                    # 5
                    ignore_num<int>(n_1) -> ();
                    constant_num<int, -10000>() -> (minus);
                    enact_calc<int, 0>(minus, use_cost) -> (minus);
                    return(gb, minus);
                    # 6
                    duplicate_num<int>(n_1) -> (n_1, n_2);
                    add<int, -1>(n_2) -> (n_2);
                    enact_calc<int, 1>(n_2, dec_cost) -> (n_2);
                    tuple_pack<GasBuiltin, int, Gas<6>>(gb, n_1, call1_inner_cost) -> (input);
                    Fibonacci(input, call1_outer_cost) -> (output);
                    tuple_unpack<GasBuiltin, int>(output) -> (gb, r1);
                    tuple_pack<GasBuiltin, int, Gas<6>>(gb, n_2, call2_inner_cost) -> (input);
                    Fibonacci(input, call2_outer_cost) -> (output);
                    tuple_unpack<GasBuiltin, int>(output) -> (gb, r2);
                    add<int>(r1, r2) -> (r);
                    enact_calc<int, 1>(r, use_cost) -> (r);
                    return(gb, r);"#
                    )
                    .unwrap(),
                funcs: vec![Function {
                    name: "Fibonacci".to_string(),
                    args: vec![
                        typed(as_id("gb"), gas_builtin_type()),
                        typed(as_id("n"), int_type()),
                        typed(as_id("cost"), gas_type(6)),
                    ],
                    res_types: vec![gas_builtin_type(), int_type()],
                    entry: BlockId(0),
                }]
            }),
            Ok(())
        );
    }
}
