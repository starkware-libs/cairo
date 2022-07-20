fn fib_program() -> sierra::graph::Program {
    sierra::ProgramParser::new()
        .parse(
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

        Fibonacci@0[ap += unknown](gb: GasBuiltin, n: int, cost: Gas<8>) -> (GasBuiltin, int);"#,
        )
        .unwrap()
}

#[test]
fn soundness_test() {
    assert_eq!(sierra::soundness::validate(&fib_program()), Ok(()));
}

#[test]
fn simulation_test() {
    let prog = fib_program();
    assert_eq!(
        sierra::simulation::run(&prog, "Fibonacci", vec![1000, 5]),
        Ok(vec![832, 8])
    );
}
