fn collatz_program() -> sierra::graph::Program {
    sierra::ProgramParser::new()
        .parse(
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

        Collatz@0(gb: GasBuiltin, n: int, cost: Gas<9>) -> (GasBuiltin, int);"#,
        )
        .unwrap()
}

#[test]
fn soundness_test() {
    assert_eq!(sierra::soundness::validate(&collatz_program()), Ok(()));
}

#[test]
fn vm_test() {
    let prog = collatz_program();
    assert_eq!(
        sierra::simulation::run(&prog, "Collatz", vec![100, 5]), // 5 -> 16 -> 8 -> 4 -> 2 -> 1
        Ok(vec![47, 5])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Collatz", vec![200, 7]), // 7 -> 22 -> 11 -> 34 -> 17 -> 52 -> 26 -> 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
        Ok(vec![30, 16])
    );
    assert_eq!(
        sierra::simulation::run(&prog, "Collatz", vec![100, 7]), // OOG
        Ok(vec![5, -1])
    );
}
