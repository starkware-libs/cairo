use indoc::indoc;
use itertools::join;
use pretty_assertions::assert_eq;

use super::CasmBuilder;
use crate::builder::CasmBuildResult;
use crate::cell_expression::CellExpression;
use crate::{casm_build_extend, res};

#[test]
fn test_ap_change_fixes() {
    let mut builder = CasmBuilder::default();
    let ap_at_7_mul_34 = builder.add_var(CellExpression::from_res_operand(res!([ap + 7] * 34)));
    let fp_at_minus_3 = builder.add_var(CellExpression::from_res_operand(res!([fp - 3])));
    let imm5 = builder.add_var(CellExpression::from_res_operand(res!(5)));
    let ap_at_5 = builder.add_var(CellExpression::from_res_operand(res!([ap + 5])));
    casm_build_extend! {builder,
        let ap_at_5_mul5 = ap_at_5 * imm5;
        ap += 2;
        let fp_at_minus_3_plus_ap_at_5 = fp_at_minus_3 + ap_at_5;
    };
    let CasmBuildResult { instructions, branches: [(state, awaiting_relocations)] } =
        builder.build(["Fallthrough"]);
    assert_eq!(
        state.get_adjusted(ap_at_7_mul_34),
        CellExpression::from_res_operand(res!([ap + 5] * 34))
    );
    assert_eq!(state.get_adjusted(fp_at_minus_3), CellExpression::from_res_operand(res!([fp - 3])));
    assert_eq!(state.get_adjusted(ap_at_5), CellExpression::from_res_operand(res!([ap + 3])));
    assert_eq!(state.get_adjusted(imm5), CellExpression::from_res_operand(res!(5)));
    assert_eq!(
        state.get_adjusted(ap_at_5_mul5),
        CellExpression::from_res_operand(res!([ap + 3] * 5))
    );
    assert_eq!(
        state.get_adjusted(fp_at_minus_3_plus_ap_at_5),
        CellExpression::from_res_operand(res!([fp - 3] + [ap + 3]))
    );
    assert_eq!(state.ap_change, 2);
    assert_eq!(
        join(instructions.iter().map(|inst| format!("{inst};\n")), ""),
        indoc! {"
            ap += 2;
        "}
    );
    assert!(awaiting_relocations.is_empty());
}

#[test]
fn test_awaiting_relocations() {
    let mut builder = CasmBuilder::default();
    casm_build_extend! {builder,
        ap += 5;
        jump Target;
    };
    let CasmBuildResult { instructions, branches: [(state, awaiting_relocations)] } =
        builder.build(["Target"]);
    assert_eq!(state.ap_change, 5);
    assert_eq!(awaiting_relocations, [1]);
    assert_eq!(
        join(instructions.iter().map(|inst| format!("{inst};\n")), ""),
        indoc! {"
            ap += 5;
            jmp rel 0;
        "}
    );
}

#[test]
fn test_noop_branch() {
    let mut builder = CasmBuilder::default();
    casm_build_extend! {builder,
        ap += 3;
        jump Target;
        Target:
    };
    let CasmBuildResult { instructions, branches: [(state, awaiting_relocations)] } =
        builder.build(["Fallthrough"]);
    assert!(awaiting_relocations.is_empty());
    assert_eq!(state.ap_change, 3);
    assert_eq!(
        join(instructions.iter().map(|inst| format!("{inst};\n")), ""),
        indoc! {"
            ap += 3;
            jmp rel 2;
        "}
    );
}

#[test]
fn test_allocations() {
    let mut builder = CasmBuilder::default();
    casm_build_extend! {builder,
        tempvar a;
        tempvar b;
        tempvar c;
        assert a = b;
        assert b = c;
        assert c = a;
    };
    let CasmBuildResult { instructions, branches: [(state, awaiting_relocations)] } =
        builder.build(["Fallthrough"]);
    assert!(awaiting_relocations.is_empty());
    assert_eq!(state.ap_change, 3);
    assert_eq!(
        join(instructions.iter().map(|inst| format!("{inst};\n")), ""),
        indoc! {"
            [ap + 0] = [ap + 1], ap++;
            [ap + 0] = [ap + 1], ap++;
            [ap + 0] = [ap + -2], ap++;
        "}
    );
}

#[test]
#[should_panic]
fn test_allocations_not_enough_commands() {
    let mut builder = CasmBuilder::default();
    casm_build_extend! {builder,
        tempvar a;
        tempvar b;
        tempvar c;
        assert a = b;
        assert b = c;
    };
    builder.build(["Fallthrough"]);
}

#[test]
fn test_aligned_branch_intersect() {
    let mut builder = CasmBuilder::default();
    let var = builder.add_var(CellExpression::from_res_operand(res!([ap + 7])));
    casm_build_extend! {builder,
        tempvar _unused;
        jump X if var != 0;
        jump ONE_ALLOC;
        X:
        ONE_ALLOC:
    };
    let CasmBuildResult { instructions, branches: [(state, awaiting_relocations)] } =
        builder.build(["Fallthrough"]);
    assert!(awaiting_relocations.is_empty());
    assert_eq!(state.ap_change, 1);
    assert_eq!(state.allocated, 1);
    assert_eq!(
        join(instructions.iter().map(|inst| format!("{inst};\n")), ""),
        indoc! {"
            jmp rel 4 if [ap + 7] != 0, ap++;
            jmp rel 2;
        "}
    );
}

#[test]
#[should_panic]
fn test_unaligned_branch_intersect() {
    let mut builder = CasmBuilder::default();
    let var = builder.add_var(CellExpression::from_res_operand(res!([ap + 7])));
    casm_build_extend! {builder,
        jump X if var != 0;
        // A single tempvar in this branch.
        tempvar _unused;
        jump ONESIDED_ALLOC;
        // No allocs in this branch.
        X:
        // When the merge occurs here we will panic on a mismatch.
        ONESIDED_ALLOC:
    };
    builder.build(["Fallthrough"]);
}

#[test]
fn test_calculation_loop() {
    let mut builder = CasmBuilder::default();
    casm_build_extend! {builder,
        const one = 1;
        const ten = 10;
        tempvar a = one;
        tempvar n = ten;
        tempvar b = one;
        rescope{a = a, b = b, n = n, one = one};
        FIB:
        tempvar new_n = n - one;
        tempvar new_b = a + b;
        rescope{a = b, b = new_b, n = new_n, one = one};
        jump FIB if n != 0;
    };
    let CasmBuildResult { instructions, branches: [(state, awaiting_relocations)] } =
        builder.build(["Fallthrough"]);
    assert!(awaiting_relocations.is_empty());
    assert_eq!(state.get_adjusted(b), CellExpression::from_res_operand(res!([ap - 1])));
    assert_eq!(
        join(instructions.iter().map(|inst| format!("{inst};\n")), ""),
        indoc! {"
            [ap + 0] = 1, ap++;
            [ap + 0] = 10, ap++;
            [ap + 0] = 1, ap++;
            [ap + -2] = [ap + 0] + 1, ap++;
            [ap + 0] = [ap + -4] + [ap + -2], ap++;
            jmp rel -3 if [ap + -2] != 0;
        "}
    );
}

#[test]
fn test_call_ret() {
    let mut builder = CasmBuilder::default();
    casm_build_extend! {builder,
        const zero = 0;
        const one = 1;
        const ten = 10;
        const fib10 = 89;
        const fib11 = 144;
        tempvar a = one;
        tempvar n = ten;
        tempvar b = one;
        let (res_a, res_n, res_b) = call FIB;
        assert res_a = fib10;
        assert res_n = zero;
        assert res_b = fib11;
        jump FT;
        FIB:
        tempvar new_a = b;
        tempvar new_n = n - one;
        tempvar new_b = a + b;
        jump REC_CALL if new_n != 0;
        rescope {};
        jump FIB_END;
        REC_CALL:
        let () = call FIB;
        FIB_END:
        ret;
        FT:
    };
    let CasmBuildResult { instructions, branches: [(_, awaiting_relocations)] } =
        builder.build(["Fallthrough"]);
    assert!(awaiting_relocations.is_empty());
    assert_eq!(
        join(instructions.iter().map(|inst| format!("{inst};\n")), ""),
        indoc! {"
            [ap + 0] = 1, ap++;
            [ap + 0] = 10, ap++;
            [ap + 0] = 1, ap++;
            call rel 10;
            [ap + -3] = 89;
            [ap + -2] = 0;
            [ap + -1] = 144;
            jmp rel 13;
            [ap + 0] = [fp + -3], ap++;
            [fp + -4] = [ap + 0] + 1, ap++;
            [ap + 0] = [fp + -5] + [fp + -3], ap++;
            jmp rel 4 if [ap + -2] != 0;
            jmp rel 4;
            call rel -8;
            ret;
        "}
    );
}

#[test]
fn test_local_fib() {
    let mut builder = CasmBuilder::default();
    casm_build_extend! {builder,
        const one = 1;
        const ten = 10;
        const fib11 = 144;
        localvar res = fib11;
        tempvar a = one;
        tempvar n = ten;
        tempvar b = one;
        rescope{a = a, b = b, n = n, one = one, res = res};
        FIB:
        tempvar new_n = n - one;
        tempvar new_b = a + b;
        rescope{a = b, b = new_b, n = new_n, one = one, res = res};
        jump FIB if n != 0;
        assert res = b;
    };
    let CasmBuildResult { instructions, branches: [(_, awaiting_relocations)] } =
        builder.build(["Fallthrough"]);
    assert!(awaiting_relocations.is_empty());
    assert_eq!(
        join(instructions.iter().map(|inst| format!("{inst};\n")), ""),
        indoc! {"
            [fp + 0] = 144, ap++;
            [ap + 0] = 1, ap++;
            [ap + 0] = 10, ap++;
            [ap + 0] = 1, ap++;
            [ap + -2] = [ap + 0] + 1, ap++;
            [ap + 0] = [ap + -4] + [ap + -2], ap++;
            jmp rel -3 if [ap + -2] != 0;
            [fp + 0] = [ap + -1];
        "}
    );
}

#[test]
fn test_array_access() {
    let mut builder = CasmBuilder::default();
    casm_build_extend! {builder,
        const zero = 0;
        const one = 1;
        tempvar a = zero;
        tempvar b = one;
        tempvar ptr;
        hint AllocSegment {} into {dst: ptr};
        assert a = ptr[0];
        assert b = ptr[1];
    };
    let CasmBuildResult { instructions, branches: [(_, awaiting_relocations)] } =
        builder.build(["Fallthrough"]);
    assert!(awaiting_relocations.is_empty());
    assert_eq!(
        join(instructions.iter().map(|inst| format!("{inst};\n")), ""),
        indoc! {"
            [ap + 0] = 0, ap++;
            [ap + 0] = 1, ap++;
            %{ memory[ap + 0] = segments.add() %}
            [ap + -2] = [[ap + 0] + 0], ap++;
            [ap + -2] = [[ap + -1] + 1];
        "}
    );
}
