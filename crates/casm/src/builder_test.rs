use indoc::indoc;
use itertools::join;

use super::CasmBuilder;
use crate::{casm_build_extend, res};

#[test]
fn test_ap_change_fixes() {
    let mut builder = CasmBuilder::default();
    let ap_at_7_mul_34 = builder.add_var(res!([ap + 7] * 34));
    let fp_at_minus_3 = builder.add_var(res!([fp - 3]));
    let imm5 = builder.add_var(res!(5));
    let ap_at_5 = builder.add_var(res!([ap + 5]));
    casm_build_extend! {builder,
        let ap_at_5_mul5 = ap_at_5 * imm5;
        ap += 2;
        let fp_at_minus_3_plus_ap_at_5 = fp_at_minus_3 + ap_at_5;
    };
    let result = builder.build();
    assert_eq!(result.fallthrough_state.get_adjusted(ap_at_7_mul_34), res!([ap + 5] * 34));
    assert_eq!(result.fallthrough_state.get_adjusted(fp_at_minus_3), res!([fp - 3]));
    assert_eq!(result.fallthrough_state.get_adjusted(ap_at_5), res!([ap + 3]));
    assert_eq!(result.fallthrough_state.get_adjusted(imm5), res!(5));
    assert_eq!(result.fallthrough_state.get_adjusted(ap_at_5_mul5), res!([ap + 3] * 5));
    assert_eq!(
        result.fallthrough_state.get_adjusted(fp_at_minus_3_plus_ap_at_5),
        res!([fp - 3] + [ap + 3])
    );
    assert_eq!(result.fallthrough_state.ap_change, 2);
    assert!(result.label_state.is_empty());
    assert_eq!(
        join(result.instructions.iter().map(|inst| format!("{inst};\n")), ""),
        indoc! {"
            ap += 2;
        "}
    );
    assert!(result.awaiting_relocations.is_empty());
}

#[test]
fn test_awaiting_relocations() {
    let mut builder = CasmBuilder::default();
    casm_build_extend! {builder,
        ap += 5;
        jump Target;
    };
    let result = builder.build();
    assert_eq!(result.label_state.len(), 1);
    assert_eq!(result.label_state["Target"].ap_change, 5);
    assert_eq!(result.awaiting_relocations, [1]);
    assert_eq!(
        join(result.instructions.iter().map(|inst| format!("{inst};\n")), ""),
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
    let result = builder.build();
    assert!(result.label_state.is_empty());
    assert!(result.awaiting_relocations.is_empty());
    assert_eq!(result.fallthrough_state.ap_change, 3);
    assert_eq!(
        join(result.instructions.iter().map(|inst| format!("{inst};\n")), ""),
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
    let result = builder.build();
    assert!(result.label_state.is_empty());
    assert!(result.awaiting_relocations.is_empty());
    assert_eq!(result.fallthrough_state.ap_change, 3);
    assert_eq!(
        join(result.instructions.iter().map(|inst| format!("{inst};\n")), ""),
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
    builder.build();
}

#[test]
fn test_aligned_branch_intersect() {
    let mut builder = CasmBuilder::default();
    let var = builder.add_var(res!([ap + 7]));
    casm_build_extend! {builder,
        tempvar _unused;
        jump X if var != 0;
        jump ONE_ALLOC;
        X:
        ONE_ALLOC:
    };
    let result = builder.build();
    assert!(result.label_state.is_empty());
    assert!(result.awaiting_relocations.is_empty());
    assert_eq!(result.fallthrough_state.ap_change, 1);
    assert_eq!(result.fallthrough_state.allocated, 1);
    assert_eq!(
        join(result.instructions.iter().map(|inst| format!("{inst};\n")), ""),
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
    let var = builder.add_var(res!([ap + 7]));
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
    builder.build();
}
