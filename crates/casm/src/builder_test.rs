use indoc::indoc;
use itertools::join;
use num_bigint::BigInt;

use super::CasmBuilder;
use crate::operand::{BinOpOperand, Operation, ResOperand};
use crate::{deref, deref_or_immediate};

#[test]
fn test_ap_change_fixes() {
    let mut builder = CasmBuilder::default();
    let ap_at_7_mul_34 = builder.add_var(ResOperand::BinOp(BinOpOperand {
        op: Operation::Mul,
        a: deref!([ap + 7]),
        b: deref_or_immediate!(34),
    }));
    let fp_at_minus_3 = builder.add_var(ResOperand::Deref(deref!([fp - 3])));
    let imm5 = builder.add_var(ResOperand::Immediate(BigInt::from(5)));
    let ap_at_5 = builder.add_var(ResOperand::Deref(deref!([ap + 5])));
    builder.add_ap(2);
    let result = builder.build();
    assert_eq!(
        result.fallthrough_state.get_fixed(ap_at_7_mul_34),
        ResOperand::BinOp(BinOpOperand {
            op: Operation::Mul,
            a: deref!([ap + 5]),
            b: deref_or_immediate!(34)
        })
    );
    assert_eq!(
        result.fallthrough_state.get_fixed(fp_at_minus_3),
        ResOperand::Deref(deref!([fp - 3]))
    );
    assert_eq!(result.fallthrough_state.get_fixed(ap_at_5), ResOperand::Deref(deref!([ap + 3])));
    assert_eq!(result.fallthrough_state.get_fixed(imm5), ResOperand::Immediate(BigInt::from(5)));
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
    builder.add_ap(5);
    builder.jump("Target".to_owned());
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
    builder.add_ap(3);
    builder.jump("Target".to_owned());
    builder.label("Target".to_owned());
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
    let a = builder.alloc_var();
    let b = builder.alloc_var();
    let c = builder.alloc_var();
    builder.assert_vars_eq(a, b);
    builder.assert_vars_eq(b, c);
    builder.assert_vars_eq(c, a);
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
    let a = builder.alloc_var();
    let b = builder.alloc_var();
    let c = builder.alloc_var();
    builder.assert_vars_eq(a, b);
    builder.assert_vars_eq(b, c);
    builder.build();
}
