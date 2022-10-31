use num_bigint::{BigInt, ToBigInt};

use crate::casm;
use crate::inline::CasmContext;

macro_rules! inst {
    ($enc:expr, $imm:expr) => {
        vec![$enc.to_bigint().unwrap(), $imm.to_bigint().unwrap()]
    };
    ($enc:expr) => {
        vec![$enc.to_bigint().unwrap()]
    };
}

/// Takes a casm instruction, which can be constructed using the macro casm!, and
/// returns its assembled representation.
fn encode_instruction(mut casm: CasmContext) -> Vec<BigInt> {
    casm.instructions.remove(0).assemble().encode().expect("Failed to encode instruction")
}

#[test]
fn test_jump_assemble() {
    assert_eq!(encode_instruction(casm!(jmp abs 3;)), inst!(0x8780017fff7fffu64, 3));

    assert_eq!(encode_instruction(casm!(jmp rel -5, ap++;)), inst!(0x90780017fff7fffu64, -5));
}

#[test]
fn test_call_assemble() {
    assert_eq!(encode_instruction(casm!(call abs 3;)), inst!(0x1084800180018000u64, 3));
    assert_eq!(encode_instruction(casm!(call rel (-5);)), inst!(0x1104800180018000u64, -5));
}

#[test]
fn test_jnz_assemble() {
    assert_eq!(
        encode_instruction(casm!(jmp rel 205 if [ap + 5] != 0;)),
        inst!(0x20680017fff8005u64, 205)
    );
}

#[test]
fn test_assert_eq_assemble() {
    assert_eq!(encode_instruction(casm!([ap + 5] = 205;)), inst!(0x400680017fff8005u64, 205));
}

#[test]
fn test_ret_assemble() {
    assert_eq!(encode_instruction(casm!(ret;)), inst!(0x208b7fff7fff7ffeu64));
}

#[test]
fn test_add_ap_assemble() {
    assert_eq!(encode_instruction(casm!(ap += 205;)), inst!(0x40780017fff7fffu64, 205));
}

#[test]
fn test_instruction_with_hint() {
    assert_eq!(encode_instruction(casm!(jmp abs 3;)), inst!(0x8780017fff7fffu64, 3));
}
