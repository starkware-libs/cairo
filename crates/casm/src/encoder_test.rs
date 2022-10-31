use num_bigint::BigInt;
use test_case::test_case;

use crate::casm;
use crate::inline::CasmContext;

#[test_case(
    casm!(jmp abs 3;),
    0x8780017fff7fffu64,
    Some(3);
    "jmp abs 3;"
)]
#[test_case(
    casm!(jmp rel -5, ap++;),
    0x90780017fff7fffu64,
    Some(-5); "jmp rel -5, ap++;"
)]
#[test_case(
    casm!(call abs 3;),
    0x1084800180018000u64,
    Some(3);
    "call abs 3;"
)]
#[test_case(
    casm!(call rel (-5);),
    0x1104800180018000u64,
    Some(-5);
    "call rel (-5);"
)]
#[test_case(
    casm!(jmp rel 205 if [ap + 5] != 0;),
    0x20680017fff8005u64,
    Some(205);
    "jmp rel 205 if [ap + 5] != 0;"
)]
#[test_case(
    casm!([ap + 5] = 205;),
    0x400680017fff8005u64,
    Some(205);
    "[ap + 5] = 205;"
)]
#[test_case(
    casm!(ret;),
    0x208b7fff7fff7ffeu64,
    None;
    "ret;"
)]
#[test_case(
    casm!(ap += 205;),
    0x40780017fff7fffu64,
    Some(205);
    "ap += 205;"
)]
fn test_encode(mut casm: CasmContext, encoding: u64, immediate: Option<i16>) {
    let enc = BigInt::from(encoding);
    assert_eq!(
        casm.instructions.remove(0).assemble().encode(),
        if let Some(imm) = immediate { vec![enc, BigInt::from(imm)] } else { vec![enc] }
    );
}
