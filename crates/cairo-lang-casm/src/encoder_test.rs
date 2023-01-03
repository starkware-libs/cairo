use num_bigint::BigInt;
use pretty_assertions::assert_eq;
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
    casm!(jmp rel 2 if [ap - 1] != 0, ap++;),
    0xa0680017fff7fffu64,
    Some(2);
    "jmp rel 2 if [ap + (-1)] != 0;"
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
#[test_case(
    casm!([ap + 0] = [fp + -5], ap++;),
    0x480a7ffb7fff8000,
    None;
    "[ap + 0] = [fp + -5], ap++;"
)]
#[test_case(
    casm!([ap] = [ap - 3], ap++;),
    0x48127ffd7fff8000,
    None;
    "[ap + 0] = [ap + -3], ap++;"
)]
fn test_encode(mut casm: CasmContext, encoding: u64, immediate: Option<i16>) {
    let enc = BigInt::from(encoding);
    assert_eq!(
        casm.instructions.remove(0).assemble().encode(),
        if let Some(imm) = immediate { vec![enc, BigInt::from(imm)] } else { vec![enc] }
    );
}

#[test_case(
    casm! {
        [ap + 0] = 1, ap++;
        [ap + 0] = 1, ap++;
        [ap + 0] = 13, ap++;
        call rel 3;
        ret;
        jmp rel 5 if [fp + -3] != 0;
        [ap + 0] = [fp + -5], ap++;
        jmp rel 8;
        [ap + 0] = [fp + -4], ap++;
        [ap + 0] = [fp + -5] + [fp + -4], ap++;
        [fp + -3] = [ap + 0] + 1, ap++;
        call rel (-9);
        ret;
    },
    vec![
        0x480680017fff8000, 1, 0x480680017fff8000, 1, 0x480680017fff8000, 13, 0x1104800180018000,
        3, 0x208b7fff7fff7ffe, 0x20780017fff7ffd, 5, 0x480a7ffb7fff8000, 0x10780017fff7fff, 8,
        0x480a7ffc7fff8000, 0x482a7ffc7ffb8000, 0x4825800180007ffd, 1, 0x1104800180018000, -9,
        0x208b7fff7fff7ffe
    ];
    "fib(1, 1, 13)"
)]
fn test_encode_multiple(casm: CasmContext, expected: Vec<i128>) {
    let exp: Vec<BigInt> = expected.into_iter().map(BigInt::from).collect();
    let enc: Vec<BigInt> =
        casm.instructions.iter().flat_map(|inst| inst.assemble().encode()).collect();
    assert_eq!(enc, exp);
}
