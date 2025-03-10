use core::qm31::{QM31Trait, m31, qm31, qm31_const};

#[test]
fn test_qm31_add_and_sub() {
    let a = qm31_const::<0x544b2fba, 0x673cff77, 0x60713d44, 0x499602d2>();
    let b = qm31_const::<0x499602d2, 0x544b2fba, 0x673cff77, 0x60713d44>();
    let c = qm31_const::<0x1de1328d, 0x3b882f32, 0x47ae3cbc, 0x2a074017>();
    assert!(a + b == c);
    assert!(b + a == c);
    assert!(c - a == b);
    assert!(c - b == a);
}

#[test]
fn test_qm31_mul_and_div() {
    let a = qm31_const::<0x544b2fba, 0x673cff77, 0x60713d44, 0x499602d2>();
    let b = qm31_const::<0x4b18de99, 0x55f6fb62, 0x6e2290d9, 0x7cd851b9>();
    let c = qm31_const::<0x38810ab4, 0x5a0fd30a, 0x2527b81e, 0x4b1ed1cd>();
    assert!(a * b == c);
    assert!(b * a == c);
    assert!(c / a == b);
    assert!(c / b == a);
}

#[test]
fn test_qm31_inverse() {
    let one = qm31_const::<1, 0, 0, 0>();
    let a = qm31_const::<0x4b18de99, 0x55f6fb62, 0x6e2290d9, 0x7cd851b9>();
    assert!((one / a) * a == one);
    let a = qm31_const::<1, 2, 3, 4>();
    assert!((one / a) * a == one);
    let a = qm31_const::<0x6849959f, 0x31bf5a51, 0x730c2120, 0x7b0430a5>();
    assert!((one / a) * a == one);
}

#[test]
fn test_pack() {
    assert!(QM31Trait::new(1, 2, 3, 4) == qm31_const::<1, 2, 3, 4>());
    assert!(QM31Trait::new(2, 3, 4, 1) == qm31_const::<2, 3, 4, 1>());
    assert!(QM31Trait::new(3, 4, 1, 2) == qm31_const::<3, 4, 1, 2>());
    assert!(QM31Trait::new(4, 1, 2, 3) == qm31_const::<4, 1, 2, 3>());
}

#[test]
fn test_unpack() {
    assert_eq!(qm31_const::<1, 2, 3, 4>().unpack(), [1, 2, 3, 4]);
    assert_eq!(qm31_const::<2, 3, 4, 1>().unpack(), [2, 3, 4, 1]);
    assert_eq!(qm31_const::<3, 4, 1, 2>().unpack(), [3, 4, 1, 2]);
    assert_eq!(qm31_const::<4, 1, 2, 3>().unpack(), [4, 1, 2, 3]);
}

#[test]
fn test_m31_into_qm31() {
    assert_eq!(Into::<m31, qm31>::into(1).unpack(), [1, 0, 0, 0]);
    assert_eq!(Into::<m31, qm31>::into(2).unpack(), [2, 0, 0, 0]);
    assert_eq!(Into::<m31, qm31>::into(3).unpack(), [3, 0, 0, 0]);
    assert_eq!(Into::<m31, qm31>::into(4).unpack(), [4, 0, 0, 0]);
}
use core::qm31::m31_ops;

#[test]
fn test_m31_ops() {
    assert_eq!(m31_ops::add(0x544b2fba, 0x4b18de99), 0x1f640e54);
    assert_eq!(m31_ops::sub(0x4b18de99, 0x544b2fba), 0x76cdaede);
    assert_eq!(m31_ops::mul(0x544b2fba, 0x4b18de99), 0x3d3740d1);
    assert_eq!(m31_ops::div(0x544b2fba, 0x4b18de99), 0x4b887296);
}
