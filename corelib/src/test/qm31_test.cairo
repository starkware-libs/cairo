extern type qm31;
type m31 = core::internal::bounded_int::BoundedInt<0, 0x7ffffffe>;

extern fn qm31_is_zero(a: qm31) -> core::internal::OptionRev<NonZero<qm31>> nopanic;
extern fn qm31_add(a: qm31, b: qm31) -> qm31 nopanic;
extern fn qm31_sub(a: qm31, b: qm31) -> qm31 nopanic;
extern fn qm31_mul(a: qm31, b: qm31) -> qm31 nopanic;
extern fn qm31_div(a: qm31, b: NonZero<qm31>) -> qm31 nopanic;
extern fn qm31_const<const W0: m31, const W1: m31, const W2: m31, const W3: m31>() -> qm31 nopanic;
extern fn qm31_pack(w0: m31, w1: m31, w2: m31, w3: m31) -> qm31 nopanic;
extern fn qm31_unpack(a: qm31) -> (m31, m31, m31, m31) implicits(core::RangeCheck) nopanic;

impl qm31Copy of Copy<qm31>;
impl qm31Drop of Drop<qm31>;
impl qm31Add of Add<qm31> {
    fn add(lhs: qm31, rhs: qm31) -> qm31 {
        qm31_add(lhs, rhs)
    }
}
impl qm31Sub of Sub<qm31> {
    fn sub(lhs: qm31, rhs: qm31) -> qm31 {
        qm31_sub(lhs, rhs)
    }
}
impl qm31Mul of Mul<qm31> {
    fn mul(lhs: qm31, rhs: qm31) -> qm31 {
        qm31_mul(lhs, rhs)
    }
}
impl qm31Div of Div<qm31> {
    fn div(lhs: qm31, rhs: qm31) -> qm31 {
        qm31_div(lhs, rhs.try_into().expect('qm31_div by 0'))
    }
}

impl QM31TryIntoNonZero of TryInto<qm31, NonZero<qm31>> {
    fn try_into(self: qm31) -> Option<NonZero<qm31>> {
        match qm31_is_zero(self) {
            core::internal::OptionRev::None => None,
            core::internal::OptionRev::Some(x) => Some(x),
        }
    }
}
impl QM31PartialEq of PartialEq<qm31> {
    fn eq(lhs: @qm31, rhs: @qm31) -> bool {
        match qm31_is_zero(*lhs - *rhs) {
            core::internal::OptionRev::None => true,
            core::internal::OptionRev::Some(_) => false,
        }
    }
}

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
    assert!(qm31_pack(1, 2, 3, 4) == qm31_const::<1, 2, 3, 4>());
    assert!(qm31_pack(2, 3, 4, 1) == qm31_const::<2, 3, 4, 1>());
    assert!(qm31_pack(3, 4, 1, 2) == qm31_const::<3, 4, 1, 2>());
    assert!(qm31_pack(4, 1, 2, 3) == qm31_const::<4, 1, 2, 3>());
}

#[test]
fn test_unpack() {
    assert_eq!(qm31_unpack(qm31_const::<1, 2, 3, 4>()), (1, 2, 3, 4));
    assert_eq!(qm31_unpack(qm31_const::<2, 3, 4, 1>()), (2, 3, 4, 1));
    assert_eq!(qm31_unpack(qm31_const::<3, 4, 1, 2>()), (3, 4, 1, 2));
    assert_eq!(qm31_unpack(qm31_const::<4, 1, 2, 3>()), (4, 1, 2, 3));
}
