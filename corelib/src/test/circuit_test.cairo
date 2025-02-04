use crate::circuit::{
    AddInputResultTrait, AddMod, CircuitElement, CircuitInput, CircuitInputs, CircuitModulus,
    CircuitOutputsTrait, EvalCircuitTrait, MulMod, RangeCheck96, circuit_add, circuit_inverse,
    circuit_mul, circuit_sub, u384, u96,
};
use crate::num::traits::Zero;
use crate::traits::TryInto;

#[test]
fn test_u96() {
    let a: u96 = 0x123;
    assert_eq!(a, 0x123);
}

#[test]
fn test_try_into_u96() {
    assert_eq!(0x123_felt252.try_into(), Option::<u96>::Some(0x123));
    assert_eq!(0x1000000000000000000000000_felt252.try_into(), Option::<u96>::None);
}

#[test]
fn test_builtins() {
    crate::internal::require_implicit::<RangeCheck96>();
    crate::internal::require_implicit::<AddMod>();
    crate::internal::require_implicit::<MulMod>();
}

#[test]
fn test_circuit_success() {
    let in1 = CircuitElement::<CircuitInput<0>> {};
    let in2 = CircuitElement::<CircuitInput<1>> {};
    let add = circuit_add(in1, in2);
    let inv = circuit_inverse(add);
    let sub = circuit_sub(inv, in2);
    let mul = circuit_mul(inv, sub);

    let modulus = TryInto::<_, CircuitModulus>::try_into([7, 0, 0, 0]).unwrap();
    let outputs = (mul, add, inv)
        .new_inputs()
        .next([3, 0, 0, 0])
        .next([6, 0, 0, 0])
        .done()
        .eval(modulus)
        .unwrap();

    assert_eq!(outputs.get_output(add), u384 { limb0: 2, limb1: 0, limb2: 0, limb3: 0 });
    assert_eq!(outputs.get_output(inv), u384 { limb0: 4, limb1: 0, limb2: 0, limb3: 0 });
    assert_eq!(outputs.get_output(sub), u384 { limb0: 5, limb1: 0, limb2: 0, limb3: 0 });
    assert_eq!(outputs.get_output(mul), u384 { limb0: 6, limb1: 0, limb2: 0, limb3: 0 });
}


#[test]
fn test_circuit_failure() {
    let in0 = CircuitElement::<CircuitInput<0>> {};
    let out0 = circuit_inverse(in0);

    let modulus = TryInto::<_, CircuitModulus>::try_into([55, 0, 0, 0]).unwrap();
    (out0,).new_inputs().next([11, 0, 0, 0]).done().eval(modulus).unwrap_err();
    (out0,).new_inputs().next([11, 0, 0, 0]).done().eval(modulus).unwrap_err();
}

#[test]
fn test_into_u384() {
    assert!(
        0x100000023000000450000006700000089000000ab000000cd000000ef0000000_u256
            .into() == u384 {
                limb0: 0xb000000cd000000ef0000000,
                limb1: 0x50000006700000089000000a,
                limb2: 0x1000000230000004,
                limb3: 0,
            },
    );
    assert!(
        0x10000002300000045000000670000008_u128
            .into() == u384 {
                limb0: 0x300000045000000670000008, limb1: 0x10000002, limb2: 0, limb3: 0,
            },
    );
    assert!(
        0x70000023000000450000006700000089000000ab000000cd000000ef0000000_felt252
            .into() == u384 {
                limb0: 0xb000000cd000000ef0000000,
                limb1: 0x50000006700000089000000a,
                limb2: 0x700000230000004,
                limb3: 0,
            },
    );
}

#[test]
fn test_from_u384() {
    let limb0 = 0xb000000cd000000ef0000000;
    let limb1 = 0x50000006700000089000000a;
    let limb2 = 0x1000000230000004;
    let limb3 = 0;
    assert!(
        u384 { limb0, limb1, limb2, limb3 }
            .try_into() == Some(
                0x100000023000000450000006700000089000000ab000000cd000000ef0000000_u256,
            ),
    );
    assert!(u384 { limb0, limb1, limb2, limb3: 1 }.try_into() == Option::<u256>::None);
    assert!(
        u384 { limb0, limb1, limb2: 0x11000000230000004, limb3 }.try_into() == Option::<u256>::None,
    );
    let limb0 = 0x300000045000000670000008;
    let limb1 = 0x10000002;
    let limb2 = 0;
    let limb3 = 0;
    assert!(
        u384 { limb0, limb1, limb2, limb3 }
            .try_into() == Some(0x10000002300000045000000670000008_u128),
    );
    assert!(u384 { limb0, limb1: 0x110000002, limb2, limb3 }.try_into() == Option::<u128>::None);
    assert!(u384 { limb0, limb1, limb2: 1, limb3 }.try_into() == Option::<u128>::None);
    assert!(u384 { limb0, limb1, limb2, limb3: 1 }.try_into() == Option::<u128>::None);
}

#[test]
fn test_fill_inputs_loop() {
    let in1 = CircuitElement::<CircuitInput<0>> {};
    let in2 = CircuitElement::<CircuitInput<1>> {};
    let add = circuit_add(in1, in2);

    let mut inputs: Array<[u96; 4]> = array![[1, 0, 0, 0], [2, 0, 0, 0]];
    let mut circuit_inputs = (add,).new_inputs();

    while let Some(input) = inputs.pop_front() {
        circuit_inputs = circuit_inputs.next(input);
    }

    let modulus = TryInto::<_, CircuitModulus>::try_into([55, 0, 0, 0]).unwrap();
    circuit_inputs.done().eval(modulus).unwrap();
}

#[test]
fn test_u384_serde() {
    let value = u384 {
        limb0: 0xb000000cd000000ef0000000,
        limb1: 0x50000006700000089000000a,
        limb2: 0x100000023000000450000000,
        limb3: 0x80000009a000000bc0000000,
    };
    let serialized = array![
        0x50000006700000089000000ab000000cd000000ef0000000,
        0x80000009a000000bc0000000100000023000000450000000,
    ];
    let mut buffer = array![];
    value.serialize(ref buffer);
    assert!(buffer == serialized);

    let mut serialized = serialized.span();

    assert!(Serde::<u384>::deserialize(ref serialized) == Some(value));
}

#[test]
fn test_u384_zero() {
    assert_eq!(Zero::zero(), u384 { limb0: 0, limb1: 0, limb2: 0, limb3: 0 });
    assert!(Zero::is_zero(@u384 { limb0: 0, limb1: 0, limb2: 0, limb3: 0 }));
    assert!(Zero::is_non_zero(@u384 { limb0: 0, limb1: 1, limb2: 0, limb3: 0 }));
}
