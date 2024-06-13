use core::circuit::{
    RangeCheck96, AddMod, MulMod, u96, CircuitElement, CircuitInput, circuit_add, circuit_sub,
    circuit_mul, circuit_inverse, EvalCircuitResult, EvalCircuitTrait, u384, CircuitOutputsTrait,
    CircuitModulus, FillInputResultTrait, CircuitInputs,
};

use core::test::test_utils::assert_eq;
use core::traits::TryInto;

#[test]
fn test_u96() {
    let a: u96 = 0x123;
    assert_eq!(a, 0x123);
}

#[test]
fn test_builtins() {
    core::internal::require_implicit::<RangeCheck96>();
    core::internal::require_implicit::<AddMod>();
    core::internal::require_implicit::<MulMod>();
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
    let outputs =
        match (mul,).new_inputs().next([3, 0, 0, 0]).next([6, 0, 0, 0]).done().eval(modulus) {
        EvalCircuitResult::Success(outputs) => { outputs },
        EvalCircuitResult::Failure((_, _)) => { panic!("Expected success") }
    };

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
    match (out0,).new_inputs().next([11, 0, 0, 0]).done().eval(modulus) {
        EvalCircuitResult::Failure((_, _)) => {},
        EvalCircuitResult::Success(_outputs) => { panic!("Expected failure"); }
    }
}
