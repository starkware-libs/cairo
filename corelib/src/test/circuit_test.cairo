use core::circuit::{
    RangeCheck96, AddMod, MulMod, u96, CircuitElement, CircuitInput, circuit_add, circuit_sub,
    circuit_mul, circuit_inverse, EvalCircuitTrait, u384, CircuitOutputsTrait, CircuitModulus,
    FillInputResultTrait, CircuitInputs, CircuitPendingInput,
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
        match (mul, add, inv)
            .new_inputs()
            .next([3, 0, 0, 0])
            .next([6, 0, 0, 0])
            .done()
            .eval(modulus) {
        Result::Ok(outputs) => { outputs },
        Result::Err(_) => { panic!("Expected success") }
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
            }
    );
    assert!(
        0x10000002300000045000000670000008_u128
            .into() == u384 {
                limb0: 0x300000045000000670000008, limb1: 0x10000002, limb2: 0, limb3: 0,
            }
    );
    assert!(
        0x70000023000000450000006700000089000000ab000000cd000000ef0000000_felt252
            .into() == u384 {
                limb0: 0xb000000cd000000ef0000000,
                limb1: 0x50000006700000089000000a,
                limb2: 0x700000230000004,
                limb3: 0,
            }
    );
}


fn test_fill_inputs_loop() {
    let in1 = CircuitElement::<CircuitInput<0>> {};
    let in2 = CircuitElement::<CircuitInput<1>> {};
    let add = circuit_add(in1, in2);

    let mut inputs: Array::<[u96; 4]> = array![[1, 0, 0, 0], [2, 0, 0, 0]];
    let mut circuit_inputs = (add,).new_inputs();

    while let Option::Some(input) = inputs.pop_front() {
        circuit_inputs = circuit_inputs.next(input);
    };

    circuit_inputs.done();
}

fn test_into_u96_guarantee() {
    let input_opt: Option<CircuitPendingInput> = [1, 2, 3, 4].try_into();
    assert!(input_opt.is_some());
    let input_opt: Option<CircuitPendingInput> = [0x1000000000000000000000001, 2, 3, 4].try_into();
    assert!(input_opt.is_none());
    let input_opt: Option<CircuitPendingInput> = [1, 0x1000000000000000000000002, 3, 4].try_into();
    assert!(input_opt.is_none());
    let input_opt: Option<CircuitPendingInput> = [1, 2, 0x1000000000000000000000003, 4].try_into();
    assert!(input_opt.is_none());
    let input_opt: Option<CircuitPendingInput> = [1, 2, 3, 0x1000000000000000000000004].try_into();
    assert!(input_opt.is_none());
}
