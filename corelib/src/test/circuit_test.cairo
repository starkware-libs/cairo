use core::circuit::{
    RangeCheck96, AddMod, MulMod, u96, CircuitElement, CircuitInput, CircuitDefinition, circuit_add,
    FillInputResult, InputAccumulatorTrait, u384_is_zero, u384
};


use core::traits::TryInto;

#[test]
fn test_u96() {
    let _a: u96 = 0x123;
}

/// Helpr function to test if a u384 is zero
fn try_into_nz(val: u384) -> Option<NonZero<u384>> {
    val.try_into()
}

#[test]
fn test_u384_is_zero() {
    assert(
        try_into_nz(u384 { limb0: 0, limb1: 0, limb2: 0, limb3: 0 }).is_none(), 'Should be None.'
    );
    assert(
        try_into_nz(u384 { limb0: 0, limb1: 17, limb2: 0, limb3: 0 }).is_some(), 'Should be Some.'
    );
}

#[test]
fn test_builtins() {
    core::internal::require_implicit::<RangeCheck96>();
    core::internal::require_implicit::<AddMod>();
    core::internal::require_implicit::<MulMod>();
}

#[test]
fn test_circuit_definition() {
    let in1 = CircuitElement::<CircuitInput<0>> {};
    let in2 = CircuitElement::<CircuitInput<1>> {};
    let out1 = circuit_add(in1, in2);
    let inputs = (out1,).init();

    let inputs = match inputs.fill_input([1, 2, 3, 4]) {
        FillInputResult::More(new_inputs) => new_inputs,
        FillInputResult::Done(_data) => { panic!("Expected more inputs") }
    };
    let _data = match inputs.fill_input([1, 2, 3, 4]) {
        FillInputResult::More(_new_inputs) => panic!("Expected Done"),
        FillInputResult::Done(data) => data
    };
}
