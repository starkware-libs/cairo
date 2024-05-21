use core::circuit::{
    RangeCheck96, AddMod, MulMod, u96, CircuitElement, CircuitInput, CircuitDefinition, circuit_add,
    FillInputResult, InputAccumulatorTrait
};

#[test]
fn test_u96() {
    let _a: u96 = 0x123;
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
