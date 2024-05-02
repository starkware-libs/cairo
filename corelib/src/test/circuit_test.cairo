use core::circuit::{
    RangeCheck96, u96, CircuitElement, CircuitInput, CircuitDefinition, circuit_add
};

#[test]
fn test_u96() {
    let _a: u96 = 0x123;
}


#[test]
fn test_rc96() {
    core::internal::require_implicit::<RangeCheck96>();
}

#[test]
fn test_circuit_definition() {
    let in1 = CircuitElement::<CircuitInput<0>> {};
    let in2 = CircuitElement::<CircuitInput<1>> {};
    let out1 = circuit_add(in1, in2);
    (out1,).init();
}


use core::circuit::{
    AddModGate, fill_circuit_input, FillInputResult, CircuitInputAccumulator, init_circuit_data
};
type MyCircuit = (AddModGate<CircuitInput<0>, CircuitInput<1>>, CircuitInput<2>,);

fn foo(
    accumulator: CircuitInputAccumulator<MyCircuit>, val: [u96; 4]
) -> FillInputResult<MyCircuit> {
    fill_circuit_input(accumulator, val)
}
