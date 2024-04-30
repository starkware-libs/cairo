use core::circuit::{RangeCheck96, u96, CircuitElement, CircuitInput, CircuitDefinition, circuit_add};

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
