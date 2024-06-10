use core::circuit::{
    RangeCheck96, AddMod, MulMod, u96, CircuitElement, CircuitInput, CircuitDefinition, circuit_add,
    circuit_sub, circuit_mul, circuit_inverse, EvalCircuitResult, FillInputResult, InputAccumulatorTrait,
    CircuitDescriptorTrait, u384, CircuitOutputsTrait, CircuitModulus
};


use core::traits::TryInto;

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
fn test_circuit() {
    let in1 = CircuitElement::<CircuitInput<0>> {};
    let in2 = CircuitElement::<CircuitInput<1>> {};
    let out1 = circuit_mul(circuit_inverse(circuit_add(in1, in2)), circuit_sub(in2, in1));
    let circ = (out1,);
    let inputs = circ.init();

    let inputs = match inputs.fill_input([1, 2, 3, 4]) {
        FillInputResult::More(new_inputs) => new_inputs,
        FillInputResult::Done(_data) => { panic!("Expected more inputs") }
    };
    let data = match inputs.fill_input([1, 2, 3, 4]) {
        FillInputResult::More(_new_inputs) => panic!("Expected Done"),
        FillInputResult::Done(data) => data
    };

    let modulus = TryInto::<_, CircuitModulus>::try_into([1, 2, 3, 4]).unwrap();
    match circ.get_descriptor().eval(data, modulus) {
        EvalCircuitResult::Failure((_, _)) => {},
        EvalCircuitResult::Success(outputs) => { outputs.get_output(out1); }
    }
}
