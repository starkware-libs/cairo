use core::circuit::{
    RangeCheck96, AddMod, MulMod, u96, CircuitElement, CircuitInput, circuit_add, circuit_sub,
    circuit_mul, circuit_inverse, EvalCircuitResult, EvalCircuitTrait, u384, CircuitOutputsTrait,
    CircuitModulus, FillInputResultTrait, CircuitInputs,
};



pub fn eval_circuit() -> u384 {
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
        EvalCircuitResult::Success(outputs) => { outputs },
        EvalCircuitResult::Failure((_, _)) => { panic!("Expected success") }
    };

    outputs.get_output(mul)
}
