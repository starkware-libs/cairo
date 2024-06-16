#[starknet::contract]
mod circuit_contract {
    use core::circuit::{
        CircuitElement, CircuitInput, circuit_add, circuit_inverse, CircuitOutputsTrait,
        CircuitModulus, FillInputResultTrait, CircuitInputs, EvalCircuitTrait, EvalCircuitResult
    };


     #[storage]
    struct Storage {
    }

 
    /// An external method that requires the `AddMod`, `MulMod` and `RangeCheck96` builtins.
    #[external(v0)]
    fn circuit_builtins(ref self: ContractState,) {
        let in1 = CircuitElement::<CircuitInput<0>> {};
        let in2 = CircuitElement::<CircuitInput<1>> {};
        let add = circuit_add(in1, in2);
        let inv = circuit_inverse(add);

        let modulus = TryInto::<_, CircuitModulus>::try_into([7, 0, 0, 0]).unwrap();
        let _outputs =
            match (inv,).new_inputs().next([3, 0, 0, 0]).next([6, 0, 0, 0]).done().eval(modulus) {
            EvalCircuitResult::Success(outputs) => { outputs },
            EvalCircuitResult::Failure((_, _)) => { panic!("Expected success") }
        };
    }   
}
