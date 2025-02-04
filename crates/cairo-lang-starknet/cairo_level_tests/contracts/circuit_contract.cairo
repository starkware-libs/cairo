#[starknet::contract]
mod circuit_contract {
    use core::circuit::{
        AddInputResultTrait, CircuitElement, CircuitInput, CircuitInputs, CircuitModulus,
        EvalCircuitTrait, circuit_add, circuit_inverse,
    };

    #[storage]
    struct Storage {}


    /// An external method that requires the `AddMod`, `MulMod` and `RangeCheck96` builtins.
    #[external(v0)]
    fn circuit_builtins(ref self: ContractState) {
        let in1 = CircuitElement::<CircuitInput<0>> {};
        let in2 = CircuitElement::<CircuitInput<1>> {};
        let add = circuit_add(in1, in2);
        // Making the type much larger.
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let add = circuit_add(add, add);
        let inv = circuit_inverse(add);

        let modulus = TryInto::<_, CircuitModulus>::try_into([7, 0, 0, 0]).unwrap();
        let _outputs =
            match (inv,).new_inputs().next([3, 0, 0, 0]).next([6, 0, 0, 0]).done().eval(modulus) {
            Ok(outputs) => { outputs },
            Err(_) => { panic!("Expected success") },
        };
    }
}
