#[starknet::contract]
mod max_entrypoint_contract {
    use core::circuit::{
        AddInputResultTrait, CircuitElement, CircuitInput, CircuitInputs, CircuitModulus,
        CircuitOutputsTrait, EvalCircuitTrait, circuit_add, circuit_inverse, circuit_mul,
        circuit_sub, u384,
    };

    #[storage]
    struct Storage {}

    #[abi(per_item)]
    #[generate_trait]
    impl Impl of Trait {
        #[external(v0)]
        fn maximal(
            ref self: ContractState, v1: felt252, v2: u128, v3: u384, v4: u32,
        ) -> (felt252, (felt252, felt252, felt252), u128, bool, u384, [u32; 8]) {
            (
                core::pedersen::pedersen(core::pedersen::pedersen(v1, v1), v1),
                {
                    let (s0, s1, s2) = core::poseidon::hades_permutation(v1, v1, v1);
                    core::poseidon::hades_permutation(s0, s1, s2)
                },
                (v2 ^ v4.into()) | v2,
                core::ecdsa::check_ecdsa_signature(v1, v1, v1, v1),
                {
                    let in1 = CircuitElement::<CircuitInput<0>> {};
                    let in2 = CircuitElement::<CircuitInput<1>> {};
                    let add = circuit_add(in1, in2);
                    let inv = circuit_inverse(add);
                    let sub = circuit_sub(inv, in2);
                    let mul = circuit_mul(inv, sub);

                    let modulus = TryInto::<
                        _, CircuitModulus,
                    >::try_into([v3.limb0, v3.limb1, v3.limb2, v3.limb3])
                        .unwrap();
                    let outputs = (mul, add, inv)
                        .new_inputs()
                        .next(v3)
                        .next(v3)
                        .done()
                        .eval(modulus)
                        .unwrap();
                    outputs.get_output(add)
                },
                core::blake::blake2s_finalize(
                    core::blake::blake2s_compress(
                        BoxTrait::new([v4; 8]), 16, BoxTrait::new([v4; 16]),
                    ),
                    16,
                    BoxTrait::new([v4; 16]),
                )
                    .unbox(),
            )
        }
    }
}
