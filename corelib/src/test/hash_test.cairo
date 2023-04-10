use array::ArrayTrait;

#[test]
fn test_pedersen_hash() {
    assert(
        hash::pedersen(
            1, 2
        ) == 2592987851775965742543459319508348457290966253241455514226127639100457844774,
        'Wrong hash value'
    );
}

#[test]
fn test_poseidon_hades_permutation() {
    let (s0, s1, s2) = poseidon::hades_permutation(1, 2, 3);
    assert(
        s0 == 442682200349489646213731521593476982257703159825582578145778919623645026501,
        'wrong s0'
    );
    assert(
        s1 == 2233832504250924383748553933071188903279928981104663696710686541536735838182,
        'wrong s1'
    );
    assert(
        s2 == 2512222140811166287287541003826449032093371832913959128171347018667852712082,
        'wrong s2'
    );
}

#[test]
#[available_gas(300000)]
fn test_poseidon_hash_span() {
    let mut input = ArrayTrait::new();
    input.append(1);
    input.append(2);
    input.append(3);

    // Test odd number of inputs.
    assert(
        poseidon::poseidon_hash_span(
            input.span()
        ) == 0x2f0d8840bcf3bc629598d8a6cc80cb7c0d9e52d93dab244bbf9cd0dca0ad082,
        'wrong result'
    );

    // Test even number of inputs.
    input.append(4);
    assert(
        poseidon::poseidon_hash_span(
            input.span()
        ) == 0x26e3ad8b876e02bc8a4fc43dad40a8f81a6384083cabffa190bcf40d512ae1d,
        'wrong result'
    );
}
