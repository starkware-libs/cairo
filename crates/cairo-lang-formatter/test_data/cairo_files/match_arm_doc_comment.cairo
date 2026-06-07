fn inner(x: felt252) -> felt252 {
    match x {
        0 => {
            //! inner comment
            1
        },
        _ => 2,
    }
}

fn doc(x: felt252) -> felt252 {
    match x {
        0 => {
            /// doc comment
            1
        },
        _ => 2,
    }
}
