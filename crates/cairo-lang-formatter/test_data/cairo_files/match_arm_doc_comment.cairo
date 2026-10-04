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

fn comment_after_statement(x: felt252) -> felt252 {
    match x {
        0 => {
            1
            // comment after the statement
        },
        _ => 2,
    }
}
