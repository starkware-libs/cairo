#[test]
#[available_gas(static)]
#[should_panic(expected: 'Out of gas')]
fn test_never_loop() {
    loop {
        1 + 1;
    }
}

#[test]
#[available_gas(static)]
#[should_panic(expected: 'Out of gas')]
fn test_never_rec() {
    never_rec()
}

fn never_rec() -> core::never {
    never_rec()
}
