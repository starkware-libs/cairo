fn foo() {
    let x =
        // Own-line comment after `=`.
        bar();
    let y = // Trailing comment after `=`.
        bar();
    let mut z = 1;
    z += // Trailing comment after `+=`.
        bar();
}
