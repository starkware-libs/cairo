// A macro capture is spliced into the expansion as text, so an `expr` capture is parenthesized:
// without that, the operators the rule wrote around the placeholder bind into the value, and
// `0 - $x` on `1 + 2` is `0 - 1 + 2`, which is `1`. `rustc` prints `-3` for the equivalent macro
// and call.
macro neg {
    ($x:expr) => { 0 - $x };
}

// Both captures are protected, so the `*` of the rule binds the two captured sums rather than
// their nearest operands. `rustc` prints `12` for the equivalent macro and call.
macro mul {
    ($a:expr, $b:expr) => { $a * $b };
}

// Passes its capture on to a rule that captures it back, parentheses and all - the second splice
// adds nothing, as a parenthesized expression is an atom. The call site names inside it still have
// to resolve, which is what the code mapping of the wrapping covering its parentheses buys.
macro pass {
    ($x:expr, $y:expr) => { ($x, $y) };

    ($x:expr) => { $defsite::pass!($x, 2) };
}

#[test]
fn test_expr_capture_is_grouped() {
    // Locals rather than literals, so that resolving the capture has to go through the code
    // mapping of the parentheses that now wrap it.
    let a = 1;
    let b = 2;
    assert!(neg!(a + b) == -3);
    assert!(mul!(a + b, b + 2) == 12);
    assert!(pass!(a + b) == (3, 2));
}
