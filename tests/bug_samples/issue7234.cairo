// A function which takes a closure as an argument and calls it.
// <F> denotes that F is a "Generic type parameter"
fn apply<F, +Drop<F>, impl func: core::ops::FnOnce<F, ()>, +Drop<func::Output>>(f: F) {
    // ^ TODO: Try changing this to `Fn`.

    f();
}
#[test]
fn main() {
    // A non-copy type.
    let greeting: ByteArray = "hello";
    let farewell: ByteArray = "goodbye";

    // Capture 2 variables: `greeting` by snapshot and
    // `farewell` by value.
    let diary = || {
        // `greeting` is by snapshot: requires `Fn`.
        let _ignore_greeting = format!("I said {greeting}.");

        // Using farewell by value requires `FnOnce`.
        // Convert farewell to uppercase to demonstrate value capture through `into_iter`
        let mut iter = farewell.into_iter();
        let uppercase: ByteArray = iter.map(|c| if c >= 'a' {
            c - 32
        } else {
            c
        }).collect();
        let _ignore_farewell = format!("Then I screamed {uppercase}!");
    };

    // Call the function which applies the closure.
    apply(diary);
}
