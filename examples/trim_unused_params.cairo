/// Runs a loop that always overwrites `c` before reading it, so the initial value of `c` is an
/// unused parameter of the generated loop function, trimmed by the `TrimUnusedParams` phase.
fn overwritten_in_loop(mut n: u32) -> felt252 {
    let mut c = 0;
    loop {
        c = 17;
        if n == 0 {
            break;
        }
        n -= 1;
    }
    c
}
