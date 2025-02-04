#[inline(never)]
fn foo(mut x: i8, mut y: i8) -> i8 {
    loop {
        match x {
            0 => {
                // A loop before continue - bug would cause the continue to call this loop.
                while y < 100 {
                    y += 1;
                }
                x += 1;
            },
            1 => {
                x -= 2;
                continue;
            },
            _ => { x -= 2; },
        }
        if x < 0 {
            break y;
        }
    }
}

#[test]
fn loop_of_loop() -> i8 {
    foo(10, 10)
}
