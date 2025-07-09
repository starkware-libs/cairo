#[derive(Drop, Copy)]
enum Inner {
    V1,
    V2,
}

#[derive(Drop, Copy)]
struct Struct {
    inner: Inner,
    rest: felt252,
}

#[derive(Drop, Copy)]
enum Outer {
    V1,
    V2: (Inner, felt252),
}

#[inline(never)]
fn revoke(input: felt252) {
    core::internal::revoke_ap_tracking();
}

#[test]
fn const_with_snap() {
    const outer: Outer = Outer::V1;
    let mut x = 1;
    match @outer {
        Outer::V1 => { x += 1; },
        Outer::V2((
            inner, y,
        )) => {
            x += 2;
            match inner {
                Inner::V1 => { x += 3; },
                Inner::V2 => { x += 4; },
            }
            x += *y;
        },
    }

    revoke(x);
}

