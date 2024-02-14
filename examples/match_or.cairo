use core::traits::Into;


#[derive(Copy, Drop)]
struct P {
    x: u8,
    y: u8,
    z: u8
}
#[derive(Copy, Drop)]
enum MyEnum {
    A: (felt252, felt252),
    B: (felt252, felt252),
    C: (u8, u8, u8),
    D: P,
}

fn main() {
    let a = MyEnum::A((1, 2));
    let b = MyEnum::B((1, 2));
    let c = MyEnum::C((1, 2, 3));
    let d = MyEnum::D(P { x: 1, y: 2, z: 9 });
    let _ = foo(a);
    let _ = foo(b);
    let _ = foo(c);
    let _ = foo(d);
}
fn foo(a: MyEnum) -> felt252 {
    match a {
        MyEnum::A((_, x)) | MyEnum::B((x, _)) => x,
        MyEnum::C((x, _, t)) | MyEnum::D(P { x, y: _, z: t }) => (x + t).into(),
    }
}
