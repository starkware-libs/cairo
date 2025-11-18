extern fn local_into_box<T>(value: T) -> Box<T> nopanic;

#[inline(never)]
pub fn into_box<T>(value: T) -> Box<T> {
    local_into_box(value)
}

#[derive(Copy, Drop)]
struct TestStruct {
    x: felt252,
    y: felt252,
    z: felt252,
}

#[test]
fn test_local_into_box() {
    let boxed = into_box(TestStruct { x: 1, y: 2, z: 3 });
    assert_eq!(boxed.x, 1);
    assert_eq!(boxed.y, 2);
    assert_eq!(boxed.z, 3);
}

#[derive(Copy, Drop, PartialEq, Debug)]
struct Empty {}

#[test]
fn test_empty_struct_into_box() {
    let empty = into_box(Empty {});
    assert_eq!(empty.unbox(), Empty {});
}

#[derive(Copy, Drop, PartialEq, Debug)]
enum TestEnum {
    A: (felt252, felt252),
    B: felt252,
}

#[test]
fn test_enum_local_into_box() {
    let boxed_a = into_box(TestEnum::A((1, 2)));
    assert_eq!(boxed_a.unbox(), TestEnum::A((1, 2)));

    let boxed_b = into_box(TestEnum::B(1));
    assert_eq!(boxed_b.unbox(), TestEnum::B(1));
}
