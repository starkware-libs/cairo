#[derive(Copy, Drop)]
struct GenericStruct<T, U> {
    x: T,
    y: U,
}

//#[derive(Drop)]
struct GenericStructConst<T, const U: felt252> {
    x: T, 
}

// This doesn't actually work, fails with 'unknown literal' on U
//impl GSCDrop<T, const U: felt252> of Destruct<GenericStructConst<T, U>> {
//    fn destruct(self: GenericStructConst<T, U>) nopanic {}
//}

#[test]
fn main() {
    let a = GenericStruct { x: 1, y: 2 };
//let b = GenericStructConst::<felt252, 245> { x: 1 };
}
