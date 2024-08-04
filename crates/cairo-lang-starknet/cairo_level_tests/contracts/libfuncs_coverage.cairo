#[starknet::contract]
mod libfuncs_coverage {
    #[storage]
    struct Storage {}

    #[abi(per_item)]
    #[generate_trait]
    impl Impl of Trait {
        #[external(v0)]
        fn entry_point(ref self: ContractState) {
            super::all_libfuncs(super::Libfuncs::None)
        }
    }
}

enum Libfuncs {
    None,
    U8: IntLibfuncs<u8>,
    U16: IntLibfuncs<u16>,
    U32: IntLibfuncs<u32>,
    U64: IntLibfuncs<u64>,
    U128: IntLibfuncs<u128>,
    I8: IntLibfuncs<i8>,
    I16: IntLibfuncs<i16>,
    I32: IntLibfuncs<i32>,
    I64: IntLibfuncs<i64>,
    I128: IntLibfuncs<i128>,
    F252: IntLibfuncs<felt252>,
}

enum IntLibfuncs<T> {
    Add: (T, T),
    Sub: (T, T),
}

#[inline(never)]
fn all_libfuncs(libfuncs: Libfuncs) {
    match libfuncs {
        Libfuncs::None => {},
        Libfuncs::U8(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::U16(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::U32(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::U64(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::U128(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I8(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I16(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I32(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I64(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I128(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::F252(libfuncs) => int_libfuncs(libfuncs),
    }
}

fn int_libfuncs<T, +Add<T>, +Sub<T>, +PanicDestruct<T>>(libfuncs: IntLibfuncs<T>) {
    match libfuncs {
        IntLibfuncs::Add((a, b)) => use_and_panic(a + b),
        IntLibfuncs::Sub((a, b)) => use_and_panic(a - b),
    }
}

fn use_and_panic<T, +PanicDestruct<T>>(t: T) {
    panic!();
}
