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
    U8: UnsingedIntLibfuncs<u8>,
    U16: UnsingedIntLibfuncs<u16>,
    U32: UnsingedIntLibfuncs<u32>,
    U64: UnsingedIntLibfuncs<u64>,
    U128: UnsingedIntLibfuncs<u128>,
    I8: IntLibfuncs<i8>,
    I16: IntLibfuncs<i16>,
    I32: IntLibfuncs<i32>,
    I64: IntLibfuncs<i64>,
    I128: IntLibfuncs<i128>,
    F252: FeltLibfuncs<felt252>,
}

enum UnsingedIntLibfuncs<T> {
    Add: (T, T),
    Sub: (T, T),
    Mul: (T, T),
    Div: (T, T),
    Mod: (T, T),
    Equal: (T, T),
}

enum IntLibfuncs<T> {
    Add: (T, T),
    Sub: (T, T),
    Mul: (T, T),
    Equal: (T, T),
    IsZero: T,
}

enum FeltLibfuncs<T> {
    Add: (T, T),
    Sub: (T, T),
    Mul: (T, T),
    Equal: (T, T),
}

#[inline(never)]
fn all_libfuncs(libfuncs: Libfuncs) {
    match libfuncs {
        Libfuncs::None => {},
        Libfuncs::U8(libfuncs) => unsigned_int_libfuncs(libfuncs),
        Libfuncs::U16(libfuncs) => unsigned_int_libfuncs(libfuncs),
        Libfuncs::U32(libfuncs) => unsigned_int_libfuncs(libfuncs),
        Libfuncs::U64(libfuncs) => unsigned_int_libfuncs(libfuncs),
        Libfuncs::U128(libfuncs) => unsigned_int_libfuncs(libfuncs),
        Libfuncs::I8(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I16(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I32(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I64(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I128(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::F252(libfuncs) => felt_libfuncs(libfuncs),
    }
}

fn unsigned_int_libfuncs<
    T,
    +Add<T>,
    +Sub<T>,
    +Mul<T>,
    +DivRem<T>,
    +Div<T>,
    +Rem<T>,
    +core::num::traits::Sqrt<T>,
    +core::traits::PartialEq<T>,
    impl D: Drop<T>,
    impl PD: PanicDestruct<T>
>(
    libfuncs: UnsingedIntLibfuncs<T>
) {
    match libfuncs {
        UnsingedIntLibfuncs::Add((a, b)) => use_and_panic::<T, PD>(a + b),
        UnsingedIntLibfuncs::Sub((a, b)) => use_and_panic::<T, PD>(a - b),
        UnsingedIntLibfuncs::Mul((a, b)) => use_and_panic::<T, PD>(a * b),
        UnsingedIntLibfuncs::Div((a, b)) => use_and_panic::<T, PD>(a / b),
        UnsingedIntLibfuncs::Mod((a, b)) => use_and_panic::<T, PD>(a % b),
        UnsingedIntLibfuncs::Equal((a, b)) => use_and_panic(a == b),
    }
}

fn int_libfuncs<
    T,
    +Add<T>,
    +Sub<T>,
    +Mul<T>,
    +DivRem<T>,
    +core::traits::PartialEq<T>,
    impl D: Drop<T>,
    impl PD: PanicDestruct<T>
>(
    libfuncs: IntLibfuncs<T>
) {
    match libfuncs {
        IntLibfuncs::Add((a, b)) => use_and_panic::<T, PD>(a + b),
        IntLibfuncs::Sub((a, b)) => use_and_panic::<T, PD>(a - b),
        IntLibfuncs::Mul((a, b)) => use_and_panic::<T, PD>(a * b),
        IntLibfuncs::Equal((a, b)) => use_and_panic(a == b),
        _ => panic!(),
    }
}

fn felt_libfuncs<
    T,
    +Add<T>,
    +Sub<T>,
    +Mul<T>,
    +core::traits::PartialEq<T>,
    impl D: Drop<T>,
    impl PD: PanicDestruct<T>
>(
    libfuncs: FeltLibfuncs<T>
) {
    match libfuncs {
        FeltLibfuncs::Add((a, b)) => use_and_panic::<T, PD>(a + b),
        FeltLibfuncs::Sub((a, b)) => use_and_panic::<T, PD>(a - b),
        FeltLibfuncs::Mul((a, b)) => use_and_panic::<T, PD>(a * b),
        FeltLibfuncs::Equal((a, b)) => use_and_panic(a == b),
    }
}

fn use_and_panic<T, +PanicDestruct<T>>(t: T) {
    panic!();
}
