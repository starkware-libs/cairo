// === Representation ===

trait Representation<T> {
    /// Returns the representation in bits of T.
    fn bits() -> T;
    /// Returns the representation in bits of self.
    fn bits_repr(self: @T) -> T;
}

impl U8Representation of Representation<u8> {
    fn bits() -> u8 {
        8
    }
    fn bits_repr(self: @u8) -> u8 {
        U8Representation::bits()
    }
}

impl U16Representation of Representation<u16> {
    fn bits() -> u16 {
        16
    }
    fn bits_repr(self: @u16) -> u16 {
        U16Representation::bits()
    }
}

impl U32Representation of Representation<u32> {
    fn bits() -> u32 {
        32
    }
    fn bits_repr(self: @u32) -> u32 {
        U32Representation::bits()
    }
}

impl U64Representation of Representation<u64> {
    fn bits() -> u64 {
        64
    }
    fn bits_repr(self: @u64) -> u64 {
        U64Representation::bits()
    }
}

impl U128Representation of Representation<u128> {
    fn bits() -> u128 {
        128
    }
    fn bits_repr(self: @u128) -> u128 {
        U128Representation::bits()
    }
}

impl U256Representation of Representation<u256> {
    fn bits() -> u256 {
        256
    }
    fn bits_repr(self: @u256) -> u256 {
        U256Representation::bits()
    }
}

impl I8Representation of Representation<i8> {
    fn bits() -> i8 {
        8
    }
    fn bits_repr(self: @i8) -> i8 {
        I8Representation::bits()
    }
}

impl I16Representation of Representation<i16> {
    fn bits() -> i16 {
        16
    }
    fn bits_repr(self: @i16) -> i16 {
        I16Representation::bits()
    }
}

impl I32Representation of Representation<i32> {
    fn bits() -> i32 {
        32
    }
    fn bits_repr(self: @i32) -> i32 {
        I32Representation::bits()
    }
}

impl I64Representation of Representation<i64> {
    fn bits() -> i64 {
        64
    }
    fn bits_repr(self: @i64) -> i64 {
        I64Representation::bits()
    }
}

impl I128Representation of Representation<i128> {
    fn bits() -> i128 {
        128
    }
    fn bits_repr(self: @i128) -> i128 {
        I128Representation::bits()
    }
}

impl Felt252Representation of Representation<felt252> {
    fn bits() -> felt252 {
        252
    }
    fn bits_repr(self: @felt252) -> felt252 {
        Felt252Representation::bits()
    }
}
