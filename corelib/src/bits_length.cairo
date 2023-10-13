// === BitsLength ===

trait BitsLength<T> {
    /// Returns the BitsLength in bits of T.
    fn bits() -> T;
    /// Returns the BitsLength in bits of self.
    fn bits_len(self: @T) -> T;
}

impl U8BitsLength of BitsLength<u8> {
    fn bits() -> u8 {
        8
    }
    fn bits_len(self: @u8) -> u8 {
        U8BitsLength::bits()
    }
}

impl U16BitsLength of BitsLength<u16> {
    fn bits() -> u16 {
        16
    }
    fn bits_len(self: @u16) -> u16 {
        U16BitsLength::bits()
    }
}

impl U32BitsLength of BitsLength<u32> {
    fn bits() -> u32 {
        32
    }
    fn bits_len(self: @u32) -> u32 {
        U32BitsLength::bits()
    }
}

impl U64BitsLength of BitsLength<u64> {
    fn bits() -> u64 {
        64
    }
    fn bits_len(self: @u64) -> u64 {
        U64BitsLength::bits()
    }
}

impl U128BitsLength of BitsLength<u128> {
    fn bits() -> u128 {
        128
    }
    fn bits_len(self: @u128) -> u128 {
        U128BitsLength::bits()
    }
}

impl U256BitsLength of BitsLength<u256> {
    fn bits() -> u256 {
        256
    }
    fn bits_len(self: @u256) -> u256 {
        U256BitsLength::bits()
    }
}

impl I8BitsLength of BitsLength<i8> {
    fn bits() -> i8 {
        8
    }
    fn bits_len(self: @i8) -> i8 {
        I8BitsLength::bits()
    }
}

impl I16BitsLength of BitsLength<i16> {
    fn bits() -> i16 {
        16
    }
    fn bits_len(self: @i16) -> i16 {
        I16BitsLength::bits()
    }
}

impl I32BitsLength of BitsLength<i32> {
    fn bits() -> i32 {
        32
    }
    fn bits_len(self: @i32) -> i32 {
        I32BitsLength::bits()
    }
}

impl I64BitsLength of BitsLength<i64> {
    fn bits() -> i64 {
        64
    }
    fn bits_len(self: @i64) -> i64 {
        I64BitsLength::bits()
    }
}

impl I128BitsLength of BitsLength<i128> {
    fn bits() -> i128 {
        128
    }
    fn bits_len(self: @i128) -> i128 {
        I128BitsLength::bits()
    }
}

impl Felt252BitsLength of BitsLength<felt252> {
    fn bits() -> felt252 {
        252
    }
    fn bits_len(self: @felt252) -> felt252 {
        Felt252BitsLength::bits()
    }
}

impl Bytes31BitsLength of BitsLength<bytes31> {
    fn bits() -> bytes31 {
        31_usize.into()
    }
    fn bits_len(self: @bytes31) -> bytes31 {
        Bytes31BitsLength::bits()
    }
}
