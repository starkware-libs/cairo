// === Bits ===

trait Bits<T> {
    /// Returns the length in bits of T.
    fn len() -> usize;
    /// Returns the length in bits of self.
    fn bits(self: @T) -> usize;
}

impl U8Bits of Bits<u8> {
    fn len() -> usize {
        8
    }
    fn bits(self: @u8) -> usize {
        U8Bits::len()
    }
}

impl U16Bits of Bits<u16> {
    fn len() -> usize {
        16
    }
    fn bits(self: @u16) -> usize {
        U16Bits::len()
    }
}

impl U32Bits of Bits<u32> {
    fn len() -> usize {
        32
    }
    fn bits(self: @u32) -> usize {
        U32Bits::len()
    }
}

impl U64Bits of Bits<u64> {
    fn len() -> usize {
        64
    }
    fn bits(self: @u64) -> usize {
        U64Bits::len()
    }
}

impl U128Bits of Bits<u128> {
    fn len() -> usize {
        128
    }
    fn bits(self: @u128) -> usize {
        U128Bits::len()
    }
}

impl U256Bits of Bits<u256> {
    fn len() -> usize {
        256
    }
    fn bits(self: @u256) -> usize {
        U256Bits::len()
    }
}

impl I8Bits of Bits<i8> {
    fn len() -> usize {
        8
    }
    fn bits(self: @i8) -> usize {
        I8Bits::len()
    }
}

impl I16Bits of Bits<i16> {
    fn len() -> usize {
        16
    }
    fn bits(self: @i16) -> usize {
        I16Bits::len()
    }
}

impl I32Bits of Bits<i32> {
    fn len() -> usize {
        32
    }
    fn bits(self: @i32) -> usize {
        I32Bits::len()
    }
}

impl I64Bits of Bits<i64> {
    fn len() -> usize {
        64
    }
    fn bits(self: @i64) -> usize {
        I64Bits::len()
    }
}

impl I128Bits of Bits<i128> {
    fn len() -> usize {
        128
    }
    fn bits(self: @i128) -> usize {
        I128Bits::len()
    }
}

impl Bytes31Bits of Bits<bytes31> {
    fn len() -> usize {
        31
    }
    fn bits(self: @bytes31) -> usize {
        Bytes31Bits::len()
    }
}
