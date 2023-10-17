// === Bits ===

trait Bits<T> {
    /// Returns the bitsgth in bits of T.
    fn bits() -> usize;
}

impl U8Bits of Bits<u8> {
    fn bits() -> usize {
        8
    }
}

impl U16Bits of Bits<u16> {
    fn bits() -> usize {
        16
    }
}

impl U32Bits of Bits<u32> {
    fn bits() -> usize {
        32
    }
}

impl U64Bits of Bits<u64> {
    fn bits() -> usize {
        64
    }
}

impl U128Bits of Bits<u128> {
    fn bits() -> usize {
        128
    }
}

impl U256Bits of Bits<u256> {
    fn bits() -> usize {
        256
    }
}

impl I8Bits of Bits<i8> {
    fn bits() -> usize {
        8
    }
}

impl I16Bits of Bits<i16> {
    fn bits() -> usize {
        16
    }
}

impl I32Bits of Bits<i32> {
    fn bits() -> usize {
        32
    }
}

impl I64Bits of Bits<i64> {
    fn bits() -> usize {
        64
    }
}

impl I128Bits of Bits<i128> {
    fn bits() -> usize {
        128
    }
}

impl Bytes31Bits of Bits<bytes31> {
    fn bits() -> usize {
        248
    }
}
