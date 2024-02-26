use starknet_types_core::felt::Felt;

pub const BYTE_ARRAY_MAGIC: Felt =
    Felt::from_hex_unchecked("46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3");
pub const BYTES_IN_WORD: usize = 31;
