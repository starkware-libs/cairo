/// State for Blake2s hash.
type Blake2sState = Box<[u32; 8]>;

/// The input to the Blake2s compress function.
type Blake2sInput = Box<[u32; 16]>;


/// The blake2s compress function, which takes a state, a byte count, and a message, and returns a
/// new state.
/// `byte_count` should be the total number of bytes hashed after hashing the current `msg`.
///
/// Message packing specification:
/// Each `msg[i]` encodes bytes `block[4*i..4*i+4]` as a little endian `u32`.
/// The returned `Blake2sState` words are the encoded state in little endian representation.
pub extern fn blake2s_compress(
    state: Blake2sState, byte_count: u32, msg: Blake2sInput,
) -> Blake2sState nopanic;


/// A variant of `blake2s_compress` for the final block of the message.
///
/// The input `msg` must always be exactly 16 `u32` elements, padded with zeros if necessary,
/// regardless of the value of `byte_count`. Using any padding scheme other than zero-padding
/// will produce a different hash output.
///
/// Message packing specification:
/// Each `msg[i]` encodes bytes `block[4*i..4*i+4]` as a little endian `u32`.
/// Unused bytes in that word must be zero.
/// The returned `Blake2sState` words are the encoded state in little endian representation.
pub extern fn blake2s_finalize(
    state: Blake2sState, byte_count: u32, msg: Blake2sInput,
) -> Blake2sState nopanic;
