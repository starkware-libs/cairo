/// State for Blake2s hash.
type Blake2sState = Box<[u32; 8]>;

/// The input to the Blake2s compress function.
type Blake2sInput = Box<[u32; 16]>;


/// The blake2s compress function, which takes a state, a byte count, and a message, and returns a
/// new state.
/// `byte_count` should be the total number of bytes hashed after hashing the current `msg`.
pub extern fn blake2s_compress(
    state: Blake2sState, byte_count: u32, msg: Blake2sInput,
) -> Blake2sState nopanic;


/// A variant of `blake2s_compress` for the final block of the message.
///
/// The input `msg` must always be exactly 16 `u32` elements, padded with zeros if necessary,
/// regardless of the value of `byte_count`. Using any padding scheme other than zero-padding
/// will produce a different hash output.
pub extern fn blake2s_finalize(
    state: Blake2sState, byte_count: u32, msg: Blake2sInput,
) -> Blake2sState nopanic;
