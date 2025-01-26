type Blake2sState = Box<[u32; 8]>;

type Blake2sInput = Box<[u32; 16]>;

pub extern fn blake2s_compress(
    state: Blake2sState, byte_count: u32, msg: Blake2sInput,
) -> Blake2sState nopanic;
