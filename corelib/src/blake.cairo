type Blake2sState = Box<[u32; 8]>;

type Msg = [u32; 16];

pub extern fn blake2s_compress(
    state: Blake2sState, byte_count: u32, msg: Box<Msg>,
) -> Blake2sState nopanic;
