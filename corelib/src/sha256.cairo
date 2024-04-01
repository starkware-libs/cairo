use core::starknet::SyscallResultTrait;

/// A handle to the state of a SHA-256 hash.
extern type SHA256StateHandle;

impl DropSHA256StateHandle of Drop<SHA256StateHandle>;
impl CopySHA256StateHandle of Copy<SHA256StateHandle>;

/// Initializes a new SHA-256 state handle.
pub extern fn sha256_state_handle_init(state: Box<[u32; 8]>) -> SHA256StateHandle nopanic;

/// returns the state of a SHA-256 hash.
pub extern fn sha256_state_handle_digest(state: SHA256StateHandle) -> Box<[u32; 8]> nopanic;

const SHA256_INITIAL_STATE: [
    u32
    ; 8] = [
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
];

/// Appends `max(n,8)` zeros to the array and updates n.
#[inline(always)]
fn append_zero(ref arr: Array<u32>, ref n: felt252) {
    match n {
        0 => {},
        1 => {
            arr.append(0);
            n = n - 1;
        },
        2 => {
            arr.append(0);
            arr.append(0);
            n = n - 2;
        },
        3 => {
            arr.append(0);
            arr.append(0);
            arr.append(0);
            n = n - 3;
        },
        4 => {
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            n = n - 4;
        },
        5 => {
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            n = n - 5;
        },
        6 => {
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            n = n - 6;
        },
        7 => {
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            n = n - 7;
        },
        _ => {
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            arr.append(0);
            n = n - 8;
        },
    }
}

/// Adds padding to the input array for SHA-256. The padding is defined as follows:
/// 1. Append a single bit with value 1 to the end of the array.
/// 2. Append zeros until the length of the array is 448 mod 512.
/// 3. Append the length of the array in bits as a 64-bit number.
/// use last_input_word when the number of bytes in the last input word is less than 4.
fn add_sha256_padding(ref arr: Array<u32>, last_input_word: u32, last_input_num_bytes: u32) {
    let len = arr.len();
    if last_input_num_bytes == 0 {
        arr.append(0x80000000);
    } else {
        let (q, m, pad) = if last_input_num_bytes == 1 {
            (0x100, 0x1000000, 0x800000)
        } else if last_input_num_bytes == 2 {
            (0x10000, 0x10000, 0x8000)
        } else {
            (0x1000000, 0x100, 0x80)
        };
        let (_, r) = core::integer::u32_safe_divmod(last_input_word, q);
        arr.append(r * m + pad);
    }

    let mut remaining: felt252 = 16 - ((arr.len() + 1) % 16).into();

    append_zero(ref arr, ref remaining);
    append_zero(ref arr, ref remaining);

    arr.append(len * 32 + last_input_num_bytes * 8);
}

/// Computes the SHA-256 hash of the input array.
/// use last_input_word when the number of bytes in the last input word is less than 4.
pub fn sha256_cairo(
    ref input: Array<u32>, last_input_word: u32, last_input_num_bytes: u32
    ) -> [
    u32
; 8] {
    add_sha256_padding(ref input, last_input_word, last_input_num_bytes);

    let arr = input.span();
    let mut state = sha256_state_handle_init(BoxTrait::new(SHA256_INITIAL_STATE));
    let mut ind = 0;

    while ind != arr.len() {
        let input: Span<u32> = arr.slice(ind, 16);
        state = starknet::syscalls::sha256_chunk_syscall(state, input).unwrap_syscall();
        ind = ind + 16;
    };

    sha256_state_handle_digest(state).unbox()
}

/// Computes the SHA-256 hash of the input ByteArray.
pub fn sha256_byte_array(arr: @ByteArray) -> [u32; 8] {
    let mut word_arr = array![];
    let len = arr.len();
    let rem = len % 4;
    let mut index = 0;
    let rounded_len = len - rem;
    while index != rounded_len {
        let word = arr.at(index + 3).unwrap().into()
            + arr.at(index + 2).unwrap().into() * 0x100
            + arr.at(index + 1).unwrap().into() * 0x10000
            + arr.at(index).unwrap().into() * 0x1000000;
        word_arr.append(word);
        index = index + 4;
    };
    let last = match rem {
        0 => 0,
        1 => arr.at(len - 1).unwrap().into(),
        2 => arr.at(len - 1).unwrap().into() + arr.at(len - 2).unwrap().into() * 0x100,
        _ => arr.at(len - 1).unwrap().into()
            + arr.at(len - 2).unwrap().into() * 0x100
            + arr.at(len - 3).unwrap().into() * 0x10000,
    };
    sha256_cairo(ref word_arr, last, (len % 4).into())
}
