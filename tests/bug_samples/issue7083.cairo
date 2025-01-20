fn main() {
    let zero: ByteArray = "0";

    let format_string = |acc: ByteArray, x: u8| {
        format!("({acc} + {x})")
    };

    let result = format_string(zero, 1);
    assert_eq!(result, "(0 + 1)");
}
