use array::ArrayTrait;
use array::SpanTrait;

#[test]
fn test() {
    let mut arr = ArrayTrait::new();

    arr.append(0x00_u8);
    arr.append(0x01_u8);
    arr.append(0x02_u8);
    arr.append(0x03_u8);
    arr.append(0x04_u8);
    arr.append(0x05_u8);
    arr.append(0x06_u8);
    arr.append(0x07_u8);
    arr.append(0x08_u8);
    arr.append(0x09_u8);
    assert(arr.len() == 10_usize, 'len() == 10');
    assert(arr.span().subspan(0_usize, 10_usize).len() == 10_usize, 'len() == 10');
    assert(arr.span().subspan(1_usize, 8_usize).len() == 7_usize, 'len() == 7');
}

