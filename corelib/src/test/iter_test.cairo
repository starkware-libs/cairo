use crate::iter::{IntoIterator, Iterator};

#[test]
fn test_iter_adapter_map() {
    let mut iter = (1..3_u8).into_iter().map(|x: u8| 2 * x);

    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(4));
    assert_eq!(iter.next(), Option::Some(6));
    assert_eq!(iter.next(), Option::None);
}
