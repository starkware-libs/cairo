use crate::iter::{IntoIterator, Iterator};

#[test]
fn test_iter_adapter_map() {
    let mut i = 1;
    for elem in (1..4_u8).into_iter().map(|x| 2 * x).map(|x| x + 1) {
        assert_eq!(elem, i * 2 + 1);
        i += 1;
    }
}
