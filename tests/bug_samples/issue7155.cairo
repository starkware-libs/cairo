#[test]
fn test_enumerate_type_resolution() {
    // TODO(orizi): Remove "_usize" from the next line. Currently doesn't work without it, due to
    // `NumericLiteral` not auto supplying `Destruct` to a type implementing it.
    for (k, v) in array![0, 1, 2_usize].into_iter().enumerate() {
        assert_eq!(k, v);
    }

    let mut iter = array![0, 1, 2].into_iter().enumerate();
    while let Option::Some((k, v)) = iter.next() {
        assert_eq!(k, v);
    }
}
