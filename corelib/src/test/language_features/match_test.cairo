#[test]
fn test_match_multienum_binding() {
    if true {
        match @@Some(@Some(@2)) {
            Some(Some(x)) => {
                assert_eq!(x, @@@@2);
                return;
            },
            _ => panic!("Expected Some(Some(2)), but got a different pattern"),
        }
    }
    panic!("Match expression did not return - this should be unreachable");
}

#[test]
fn test_match_span_to_fixed_size_array() {
    let span: Span<u32> = array![10, 20, 30].span();

    match span {
        [_a, _b, _c, _d] => { panic!("Expected 3 elements, but got 4"); },
        [_a, _b] => { panic!("Expected 3 elements, but got 2"); },
        [a, b, c] => {
            assert_eq!(a, @10);
            assert_eq!(b, @20);
            assert_eq!(c, @30);
        },
        _ => panic!("Expected 3 elements, but got a different pattern"),
    }
}

#[test]
fn test_match_extern_multilevel() {
    if true {
        let x: Option<Option<Option<felt252>>> = Some(Some(None));
        let b = BoxTrait::new(x);
        match b.unbox() {
            Some(Some(None)) => { return; },
            _ => panic!("Expected Some(Some(None)), but got a different pattern"),
        }
    }
    panic!("Match expression did not return - this should be unreachable");
}
