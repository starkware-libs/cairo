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
        [a, b, c] => { assert_eq!((*a, *b, *c), (10, 20, 30)); },
        _ => panic!("Expected 3 elements, but got a different pattern"),
    }
}

#[test]
fn test_match_span_empty_pattern() {
    let span: Span<u32> = array![].span();

    match span {
        [_a] => { panic!("Expected 0 elements, but got 1"); },
        [] => {},
        _ => panic!("Expected 0 elements, but got a different count"),
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


#[test]
fn test_match_span_inner_pattern_mismatch() {
    let matcher = |s: Array<Option<felt252>>| match s.span() {
        [Some(_)] => 1,
        [None] => 2,
        _ => 0,
    };

    assert_eq!(matcher(array![Some(42)]), 1);
    assert_eq!(matcher(array![None]), 2);
    assert_eq!(matcher(array![Some(1), Some(2)]), 0);
}

#[test]
fn test_match_span_fsa_with_struct_catch_all() {
    let matcher = |s: Span<u32>| match s {
        [a] => *a,
        Span { snapshot: inner } => inner.len(),
    };

    assert_eq!(matcher(array![42].span()), 42);
    assert_eq!(matcher(array![].span()), 0);
    assert_eq!(matcher(array![1, 2, 3].span()), 3);
}

#[test]
fn test_match_span_fsa_struct_between_sizes() {
    let matcher = |s: Span<u32>, val: u32| match (s, val) {
        ([a], v) => *a + v,
        (Span { snapshot: inner }, 5) => 2 * inner.len(),
        ([a, b], v) => *a + *b + v,
        ([a, b, c], v) => *a + *b + *c + v,
        _ => 0,
    };

    // `[a]` arm wins for size 1 regardless of `val`.
    assert_eq!(matcher(array![10].span(), 7), 17);
    assert_eq!(matcher(array![10].span(), 5), 15);

    // Struct arm wins when `val == 5` and size != 1 (covers sizes outside `[a, b]` / `[a, b, c]`
    // too).
    assert_eq!(matcher(array![].span(), 5), 0);
    assert_eq!(matcher(array![1, 2].span(), 5), 4);
    assert_eq!(matcher(array![1, 2, 3].span(), 5), 6);
    assert_eq!(matcher(array![1, 2, 3, 4].span(), 5), 8);

    // `[a, b]` and `[a, b, c]` arms win for their sizes when `val != 5`.
    assert_eq!(matcher(array![10, 20].span(), 7), 37);
    assert_eq!(matcher(array![10, 20, 30].span(), 7), 67);

    // Wildcard arm — `val != 5` and size not in {1, 2, 3}.
    assert_eq!(matcher(array![].span(), 7), 0);
    assert_eq!(matcher(array![1, 2, 3, 4].span(), 7), 0);
}
