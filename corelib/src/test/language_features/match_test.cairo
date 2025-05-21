#[test]
fn test_match_multienum_binding() {
    if true {
        match @@Some(@Some(@2)) {
            Some(Some(x)) => {
                assert_eq!(x, @@@@2);
                return ();
            },
            _ => panic!("Unexpected match arm"),
        }
    }
    assert(false, 'Match did not catch all paths');
}

#[test]
fn test_match_extern_multilevel() {
    if true {
        let x: Option<Option<Option<felt252>>> = Some(Some(None));
        let b = BoxTrait::new(x);
        match b.unbox() {
            Some(Some(None)) => { return (); },
            _ => panic!("Unexpected match arm"),
        }
    }
    assert(false, 'Match did not catch all paths');
}
