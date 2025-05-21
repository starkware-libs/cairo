use core::box::BoxImpl;
use core::option::Option;
use core::option::Option::{None, Some};

#[test]
fn test_match_multienum_binding() {
    let i = Some(@2);
    let x = Some(@i);
    match @@x {
        Some(Some(x)) => {
            assert_eq!(****x, 2);
            return ();
        },
        _ => (),
    }
    assert(false, 'Expected match to succeed');
}

#[test]
fn test_match_extern_multilevel() {
    let x: Option<Option<Option<felt252>>> = Some(Some(None));
    let b = BoxImpl::new(x);
    match b.unbox() {
        Some(Some(None)) => { return (); },
        _ => (),
    }
    assert(false, 'Expected match to succeed');
}
