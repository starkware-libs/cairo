use core::test::test_utils::assert_ne;

#[test]
fn test_bool_operators() {
    assert_eq!(true, true);
    assert_eq!(false, false);
    assert_eq!(!true, false);
    assert_eq!(!false, true);
    assert_ne(@true, @false, 't == f');
    assert_ne(@false, @true, 'f == t');
    assert!(!(false & false));
    assert!(!(true & false));
    assert!(!(false & true));
    assert!(true & true);
    assert!(!(false | false));
    assert!(true | false);
    assert!(false | true);
    assert!(true | true);
    assert!(!(false ^ false));
    assert!(true ^ false);
    assert!(false ^ true);
    assert!(!(true ^ true));
}

#[test]
fn test_bool_conversion() {
    assert_eq!(false.into(), 0);
    assert_eq!(true.into(), 1);
}
