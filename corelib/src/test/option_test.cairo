#[test]
fn test_option_some_expect() {
    assert!(Option::Some(42).expect('') == 42);
}

#[test]
#[should_panic(expected: ('err msg',))]
fn test_option_none_expect() {
    Option::<felt252>::None.expect('err msg');
}

#[test]
fn test_option_some_unwrap() {
    assert!(Option::Some(42).unwrap() == 42);
}

#[test]
#[should_panic(expected: ('Option::unwrap failed.',))]
fn test_option_none_unwrap() {
    Option::<felt252>::None.unwrap();
}

#[test]
fn test_option_some_unwrap_or() {
    assert!(Option::Some(42).unwrap_or(0) == 42);
}

#[test]
fn test_option_none_unwrap_or() {
    assert!(Option::None.unwrap_or(0) == 0);
}

#[test]
fn test_option_some_unwrap_or_default() {
    assert!(Option::Some(42).unwrap_or_default() == 42);
}

#[test]
fn test_option_none_unwrap_or_default() {
    assert!(Option::None.unwrap_or_default() == 0);
}

#[test]
fn test_option_some_unwrap_or_else() {
    assert!(Option::Some(42).unwrap_or_else( || 0) == 42);
}

#[test]
fn test_option_none_unwrap_or_else() {
    assert!(Option::None.unwrap_or_else( || 0) == 0);
}

#[test]
fn test_option_some_is_some() {
    assert!(Option::Some(42).is_some());
}

#[test]
fn test_option_none_is_some() {
    assert!(!Option::<felt252>::None.is_some());
}

#[test]
fn test_option_some_is_some_and() {
    assert_eq!(Option::Some(2_u8).is_some_and(|x| x > 1), true);
    assert_eq!(Option::Some(0_u8).is_some_and(|x| x > 1), false);
}

#[test]
fn test_option_none_is_some_and() {
    let option: Option<u8> = Option::None;
    assert_eq!(option.is_some_and(|x| x > 1), false);
}

#[test]
fn test_option_some_is_none() {
    assert!(!Option::Some(42).is_none());
}

#[test]
fn test_option_none_is_none() {
    assert!(Option::<felt252>::None.is_none());
}

#[test]
fn test_option_some_ok_or() {
    assert_eq!(Option::Some('foo').ok_or(0), Result::Ok('foo'));
}

#[test]
fn test_option_none_ok_or() {
    let option: Option<felt252> = Option::None;
    assert_eq!(option.ok_or(0), Result::Err(0));
}

#[test]
fn test_option_some_ok_or_else() {
    assert_eq!(Option::Some('foo').ok_or_else( || 0), Result::Ok('foo'));
}

#[test]
fn test_option_none_ok_or_else() {
    let option: Option<felt252> = Option::None;
    assert_eq!(option.ok_or_else( || 0), Result::Err(0));
}

fn test_option_some_is_none_or() {
    assert_eq!(Option::Some(2_u8).is_none_or(|x| x > 1), true);
    assert_eq!(Option::Some(0_u8).is_none_or(|x| x > 1), false);
}

#[test]
fn test_option_none_is_none_or() {
    let option: Option<u8> = Option::None;
    assert_eq!(option.is_none_or(|x| x > 1), true);
}

#[derive(Drop)]
struct NonCopy {}

#[test]
fn test_default_for_option() {
    assert!(Default::<Option<felt252>>::default().is_none());
    assert!(Default::<Option<NonCopy>>::default().is_none());
}

#[test]
fn test_option_some_map() {
    let maybe_some_string: Option<ByteArray> = Option::Some("Hello, World!");
    let maybe_some_len = maybe_some_string.map(|s: ByteArray| s.len());
    assert!(maybe_some_len == Option::Some(13));
}

#[test]
fn test_option_none_map() {
    let x: Option<ByteArray> = Option::None;
    assert!(x.map(|s: ByteArray| s.len()) == Option::None);
}
