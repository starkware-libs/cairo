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
fn test_option_some_is_some() {
    assert!(Option::Some(42).is_some());
}

#[test]
fn test_option_none_is_some() {
    assert!(!Option::<felt252>::None.is_some());
}

#[test]
fn test_option_some_is_none() {
    assert!(!Option::Some(42).is_none());
}

#[test]
fn test_option_none_is_none() {
    assert!(Option::<felt252>::None.is_none());
}

#[derive(Drop)]
struct NonCopy {}

#[test]
fn test_default_for_option() {
    assert!(Default::<Option<felt252>>::default().is_none());
    assert!(Default::<Option<NonCopy>>::default().is_none());
}
