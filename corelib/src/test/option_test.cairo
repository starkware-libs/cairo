#[test]
fn test_option_some_expect() {
    assert!(Some(42).expect('') == 42);
}

#[test]
#[should_panic(expected: ('err msg',))]
fn test_option_none_expect() {
    Option::<felt252>::None.expect('err msg');
}

#[test]
fn test_option_some_unwrap() {
    assert!(Some(42).unwrap() == 42);
}

#[test]
#[should_panic(expected: ('Option::unwrap failed.',))]
fn test_option_none_unwrap() {
    Option::<felt252>::None.unwrap();
}

#[test]
fn test_option_some_unwrap_or() {
    assert!(Some(42).unwrap_or(0) == 42);
}

#[test]
fn test_option_none_unwrap_or() {
    assert!(None.unwrap_or(0) == 0);
}

#[test]
fn test_option_some_unwrap_or_default() {
    assert!(Some(42).unwrap_or_default() == 42);
}

#[test]
fn test_option_none_unwrap_or_default() {
    assert!(None.unwrap_or_default() == 0);
}

#[test]
fn test_option_some_unwrap_or_else() {
    assert!(Some(42).unwrap_or_else(|| 0) == 42);
}

#[test]
fn test_option_none_unwrap_or_else() {
    assert!(None.unwrap_or_else(|| 0) == 0);
}

#[test]
fn test_option_some_is_some() {
    assert!(Some(42).is_some());
}

#[test]
fn test_option_none_is_some() {
    assert!(!Option::<felt252>::None.is_some());
}

#[test]
fn test_option_some_is_some_and() {
    assert_eq!(Some(2_u8).is_some_and(|x| x > 1), true);
    assert_eq!(Some(0_u8).is_some_and(|x| x > 1), false);
}

#[test]
fn test_option_none_is_some_and() {
    let option: Option<u8> = None;
    assert_eq!(option.is_some_and(|x| x > 1), false);
}

#[test]
fn test_option_some_is_none() {
    assert!(!Some(42).is_none());
}

#[test]
fn test_option_none_is_none() {
    assert!(Option::<felt252>::None.is_none());
}

#[test]
fn test_option_some_ok_or() {
    assert_eq!(Some('foo').ok_or(0), Ok('foo'));
}

#[test]
fn test_option_none_ok_or() {
    let option: Option<felt252> = None;
    assert_eq!(option.ok_or(0), Err(0));
}

#[test]
fn test_option_some_ok_or_else() {
    assert_eq!(Some('foo').ok_or_else(|| 0), Ok('foo'));
}

#[test]
fn test_option_none_ok_or_else() {
    let option: Option<felt252> = None;
    assert_eq!(option.ok_or_else(|| 0), Err(0));
}

#[test]
fn test_option_and() {
    let x = Some(2);
    let y: Option<ByteArray> = None;
    assert_eq!(x.and(y), None);

    let x: Option<u32> = None;
    let y: Option<ByteArray> = Some("foo");
    assert_eq!(x.and(y), None);

    let x = Some(2);
    let y: Option<ByteArray> = Some("foo");
    assert_eq!(x.and(y), Some("foo"));

    let x: Option<u32> = None;
    let y: Option<ByteArray> = None;
    assert_eq!(x.and(y), None);
}

#[test]
fn test_option_and_then() {
    let checked_mul = core::num::traits::CheckedMul::checked_mul(2_u32, 2_u32);
    let option: Option<ByteArray> = checked_mul.and_then(|v| Some(format!("{}", v)));
    assert_eq!(option, Some("4"));

    let checked_mul = core::num::traits::CheckedMul::checked_mul(65536_u32, 65536_u32);
    let option: Option<ByteArray> = checked_mul.and_then(|v| Some(format!("{}", v)));
    assert_eq!(option, None); // overflowed!

    let option: Option<ByteArray> = Option::<u32>::None.and_then(|v| Some(format!("{}", v)));
    assert_eq!(option, None);
}

#[test]
fn test_option_or() {
    let x = Some(2);
    let y = None;
    assert_eq!(x.or(y), Some(2));

    let x = None;
    let y = Some(100);
    assert_eq!(x.or(y), Some(100));

    let x = Some(2);
    let y = Some(100);
    assert_eq!(x.or(y), Some(2));

    let x: Option<u32> = None;
    let y = None;
    assert_eq!(x.or(y), None);
}

#[test]
fn test_option_or_else() {
    let nobody = || Option::<ByteArray>::None;
    let vikings = || Option::<ByteArray>::Some("vikings");

    assert_eq!(Some("barbarians").or_else(vikings), Some("barbarians"));
    assert_eq!(None.or_else(vikings), Some("vikings"));
    assert_eq!(None.or_else(nobody), None);
}

#[test]
fn test_option_xor() {
    let x = Some(2);
    let y: Option<u32> = None;
    assert_eq!(x.xor(y), Some(2));

    let x: Option<u32> = None;
    let y = Some(2);
    assert_eq!(x.xor(y), Some(2));

    let x = Some(2);
    let y = Some(2);
    assert_eq!(x.xor(y), None);

    let x: Option<u32> = None;
    let y: Option<u32> = None;
    assert_eq!(x.xor(y), None);
}

fn test_option_some_is_none_or() {
    assert_eq!(Some(2_u8).is_none_or(|x| x > 1), true);
    assert_eq!(Some(0_u8).is_none_or(|x| x > 1), false);
}

#[test]
fn test_option_none_is_none_or() {
    let option: Option<u8> = None;
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
    let maybe_some_string: Option<ByteArray> = Some("Hello, World!");
    let maybe_some_len = maybe_some_string.map(|s| s.len());
    assert!(maybe_some_len == Some(13));
}

#[test]
fn test_option_none_map() {
    let x: Option<ByteArray> = None;
    assert!(x.map(|s: ByteArray| s.len()) == None);
}

#[test]
fn test_option_some_map_or() {
    assert_eq!(Some("foo").map_or(42, |v: ByteArray| v.len()), 3);
}

#[test]
fn test_option_none_map_or() {
    let x: Option<ByteArray> = None;
    assert_eq!(x.map_or(42, |v: ByteArray| v.len()), 42);
}

#[test]
fn test_option_some_map_or_else() {
    let k = 21;
    let x = Some("foo");
    assert_eq!(x.map_or_else(|| 2 * k, |v: ByteArray| v.len()), 3);
}

#[test]
fn test_option_none_map_or_else() {
    let k = 21;
    let x: Option<ByteArray> = None;
    assert_eq!(x.map_or_else(|| 2 * k, |v: ByteArray| v.len()), 42);
}

fn test_option_some_into_iter() {
    let x: Option<u32> = Some(5);
    let mut x_iter = x.into_iter();
    assert!(x_iter.next() == Some(5));
    assert!(x_iter.next() == None);
}

#[test]
fn test_option_none_into_iter() {
    let x: Option<u32> = None;
    let mut x_iter = x.into_iter();
    assert!(x_iter.next() == None);
}

#[test]
fn test_option_take() {
    let mut x = Some(2);
    let y = x.take();
    assert_eq!(x, None);
    assert_eq!(y, Some(2));

    let mut x: Option<u32> = None;
    let y = x.take();
    assert_eq!(x, None);
    assert_eq!(y, None);
}

#[test]
fn test_option_filter() {
    let is_even = |x: @u32| -> bool {
        *x % 2 == 0
    };

    assert!(None.filter(is_even) == None);
    assert!(Some(3).filter(is_even) == None);
    assert!(Some(4).filter(is_even) == Some(4));
}

#[test]
fn test_option_flatten() {
    let x: Option<Option<u32>> = Some(Some(6));
    assert_eq!(Some(6), x.flatten());

    let x: Option<Option<u32>> = Some(None);
    assert_eq!(None, x.flatten());

    let x: Option<Option<u32>> = None;
    assert_eq!(None, x.flatten());

    let x: Option<Option<Option<u32>>> = Some(Some(Some(6)));
    assert_eq!(Some(Some(6)), x.flatten());
    assert_eq!(Some(6), x.flatten().flatten());
}

#[test]
fn test_option_into() {
    let o: Option<u8> = 67_u8.into();
    assert_eq!(Some(67), o);
}
