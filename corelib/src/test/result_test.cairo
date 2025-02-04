use crate::result::{Result, ResultTraitImpl};

#[test]
fn test_result_ok_expect() {
    let result: Result<u32, felt252> = Ok(42);
    assert(result.expect('') == 42, 'result_ok_expect');
}

#[test]
#[should_panic(expected: ('err msg',))]
fn test_result_err_expect() {
    let result: Result<u32, felt252> = Err('no');
    result.expect('err msg');
}

#[test]
fn test_result_ok_unwrap() {
    let result: Result<u32, felt252> = Ok(42);
    assert(result.unwrap() == 42, 'result_ok_unwrap');
}

#[test]
#[should_panic(expected: ('Result::unwrap failed.',))]
fn test_result_err_unwrap() {
    let result: Result<u32, felt252> = Err('no');
    result.unwrap();
}

#[test]
fn test_result_ok_unwrap_or() {
    let result: Result<u32, felt252> = Ok(42);
    assert(result.unwrap_or(0) == 42, 'result_ok_unwrap_or');
}

#[test]
fn test_result_err_unwrap_or() {
    let result: Result<u32, felt252> = Err('no');
    assert(result.unwrap_or(0) == 0, 'result_err_unwrap_or');
}

#[test]
fn test_result_ok_unwrap_or_default() {
    let result: Result<u32, felt252> = Ok(42);
    assert(result.unwrap_or_default() == 42, 'result_ok_unwrap_or_default');
}

#[test]
fn test_result_err_unwrap_or_default() {
    let result: Result<u32, felt252> = Err('no');
    assert(result.unwrap_or_default() == 0, 'result_err_unwrap_or_default');
}

#[test]
fn test_result_ok_unwrap_or_else() {
    assert!(Ok(2).unwrap_or_else(|e: ByteArray| e.len()) == 2);
}

#[test]
fn test_result_err_unwrap_or_else() {
    assert!(Err("foo").unwrap_or_else(|e: ByteArray| e.len()) == 3);
}

#[test]
fn test_result_ok_and_err() {
    let x: Result<u32, ByteArray> = Ok(2);
    let y: Result<ByteArray, ByteArray> = Err("late error");
    assert!(x.and(y) == Err("late error"));
}

#[test]
fn test_result_err_and_ok() {
    let x: Result<u32, ByteArray> = Err("early error");
    let y: Result<ByteArray, ByteArray> = Ok("foo");
    assert!(x.and(y) == Err("early error"));
}

#[test]
fn test_result_err_and_err() {
    let x: Result<u32, ByteArray> = Err("not a 2");
    let y: Result<ByteArray, ByteArray> = Err("late error");
    assert!(x.and(y) == Err("not a 2"));
}

#[test]
fn test_result_and_ok_and_ok() {
    let x: Result<u32, ByteArray> = Ok(2);
    let y: Result<ByteArray, ByteArray> = Ok("different result type");
    assert!(x.and(y) == Ok("different result type"));
}

#[test]
fn test_result_err_and_then() {
    let checked_mul = core::num::traits::CheckedMul::checked_mul(65536_u32, 65536_u32)
        .ok_or("overflowed");
    let res: Result<ByteArray, ByteArray> = checked_mul.and_then(|v| Ok(format!("{}", v)));
    assert!(res == Err("overflowed"));
}

#[test]
fn test_result_ok_and_then_err() {
    let x: Result<u32, ByteArray> = Result::<u32, ByteArray>::Ok(2)
        .and_then(|_x| Err("late error"));
    assert!(x == Err("late error"));
}

#[test]
fn test_result_ok_and_then_ok() {
    let checked_mul = core::num::traits::CheckedMul::checked_mul(4_u32, 4_u32).ok_or("overflowed");
    let res: Result<ByteArray, ByteArray> = checked_mul.and_then(|v| Ok(format!("{}", v)));
    assert!(res == Ok("16"));
}

#[test]
fn test_result_or_ok_and_err() {
    let x: Result<u32, ByteArray> = Ok(2);
    let y: Result<u32, ByteArray> = Err("late error");
    assert!(x.or(y) == Ok(2));
}

#[test]
fn test_result_or_err_and_ok() {
    let x: Result<u32, ByteArray> = Err("early error");
    let y: Result<u32, ByteArray> = Ok(2);
    assert!(x.or(y) == Ok(2));
}

#[test]
fn test_result_or_err_and_err() {
    let x: Result<u32, ByteArray> = Err("not a 2");
    let y: Result<u32, ByteArray> = Err("late error");
    assert!(x.or(y) == Err("late error"));
}

#[test]
fn test_result_or_ok_and_ok() {
    let x: Result<u32, ByteArray> = Ok(2);
    let y: Result<u32, ByteArray> = Ok(100);
    assert!(x.or(y) == Ok(2));
}

#[test]
fn test_result_err_or_else_err() {
    let y: Result<u32, ByteArray> = Result::<u32, ByteArray>::Err("bad input")
        .or_else(|_e| Err("not 42"));
    assert!(y == Err("not 42"));
}

#[test]
fn test_result_err_or_else_ok() {
    let x: Result<u32, ByteArray> = Result::<u32, ByteArray>::Err("bad input").or_else(|_e| Ok(42));
    assert!(x == Ok(42));
}

#[test]
fn test_result_ok_or_else() {
    let z: Result<u32, ByteArray> = Result::<u32, ByteArray>::Ok(100).or_else(|_e| Ok(42));
    assert!(z == Ok(100));
}


#[test]
#[should_panic(expected: ('err msg',))]
fn test_result_ok_expect_err() {
    let result: Result<u32, felt252> = Ok(42);
    result.expect_err('err msg');
}

#[test]
fn test_result_err_expect_err() {
    let result: Result<u32, felt252> = Err('no');
    assert(result.expect_err('') == 'no', 'result_err_expect_err');
}

#[test]
#[should_panic(expected: ('Result::unwrap_err failed.',))]
fn test_result_ok_unwrap_err() {
    let result: Result<u32, felt252> = Ok(42);
    result.unwrap_err();
}

#[test]
fn test_result_err_unwrap_err() {
    let result: Result<u32, felt252> = Err('no');
    assert(result.unwrap_err() == 'no', 'result_err_unwrap_err');
}

#[test]
fn test_result_ok_is_ok() {
    let result: Result<u32, felt252> = Ok(42);
    assert(result.is_ok(), 'result_ok_is_ok');
}

#[test]
fn test_result_err_is_ok() {
    let result: Result<u32, felt252> = Err('no');
    assert(!result.is_ok(), 'result_err_is_ok');
}

#[test]
fn test_result_ok_is_err() {
    let result: Result<u32, felt252> = Ok(42);
    assert(!result.is_err(), 'result_ok_is_err');
}

#[test]
fn test_result_err_is_err() {
    let result: Result<u32, felt252> = Err('no');
    assert(result.is_err(), 'result_err_is_err');
}

#[test]
fn test_result_ok_into_is_err() {
    let result: Result<u32, felt252> = Ok(42);
    assert(!result.into_is_err(), 'result_ok_into_is_err');
}

#[test]
fn test_result_err_into_is_err() {
    let result: Result<u32, felt252> = Err('no');
    assert(result.into_is_err(), 'result_err_into_is_err');
}

#[test]
fn test_result_ok_into_is_ok() {
    let result: Result<u32, felt252> = Ok(42);
    assert(result.into_is_ok(), 'result_ok_into_is_ok');
}

#[test]
fn test_result_err_into_is_ok() {
    let result: Result<u32, felt252> = Err('no');
    assert(!result.into_is_ok(), 'result_err_into_is_ok');
}

#[test]
fn test_result_ok_ok_should_return_ok_value() {
    let x: Result<u32, ByteArray> = Ok(2);
    assert_eq!(x.ok(), Some(2));
}

#[test]
fn test_result_err_ok_should_return_none() {
    let x: Result<u32, ByteArray> = Err("Nothing here");
    assert!(x.ok().is_none());
}

#[test]
fn test_result_err_err_should_return_error_value() {
    let x: Result<u32, ByteArray> = Err("Nothing here");
    assert_eq!(x.err(), Some("Nothing here"));
}

#[test]
fn test_result_ok_err_should_return_none() {
    let x: Result<u32, ByteArray> = Ok(2);
    assert!(x.err().is_none());
}

#[test]
fn test_result_ok_map() {
    let x: Result<u32, ByteArray> = Ok(1);
    assert!(x.map(|i| i * 2) == Ok(2));
}

#[test]
fn test_result_err_map() {
    let x: Result<u32, ByteArray> = Err("error");
    assert!(x.map(|i| i * 2) == Err("error"));
}

#[test]
fn test_result_ok_map_or() {
    let x: Result<ByteArray, ByteArray> = Ok("foo");
    assert!(x.map_or(42, |v: ByteArray| v.len()) == 3);
}

#[test]
fn test_result_err_map_or() {
    let x: Result<ByteArray, ByteArray> = Err("bar");
    assert!(x.map_or(42, |v: ByteArray| v.len()) == 42);
}

#[test]
fn test_result_ok_map_or_else() {
    let k = 21;
    let x: Result<ByteArray, _> = Ok("foo");
    assert!(x.map_or_else(|_e: ByteArray| k * 2, |v: ByteArray| v.len()) == 3);
}

#[test]
fn test_result_err_map_or_else() {
    let k = 21;
    let x: Result<_, ByteArray> = Err("bar");
    assert!(x.map_or_else(|_e| k * 2, |v: ByteArray| v.len()) == 42);
}

#[test]
fn test_result_ok_map_err() {
    let stringify = |x: u32| -> ByteArray {
        format!("error code: {}", x)
    };
    let x: Result<u32, u32> = Ok(2);
    assert!(x.map_err(stringify) == Result::<u32, ByteArray>::Ok(2));
}

#[test]
fn test_result_err_map_err() {
    let stringify = |x: u32| -> ByteArray {
        format!("error code: {}", x)
    };
    let x: Result<u32, u32> = Err(13);
    assert!(x.map_err(stringify) == Err("error code: 13"));
}

#[test]
fn test_result_ok_iter_next() {
    let x: Result<u32, ByteArray> = Ok(5);
    let mut x_iter = x.into_iter();
    assert!(x_iter.next() == Some(5));
    assert!(x_iter.next() == None);
}

#[test]
fn test_result_err_iter_next() {
    let x: Result<u32, ByteArray> = Err("nothing!");
    let mut x_iter = x.into_iter();
    assert!(x_iter.next() == None);
}
