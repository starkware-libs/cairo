use crate::result::{Result, ResultTraitImpl};

#[test]
fn test_result_ok_expect() {
    let result: Result<u32, felt252> = Result::Ok(42);
    assert(result.expect('') == 42, 'result_ok_expect');
}

#[test]
#[should_panic(expected: ('err msg',))]
fn test_result_err_expect() {
    let result: Result<u32, felt252> = Result::Err('no');
    result.expect('err msg');
}

#[test]
fn test_result_ok_unwrap() {
    let result: Result<u32, felt252> = Result::Ok(42);
    assert(result.unwrap() == 42, 'result_ok_unwrap');
}

#[test]
#[should_panic(expected: ('Result::unwrap failed.',))]
fn test_result_err_unwrap() {
    let result: Result<u32, felt252> = Result::Err('no');
    result.unwrap();
}

#[test]
fn test_result_ok_unwrap_or() {
    let result: Result<u32, felt252> = Result::Ok(42);
    assert(result.unwrap_or(0) == 42, 'result_ok_unwrap_or');
}

#[test]
fn test_result_err_unwrap_or() {
    let result: Result<u32, felt252> = Result::Err('no');
    assert(result.unwrap_or(0) == 0, 'result_err_unwrap_or');
}

#[test]
fn test_result_ok_unwrap_or_default() {
    let result: Result<u32, felt252> = Result::Ok(42);
    assert(result.unwrap_or_default() == 42, 'result_ok_unwrap_or_default');
}

#[test]
fn test_result_err_unwrap_or_default() {
    let result: Result<u32, felt252> = Result::Err('no');
    assert(result.unwrap_or_default() == 0, 'result_err_unwrap_or_default');
}

#[test]
fn test_result_ok_unwrap_or_else() {
    assert!(Result::Ok(2).unwrap_or_else(|e: ByteArray| e.len()) == 2);
}

#[test]
fn test_result_err_unwrap_or_else() {
    assert!(Result::Err("foo").unwrap_or_else(|e: ByteArray| e.len()) == 3);
}

#[test]
fn test_result_ok_and_err() {
    let x: Result<u32, ByteArray> = Result::Ok(2);
    let y: Result<ByteArray, ByteArray> = Result::Err("late error");
    assert!(x.and(y) == Result::Err("late error"));
}

#[test]
fn test_result_err_and_ok() {
    let x: Result<u32, ByteArray> = Result::Err("early error");
    let y: Result<ByteArray, ByteArray> = Result::Ok("foo");
    assert!(x.and(y) == Result::Err("early error"));
}

#[test]
fn test_result_err_and_err() {
    let x: Result<u32, ByteArray> = Result::Err("not a 2");
    let y: Result<ByteArray, ByteArray> = Result::Err("late error");
    assert!(x.and(y) == Result::Err("not a 2"));
}

#[test]
fn test_result_and_ok_and_ok() {
    let x: Result<u32, ByteArray> = Result::Ok(2);
    let y: Result<ByteArray, ByteArray> = Result::Ok("different result type");
    assert!(x.and(y) == Result::Ok("different result type"));
}

#[test]
fn test_result_err_and_then() {
    let checked_mul = core::num::traits::CheckedMul::checked_mul(65536_u32, 65536_u32)
        .ok_or("overflowed");
    let res: Result<ByteArray, ByteArray> = checked_mul.and_then(|v| Result::Ok(format!("{}", v)));
    assert!(res == Result::Err("overflowed"));
}

#[test]
fn test_result_ok_and_then_err() {
    let x: Result<u32, ByteArray> = Result::<u32, ByteArray>::Ok(2)
        .and_then(|_x| Result::Err("late error"));
    assert!(x == Result::Err("late error"));
}

#[test]
fn test_result_ok_and_then_ok() {
    let checked_mul = core::num::traits::CheckedMul::checked_mul(4_u32, 4_u32).ok_or("overflowed");
    let res: Result<ByteArray, ByteArray> = checked_mul.and_then(|v| Result::Ok(format!("{}", v)));
    assert!(res == Result::Ok("16"));
}

#[test]
fn test_result_or_ok_and_err() {
    let x: Result<u32, ByteArray> = Result::Ok(2);
    let y: Result<u32, ByteArray> = Result::Err("late error");
    assert!(x.or(y) == Result::Ok(2));
}

#[test]
fn test_result_or_err_and_ok() {
    let x: Result<u32, ByteArray> = Result::Err("early error");
    let y: Result<u32, ByteArray> = Result::Ok(2);
    assert!(x.or(y) == Result::Ok(2));
}

#[test]
fn test_result_or_err_and_err() {
    let x: Result<u32, ByteArray> = Result::Err("not a 2");
    let y: Result<u32, ByteArray> = Result::Err("late error");
    assert!(x.or(y) == Result::Err("late error"));
}

#[test]
fn test_result_or_ok_and_ok() {
    let x: Result<u32, ByteArray> = Result::Ok(2);
    let y: Result<u32, ByteArray> = Result::Ok(100);
    assert!(x.or(y) == Result::Ok(2));
}

#[test]
fn test_result_err_or_else_err() {
    let y: Result<u32, ByteArray> = Result::<u32, ByteArray>::Err("bad input")
        .or_else(|_e| Result::Err("not 42"));
    assert!(y == Result::Err("not 42"));
}

#[test]
fn test_result_err_or_else_ok() {
    let x: Result<u32, ByteArray> = Result::<u32, ByteArray>::Err("bad input")
        .or_else(|_e| Result::Ok(42));
    assert!(x == Result::Ok(42));
}

#[test]
fn test_result_ok_or_else() {
    let z: Result<u32, ByteArray> = Result::<u32, ByteArray>::Ok(100).or_else(|_e| Result::Ok(42));
    assert!(z == Result::Ok(100));
}


#[test]
#[should_panic(expected: ('err msg',))]
fn test_result_ok_expect_err() {
    let result: Result<u32, felt252> = Result::Ok(42);
    result.expect_err('err msg');
}

#[test]
fn test_result_err_expect_err() {
    let result: Result<u32, felt252> = Result::Err('no');
    assert(result.expect_err('') == 'no', 'result_err_expect_err');
}

#[test]
#[should_panic(expected: ('Result::unwrap_err failed.',))]
fn test_result_ok_unwrap_err() {
    let result: Result<u32, felt252> = Result::Ok(42);
    result.unwrap_err();
}

#[test]
fn test_result_err_unwrap_err() {
    let result: Result<u32, felt252> = Result::Err('no');
    assert(result.unwrap_err() == 'no', 'result_err_unwrap_err');
}

#[test]
fn test_result_ok_is_ok() {
    let result: Result<u32, felt252> = Result::Ok(42);
    assert(result.is_ok(), 'result_ok_is_ok');
}

#[test]
fn test_result_err_is_ok() {
    let result: Result<u32, felt252> = Result::Err('no');
    assert(!result.is_ok(), 'result_err_is_ok');
}

#[test]
fn test_result_ok_is_err() {
    let result: Result<u32, felt252> = Result::Ok(42);
    assert(!result.is_err(), 'result_ok_is_err');
}

#[test]
fn test_result_err_is_err() {
    let result: Result<u32, felt252> = Result::Err('no');
    assert(result.is_err(), 'result_err_is_err');
}

#[test]
fn test_result_ok_into_is_err() {
    let result: Result<u32, felt252> = Result::Ok(42);
    assert(!result.into_is_err(), 'result_ok_into_is_err');
}

#[test]
fn test_result_err_into_is_err() {
    let result: Result<u32, felt252> = Result::Err('no');
    assert(result.into_is_err(), 'result_err_into_is_err');
}

#[test]
fn test_result_ok_into_is_ok() {
    let result: Result<u32, felt252> = Result::Ok(42);
    assert(result.into_is_ok(), 'result_ok_into_is_ok');
}

#[test]
fn test_result_err_into_is_ok() {
    let result: Result<u32, felt252> = Result::Err('no');
    assert(!result.into_is_ok(), 'result_err_into_is_ok');
}

#[test]
fn test_result_ok_ok_should_return_ok_value() {
    let x: Result<u32, ByteArray> = Result::Ok(2);
    assert_eq!(x.ok(), Option::Some(2));
}

#[test]
fn test_result_err_ok_should_return_none() {
    let x: Result<u32, ByteArray> = Result::Err("Nothing here");
    assert!(x.ok().is_none());
}

#[test]
fn test_result_err_err_should_return_error_value() {
    let x: Result<u32, ByteArray> = Result::Err("Nothing here");
    assert_eq!(x.err(), Option::Some("Nothing here"));
}

#[test]
fn test_result_ok_err_should_return_none() {
    let x: Result<u32, ByteArray> = Result::Ok(2);
    assert!(x.err().is_none());
}
