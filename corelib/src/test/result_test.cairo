use core::result::{Result, ResultTraitImpl};

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
