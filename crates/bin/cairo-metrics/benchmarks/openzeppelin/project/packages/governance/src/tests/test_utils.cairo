use openzeppelin_testing::AsAddressTrait;
use starknet::account::Call;
use crate::utils::call_impls::CallPartialEq;

//
// eq
//

#[test]
fn test_eq_calls_no_calldata() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![].span() };
    let call_2 = Call { to: 1.as_address(), selector: 1, calldata: array![].span() };
    assert_eq!(call_1, call_2);
}

#[test]
fn test_eq_calls_with_calldata() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2, 3].span() };
    let call_2 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2, 3].span() };
    assert_eq!(call_1, call_2);
}

#[test]
#[should_panic]
fn test_eq_calls_ne_to() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![].span() };
    let call_2 = Call { to: 2.as_address(), selector: 1, calldata: array![].span() };
    assert_eq!(call_1, call_2);
}

#[test]
#[should_panic]
fn test_eq_calls_ne_selector() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![].span() };
    let call_2 = Call { to: 1.as_address(), selector: 2, calldata: array![].span() };
    assert_eq!(call_1, call_2);
}

#[test]
#[should_panic]
fn test_eq_calls_gt_calldata() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![1].span() };
    let call_2 = Call { to: 1.as_address(), selector: 2, calldata: array![].span() };
    assert_eq!(call_1, call_2);
}

#[test]
#[should_panic]
fn test_eq_calls_lt_calldata() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![1].span() };
    let call_2 = Call { to: 1.as_address(), selector: 2, calldata: array![1, 2].span() };
    assert_eq!(call_1, call_2);
}

#[test]
#[should_panic]
fn test_eq_calls_ne_calldata() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2].span() };
    let call_2 = Call { to: 1.as_address(), selector: 2, calldata: array![2, 1].span() };
    assert_eq!(call_1, call_2);
}

//
// ne
//

#[test]
fn test_ne_calls_to() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2, 3].span() };
    let call_2 = Call { to: 2.as_address(), selector: 1, calldata: array![1, 2, 3].span() };
    assert_ne!(call_1, call_2);
}

#[test]
fn test_ne_calls_selector() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2, 3].span() };
    let call_2 = Call { to: 1.as_address(), selector: 2, calldata: array![1, 2, 3].span() };
    assert_ne!(call_1, call_2);
}

#[test]
fn test_ne_calls_gt_calldata() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2, 3].span() };
    let call_2 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2].span() };
    assert_ne!(call_1, call_2);
}

#[test]
fn test_ne_calls_lt_calldata() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2].span() };
    let call_2 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2, 3].span() };
    assert_ne!(call_1, call_2);
}

#[test]
fn test_ne_calls_eq_len_calldata() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2, 3].span() };
    let call_2 = Call { to: 1.as_address(), selector: 1, calldata: array![3, 2, 1].span() };
    assert_ne!(call_1, call_2);
}

#[test]
#[should_panic]
fn test_ne_calls_when_eq() {
    let call_1 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2, 3].span() };
    let call_2 = Call { to: 1.as_address(), selector: 1, calldata: array![1, 2, 3].span() };
    assert_ne!(call_1, call_2);
}
