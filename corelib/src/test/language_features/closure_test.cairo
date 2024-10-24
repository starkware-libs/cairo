#[test]
fn closure() {
    let x = 8;
    let c = |a| {
        return x * (a + 3);
        7_felt252
    };
    assert_eq!(c(2), 40);
}

#[derive(Destruct)]
struct DestuctOnly {}

#[test]
fn closure_destruct() {
    let a = DestuctOnly {};
    || {
        let _b = a;
    };
}

#[derive(PanicDestruct)]
struct PanicDestructOnly {}

#[test]
#[should_panic(expected: "outer")]
fn closure_panic_destruct() {
    let a = PanicDestructOnly {};
    || -> () {
        let _b = a;
        panic!("inner")
    };
    panic!("outer")
}

#[test]
fn panicable_closure() {
    let c = |a: u32| {
        a + 3
    };
    assert_eq!(c(2), 5);
}

fn option_map<T, F, +core::ops::FnOnce<F, (T,)>, +Drop<F>>(
    opt: Option<T>, f: F
) -> Option<core::ops::FnOnce::<F, (T,)>::Output> {
    match opt {
        Option::Some(x) => Option::Some(f(x)),
        Option::None => Option::None
    }
}

#[test]
fn option_map_test() {
    assert_eq!(option_map(Option::Some(2), |x| x + 3), Option::Some(5));
    assert_eq!(option_map(Option::None, |x| x + 3), Option::None);
    assert_eq!(option_map(Option::Some(2), |x| Option::Some(x)), Option::Some(Option::Some(2)));
}

