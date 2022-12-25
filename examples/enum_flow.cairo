enum MyEnumShort { a: felt, b: felt }
enum MyEnumLong { a: felt, b: felt, c: felt }
enum MyEnumGeneric<S, T> { a: T, b: S, c: T }
fn main() -> felt {
    let es0 = MyEnumShort::a(10);
    match_short(es0);
    let es1 = MyEnumShort::b(11);
    match_short(es1);

    let el0 = MyEnumLong::a(20);
    match_long(el0);
    let el1 = MyEnumLong::b(21);
    match_long(el1);
    let el2 = MyEnumLong::c(22);
    match_long(el2);
    let eg1: MyEnumGeneric::<(), felt> = MyEnumGeneric::<(), felt>::a(30);
    let eg2: MyEnumGeneric::<(), felt> = MyEnumGeneric::<(), felt>::b(());
    let eg3: MyEnumGeneric::<(), felt> = MyEnumGeneric::<(), felt>::c(32);
    300
}

fn match_short(e: MyEnumShort) -> felt {
    match e {
        MyEnumShort::a(x) => {
            x
        },
        MyEnumShort::b(x) => {
            x
        },
    }
}

fn match_long(e: MyEnumLong) -> felt {
    match e {
        MyEnumLong::a(x) => {
            x
        },
        MyEnumLong::b(x) => {
            x
        },
        MyEnumLong::c(x) => {
            x
        },
    }
}
