enum MyEnumShort {
    a: felt252,
    b: felt252
}
enum MyEnumLong {
    a: felt252,
    b: felt252,
    c: felt252
}
enum MyEnumGeneric<S, T> {
    a: T,
    b: S,
    c: T
}

impl MyEnumGenericDrop of Drop<MyEnumGeneric<(), felt252>>;

fn main() -> felt252 {
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
    let _eg1: MyEnumGeneric<(), felt252> = MyEnumGeneric::<(), felt252>::a(30);
    let _eg2: MyEnumGeneric<(), felt252> = MyEnumGeneric::<(), felt252>::b;
    let _eg3: MyEnumGeneric<(), felt252> = MyEnumGeneric::<(), felt252>::c(32);
    300
}

fn match_short(e: MyEnumShort) -> felt252 {
    match e {
        MyEnumShort::a(x) => { x },
        MyEnumShort::b(x) => { x },
    }
}

fn match_long(e: MyEnumLong) -> felt252 {
    match e {
        MyEnumLong::a(x) => { x },
        MyEnumLong::b(x) => { x },
        MyEnumLong::c(x) => { x },
    }
}
