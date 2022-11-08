enum MyEnumShort { a: felt, b: felt }
enum MyEnumLong { a: felt, b: felt, c: felt }
func main() -> felt {
    let es0 = MyEnumShort::a(10);
    let es1 = MyEnumShort::b(11);
    match_short(es0);
    match_short(es1);

    let el0 = MyEnumLong::a(20);
    let el1 = MyEnumLong::b(21);
    let el2 = MyEnumLong::c(22);
    match_long(el0);
    match_long(el1);
    match_long(el2);
    300
}

func match_short(e: MyEnumShort) -> felt {
    match e {
        MyEnumShort::a (x) => {
            x
        },
        MyEnumShort::b (x) => {
            x
        },
    }
}

func match_long(e: MyEnumLong) -> felt {
    match e {
        MyEnumLong::a (x) => {
            x
        },
        MyEnumLong::b (x) => {
            x
        },
        MyEnumLong::c (x) => {
            x
        },
    }
}
