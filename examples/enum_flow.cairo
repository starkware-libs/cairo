enum MyEnum { a: felt, b: felt }
func main() -> felt {
    let e1 = MyEnum::b(10);
    let e2 = foo(e1);
    115 // Just since we don't support void functions.
}

func foo(e: MyEnum) -> MyEnum {
    e
}
