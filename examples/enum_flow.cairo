enum MyEnum { a: felt, b: felt }
func main() -> felt {
    let e1 = MyEnum::b(10);
    let e2 = foo(e1);
    1
}

func foo(e: MyEnum) -> MyEnum {
    e
}
