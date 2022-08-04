#[test]
fn format_test() {
    let parser = sierra::ProgramParser::new();
    assert_eq!(
        parser
            .parse(
                r#"
type ConcreteTypeId =  TypeId;
type ConcreteTypeId  = TypeId<arg>;
type  ConcreteTypeId = TypeId<arg1, 4>;
type [123] = TypeId<[12],  4>;
ext CalleeId = ExtensionId ;
ext OtherCalleeId = ExtensionId <arg, 4>;
ext [5642] = ExtensionId<[22 ], 4>;
callee() -> ();
callee(arg1) -> (res1);
callee( arg1, arg2) -> ( res1, res2);
callee() { 5( ) };
callee(arg1 , arg2) { fallthrough() 7(res1 ) 5(res1, res2) };
[12345]([12]) { 2([37]) fallthrough() };
return();
return ( r);
return(r1 , r2);
return ([1], [45], [0]);

Name@5() -> ();
Other@3([5]: T1) -> (T2);
[343]@3([5]: [6343]) -> ([341]);
        "#,
            )
            .map(|p| p.to_string()),
        Ok(r#"type ConcreteTypeId = TypeId;
type ConcreteTypeId = TypeId<arg>;
type ConcreteTypeId = TypeId<arg1, 4>;
type [123] = TypeId<[12], 4>;

ext CalleeId = ExtensionId;
ext OtherCalleeId = ExtensionId<arg, 4>;
ext [5642] = ExtensionId<[22], 4>;

callee() -> ();
callee(arg1) -> (res1);
callee(arg1, arg2) -> (res1, res2);
callee() { 5() };
callee(arg1, arg2) { fallthrough() 7(res1) 5(res1, res2) };
[12345]([12]) { 2([37]) fallthrough() };
return();
return(r);
return(r1, r2);
return([1], [45], [0]);

Name@5() -> ();
Other@3([5]: T1) -> (T2);
[343]@3([5]: [6343]) -> ([341]);
"#
        .to_string())
    );
}
