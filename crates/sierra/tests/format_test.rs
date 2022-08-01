#[test]
fn format_test() {
    let parser = sierra::ProgramParser::new();
    assert_eq!(
        parser
            .parse(
                r#"
            ext() -> ();
            ext(arg1) -> (res1);
            ext(arg1, arg2) -> (res1, res2);
            ext() { 5() };
            ext(arg1, arg2) { fallthrough() 7(res1) 5(res1, res2) };
    
            Name@0() -> ();
            Name@1(arg1: Arg1) -> (Res1);
            Name@4(arg1: Arg1, arg2: Arg2) -> (Res1, Res2);
        "#,
            )
            .map(|p| p.to_string()),
        Ok(r#"ext() -> ();
ext(arg1) -> (res1);
ext(arg1, arg2) -> (res1, res2);
ext() { 5() };
ext(arg1, arg2) { fallthrough() 7(res1) 5(res1, res2) };

Name@0() -> ();
Name@1(arg1: Arg1) -> (Res1);
Name@4(arg1: Arg1, arg2: Arg2) -> (Res1, Res2);
"#
        .to_string())
    );
}
