func foo(x: Option::<(felt, felt)>) -> Option::<felt> {
    match x {
        Option::Some (x) => {
            let (x, y) = x;
            Option::<felt>::Some(x)
        },
        Option::None (_) => {
            return Option::<felt>::None(());
        },
    }
}
