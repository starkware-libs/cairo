impl MyCopy of Copy::<Option::<(felt, felt)>>;

fn foo(x: Option::<(felt, felt)>) -> Option::<felt> {
    let y = x;
    match x {
        Option::Some(x) => {
            let (x, y) = x;
            Option::<felt>::Some(x)
        },
        // TODO(spapini): Replace with _.
        Option::None(o) => {
            return Option::<felt>::None(());
        },
    }
}
