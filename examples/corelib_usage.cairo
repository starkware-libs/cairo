impl MyCopy of Copy<Option<(felt252, felt252)>>;

pub fn foo(x: Option<(felt252, felt252)>) -> Option<felt252> {
    let y = x;
    match x {
        Option::Some(x) => {
            let (x, y) = x;
            Option::Some(x)
        },
        // TODO(spapini): Replace with _.
        Option::None(o) => {
            return Option::None(());
        },
    }
}
