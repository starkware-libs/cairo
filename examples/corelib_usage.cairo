impl MyCopy of Copy<Option<(felt252, felt252)>>;

fn foo(x: Option<(felt252, felt252)>) -> Option<felt252> {
    let _y = x;
    match x {
        Some(x) => {
            let (x, _y) = x;
            Some(x)
        },
        None => { return None; },
    }
}
