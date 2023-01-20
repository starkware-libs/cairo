enum Option<T> { Some: T, None: (), }

/// If `val` is `Option::Some(x)`, returns `x`. Otherwise, panics.
fn option_unwrap<T>(val: Option::<T>) -> T {
    match val {
        Option::Some(x) => x,
        Option::None(()) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(data, 'option_unwrap failed.')
            panic(data)
        },
    }
}
