enum Option<T> { Some: T, None: (), }

/// If `val` is `Option::Some(x)`, returns `x`. Otherwise, panics.
fn option_unwrap<T>(val: Option::<T>) -> T {
    match val {
        Option::Some(x) => x,
        Option::None(()) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, 'option_unwrap failed.')
            panic(data)
        },
    }
}

/// Returns `true` if the `Option` is `Option::Some`.
// TODO(lior): Make option_is_some and option_is_none inline.
fn option_is_some<T>(val: Option::<T>) -> bool {
    match val {
        Option::Some(x) => true,
        Option::None(()) => false,
    }
}

/// Returns `true` if the `Option` is `Option::None`.
fn option_is_none<T>(val: Option::<T>) -> bool {
    match val {
        Option::Some(x) => false,
        Option::None(()) => true,
    }
}
