fn foo(
    // before first param
    a: u32,
    // between params
    b: u32,
    // before close paren
) {}

struct S {
    a: u32,
    b: u32,
}

fn call_args() {
    foo(
        1,
        // Before the second argument.
        0,
    );
}

fn macro_args() {
    let arr = array![
        1,
        // Before the second element.
        0,
    ];
}

fn fixed_size_array() {
    let arr = [
        1,
        // Before the second element.
        0,
    ];
}

fn tuple() {
    let t = (
        // Before the first element.
        1,
        // Before the second element.
        2,
    );
}

fn struct_ctor() {
    let s = S {
        // Before a.
        a: 1,
        // Before b.
        b: 2,
    };
}

fn struct_pattern(s: S) {
    let S {
        // Before a.
        a,
        // Before b.
        b,
    } = s;
}

fn generic_args() {
    let x = bar::<
        // Before the first type.
        u32,
        // Before the second type.
        u64,
    >();
}

fn trailing_comment_before_close() {
    foo(
        1,
        0,
        // Before the closing parenthesis.
    );
}

fn fixed_size_array_comment_before_close() {
    let a = [
        1,
        2,
        // c
    ];
}

fn trailing_comment_on_item_line() {
    foo(
        1, // Trailing the first argument.
        0,
    );
}

fn trailing_comment_on_last_item_line() {
    foo(
        1,
        0 // Trailing the last argument.
    );
}

fn trailing_comment_on_param_line(
    a: felt252, // Trailing the first parameter.
    b: felt252,
) {}

fn trailing_comment_on_element_line() {
    let arr = array![
        1, // Trailing the first element.
        0
    ];
    let t = (
        1, // Trailing the first element.
        0,
    );
}

fn comment_inside_closure_arg() {
    foo(|x| {
        // c
        x
    });
}

fn comment_inside_closure_in_tuple() {
    let t = (1, |x| {
        // c
        x
    });
}

fn comment_inside_nested_call() {
    foo(bar(
        // c
        1,
    ));
}
