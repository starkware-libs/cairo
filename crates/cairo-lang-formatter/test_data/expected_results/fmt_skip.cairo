// Skip the formatting of this function.
#[cairofmt::skip]
fn foo(
a: i32, 
b: i32) 
-> 
i32 {
a + b
}

// Skip the formatting of certain statements.
fn bar() {
    #[cairofmt::skip]
    let a = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 +
            1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 +
            1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 +
            1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 +
            1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9;
    let b = 1
        + 2
        + 3
        + 4
        + 5
        + 6
        + 7
        + 8
        + 9
        + 1
        + 2
        + 3
        + 4
        + 5
        + 6
        + 7
        + 8
        + 9
        + 1
        + 2
        + 3
        + 4
        + 5
        + 6
        + 7
        + 8
        + 9
        + 1
        + 2
        + 3
        + 4
        + 5
        + 6
        + 7
        + 8
        + 9
        + 1
        + 2
        + 3
        + 4
        + 5
        + 6
        + 7
        + 8
        + 9;
    #[cairofmt::skip]
    let a = array![1, 2, 3, 4, 5, 6, 7, 8, 9,
                   1, 2, 3, 4, 5, 6, 7, 8, 9,
                   1, 2, 3, 4, 5, 6, 7, 8, 9,
                   1, 2, 3, 4, 5, 6, 7, 8, 9];
    let b = array![
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9
    ];
}
// Skip the formatting of a struct.
#[cairofmt::skip]
struct MyStruct   {
a: i32, b: i32,
}
// Skip the formatting of a module.
#[cairofmt::skip]
mod mod1 {
fn foo() {let x=1   ;}
}
