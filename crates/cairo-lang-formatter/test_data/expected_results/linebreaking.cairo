fn foo(x: T) -> S {
    // Cascaded dangling break
    let x1 = 1
        + 2
        + 3
        + 4
        + 5
        + 6
        + 7
        + 8
        + 9
        + 1
            * 2
            * 3
            * 4
            * 5
            * 6
            * 7
            * 8
            * 9
            * 1
            * 2
            * 3
            * 4
            * 5
            * 6
            * 7
            * 8
            * 9
            * 1
            * 2
            * 3
            * 4
            * 5
            * 6
            * 7
            * 8
            * 9
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
    // Non-dangling break (overridden)
    let x2 = a_very_very_very_very_very_very_very_long_name()
        + a_very_very_very_very_very_very_very_long_name();
    let x3 = (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9)
        + (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9)
        + (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9)
        + (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
    let x4 = (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9)
        + (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9)
        + (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9)
        + (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
    let x5 = (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9)
        + (1
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
            + 9)
        + (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9)
        + (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
    let x6 = (1
        + 0
        + (2
            + 0
            + (3
                + 0
                + (4
                    + 0
                    + (5
                        + 0
                        + (6
                            + 0
                            + (7
                                + 0
                                + (8 + 0 + (9 + 0 + (1 + 0 + (2 + 0 + (3 + 0 + (4 + 0)))))))))))));
    for i in 1..2 {}
    for i in 1 + 2 + 3 + 4 + 1 + 2 + 3 + 4 + 1 + 2 + 3 + 4 + 1 + 2 + 3 + 4 + 1 + 2 + 3 + 4
        ..1 + 2 + 3 + 4 + 1 + 2 + 3 + 4 + 1 + 2 + 3 + 4 + 1 + 2 + 3 + 4 {}
    for (x, y) in array![
        (10, 10), (11, 11), (12, 12), (13, 13), (14, 14), (15, 15), (16, 16), (17, 17),
    ] {
        do_something!(x, i);
    };
}

fn bar(
    first_arg: T,
    second_arg: T,
    third_arg: T,
    fourth_arg: T,
    fifth_arg: T,
    sixth_arg: T,
    seventh_arg: T,
) -> T {
    let x = Struct {
        first_arg: first_arg,
        second_arg: second_arg,
        third_arg: third_arg,
        fourth_arg: fourth_arg,
        fifth_arg: fifth_arg,
    };
    let y = Struct {
        first_arg: SubStruct { first_arg: first_arg, second_arg: second_arg, third_arg: third_arg },
        second_arg: SubStruct {
            first_arg: first_arg,
            second_arg: second_arg,
            third_arg: third_arg,
            fourth_arg: fourth_arg,
        },
    };
    let some_tuple = (
        first_arg, second_arg, third_arg, fourth_arg, fifth_arg, sixth_arg, seventh_arg,
    );
    let rec_tuple = (
        (first_arg, second_arg, third_arg, fourth_arg, fifth_arg, sixth_arg, seventh_arg),
        (
            first_arg, second_arg, third_arg, fourth_arg, fifth_arg, sixth_arg, seventh_arg,
            eighth_arg,
        ),
    );
    match 5 {
        1 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 |
        21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 |
        21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 |
        21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 2 | 3 => 4,
    }
    let foo = |
        first_arg: T,
        second_arg: T,
        third_arg: T,
        fourth_arg: T,
        fifth_arg: T,
        sixth_arg: T,
        seventh_arg: T,
    | {
        1
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
    };
}

impl AnImpl<
    T,
    impl i: ATrait<
        T,
        S,
        AVeryVeryVeryVerVeryVeryVeryVeryVeryVeryVeryVeryLongOne,
        AVeryVeryVeryVerVeryVeryVeryVeryVeryVeryVeryVeryLongOne,
        AVeryVeryVeryVerVeryVeryVeryVeryVeryVeryVeryVeryLongOne,
    >,
> of ATrait {}

/// A comment that should be broken because it is too long to fit in a single line. Some more words
/// to make it longer. And even longer as we want it to be broken into three lines. Lorem ipsum
/// dolor sit amet.
fn function_for_a_comment() {
    // A comment that should be broken because it is too long to fit in a single line. Some more
    // words to make it longer. And even longer as we want it to be broken into three lines. Lorem
    // ipsum dolor sit amet.
    let x = 1;
}

// A comment that should be broken because it is too long to fit in a single line. and the broken
// line should be added to the next line, and even longer as we want it to be broken into three
// lines. Lorem ipsum dolor sit amet.
fn function_for_a_comment() {}

// A comment that should be broken because it is too long to fit in a single line. and the broken
// line should not be added to the next line.
// Because it ended with a period.
fn function_for_a_comment() {}

// A comment that should be broken because it is too long to fit in a single line. and the broken
// line should not be added to the next line,
/// Because the comment prefix is different.
fn function_for_a_comment() {
    // A comment that should be broken because it is too long to fit in a single line. and the
    // broken line should not be added to the next line,
    //     Because the comment prefix is different (indented).
    let x = 1;
}

// leading words
// long_single_word_that_should_not_be_broken_not_creating_additional_empty_lines_padding_padding_padding_padding
fn function_for_a_comment() {}

use long_use::{
    a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
    a, a, a, a, a, a, a, a, a, a, a, a, a, a,
};
fn many_arguments(
    very_long_argument_1: T,
    very_long_argument_2: T,
    very_long_argument_3: T,
    very_long_argument_4: T,
    very_long_argument_5: T,
) {}
fn small_tuple() {
    (1,)
}
fn big_tuple() {
    (
        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111,
    )
}
fn foo() {
    let arr1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    let arr2 = [
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    ];
}

use long_use::{
    a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
    a, a, a, a, a, a, a, a, a, a, a, a, a, a,
};
fn many_arguments(
    very_long_argument_1: T,
    very_long_argument_2: T,
    very_long_argument_3: T,
    very_long_argument_4: T,
    very_long_argument_5: T,
) {}
fn small_tuple() {
    (1,)
}
fn big_tuple() {
    (
        11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111,
    )
}
fn comment_remains_after_last_comma() {
    let mut arr = array![
        1, // First comment.
        0 // Second comment that needs to remain although it follows a comma.
    ];
}
fn struct_line_breaking() {
    let MyStruct {
        long_long_long_long_key_a,
        long_long_long_long_key_b,
        long_long_long_long_key_c,
        long_long_long_long_key_d,
        long_long_long_long_key_e,
    } = my_val;
}
fn closure_line_breaking() {
    let closure_with_a_very_very_very_very_very_very_very_very_large_name = |
        left: usize, right: usize,
    | -> usize {
        left + right
    };
}
