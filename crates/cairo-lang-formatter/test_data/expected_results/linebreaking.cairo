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
        fifth_arg: fifth_arg
    };
    let y = Struct {
        first_arg: SubStruct { first_arg: first_arg, second_arg: second_arg, third_arg: third_arg },
        second_arg: SubStruct {
            first_arg: first_arg,
            second_arg: second_arg,
            third_arg: third_arg,
            fourth_arg: fourth_arg
        }
    };
    let some_tuple = (
        first_arg, second_arg, third_arg, fourth_arg, fifth_arg, sixth_arg, seventh_arg
    );
    let rec_tuple = (
        (first_arg, second_arg, third_arg, fourth_arg, fifth_arg, sixth_arg, seventh_arg),
        (
            first_arg,
            second_arg,
            third_arg,
            fourth_arg,
            fifth_arg,
            sixth_arg,
            seventh_arg,
            eighth_arg
        )
    );
    match 5 {
        1 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 |
        21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 |
        21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 |
        21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 21 | 2 | 2 | 3 => 4,
    }
    let foo = |first_arg: T,
    second_arg: T,
    third_arg: T,
    fourth_arg: T,
    fifth_arg: T,
    sixth_arg: T,
    seventh_arg: T| {
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
