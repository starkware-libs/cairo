use array::{SpanTrait, ArrayTrait};

fn index_of_min<
    T,
    impl TPartialEq: PartialEq<T>,
    impl TPartialOrd: PartialOrd<T>,
    impl TDrop: Drop<T>,
    impl TCopy: Copy<T>
>(
    mut span: Span<T>
) -> Option<usize> {
    let mut current_index = 0;
    let mut min_index = 0;
    let mut min = match span.pop_front() {
        Option::Some(item) => *item,
        Option::None(_) => {
            return Option::None(());
        },
    };
    loop {
        match span.pop_front() {
            Option::Some(item) => {
                if *item < min {
                    min = *item;
                    current_index = current_index + 1;
                    min_index = current_index;
                }
            },
            Option::None(_) => {
                break Option::Some(min_index);
            },
        };
    }
}

#[test]
#[available_gas(100000)]
fn issue_3119() {
    let min_index = index_of_min(array![5_usize, 4, 1, 2, 3].span());
}

fn foo(mut y: felt252, mut z: felt252) {
    if y == 1 {
        return;
    }
    if y == 0 {
        y = 1;
        z = y;
    }
    foo(y, z);
}

#[test]
#[available_gas(100000)]
fn simplified_3119() {
    let mut z = 0;
    let mut y = 0;
    foo(z, y);
}
