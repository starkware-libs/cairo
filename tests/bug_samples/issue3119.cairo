fn index_of_min<T, +PartialEq<T>, +PartialOrd<T>, +Drop<T>, +Copy<T>>(
    mut span: Span<T>,
) -> Option<usize> {
    let mut current_index = 0;
    let mut min_index = 0;
    let mut min = match span.pop_front() {
        Some(item) => *item,
        None => { return None; },
    };
    loop {
        match span.pop_front() {
            Some(item) => {
                if *item < min {
                    min = *item;
                    current_index = current_index + 1;
                    min_index = current_index;
                }
            },
            None => { break Some(min_index); },
        }
    }
}

#[test]
fn issue_3119() {
    let _min_index = index_of_min(array![5_usize, 4, 1, 2, 3].span());
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
fn simplified_3119() {
    let mut z = 0;
    let mut y = 0;
    foo(z, y);
}
