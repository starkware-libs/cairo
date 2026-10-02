enum Shape {
    Circle: u32,
    Square: u32,
    Point,
}

fn classify(shape: Shape, large_only: bool) -> felt252 {
    match shape {
        Shape::Circle(r) | Shape::Square(r) if r > 100 => 'huge',
        Shape::Circle(r) if large_only && r > 10 => 'large circle',
        Shape::Circle(_) => 'circle',
        Shape::Square(_) if large_only => 'skipped square',
        Shape::Square(_) => 'square',
        Shape::Point => 'point',
    }
}

fn sum(values: Array<u32>) -> u32 {
    let mut total = 0;
    for value in values {
        total += value;
    }
    total
}

fn first_or_sum(values: Option<Array<u32>>) -> u32 {
    match values {
        Some(arr) if arr.len() > 2 => *arr[0],
        Some(arr) => sum(arr),
        None => 0,
    }
}

fn count_checks(ref checks: u32, limit: u32) -> bool {
    checks += 1;
    checks > limit
}

fn pick(value: Option<u32>, limit: u32) -> (u32, u32) {
    let mut checks = 0;
    let picked = match value {
        Some(x) if count_checks(ref checks, limit) => x,
        Some(x) if count_checks(ref checks, limit) => x + 100,
        Some(_) => 0,
        None => 1,
    };
    (picked, checks)
}

fn either_is_five(pair: (felt252, felt252)) -> felt252 {
    match pair {
        (x, _) | (_, x) if x == 5 => x,
        _ => 0,
    }
}

fn retried_guard_checks(pair: (u32, u32)) -> (u32, u32) {
    let mut checks = 0;
    let picked = match pair {
        (x, _) | (_, x) if count_checks(ref checks, 100) => x,
        _ => 0,
    };
    (picked, checks)
}

#[test]
fn test_guard_falls_through_to_next_arm() {
    assert_eq!(classify(Shape::Circle(500), false), 'huge');
    assert_eq!(classify(Shape::Square(500), false), 'huge');
    assert_eq!(classify(Shape::Circle(50), true), 'large circle');
    assert_eq!(classify(Shape::Circle(50), false), 'circle');
    assert_eq!(classify(Shape::Circle(5), true), 'circle');
    assert_eq!(classify(Shape::Square(5), true), 'skipped square');
    assert_eq!(classify(Shape::Square(5), false), 'square');
    assert_eq!(classify(Shape::Point, true), 'point');
}

#[test]
fn test_guard_reads_non_copy_payload_before_it_is_moved() {
    assert_eq!(first_or_sum(Some(array![7, 8, 9])), 7);
    assert_eq!(first_or_sum(Some(array![7, 8])), 15);
    assert_eq!(first_or_sum(None), 0);
}

#[test]
fn test_guard_changes_outer_variable() {
    assert_eq!(pick(Some(5), 0), (5, 1));
    assert_eq!(pick(Some(5), 1), (105, 2));
    assert_eq!(pick(Some(5), 2), (0, 2));
    assert_eq!(pick(None, 0), (1, 0));
}

#[test]
fn test_guard_is_retried_for_each_matching_alternative() {
    assert_eq!(either_is_five((5, 1)), 5);
    assert_eq!(either_is_five((1, 5)), 5);
    assert_eq!(either_is_five((1, 2)), 0);
    assert_eq!(retried_guard_checks((1, 2)), (0, 2));
}
