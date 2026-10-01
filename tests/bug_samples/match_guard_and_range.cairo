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

fn bucket(x: u8) -> felt252 {
    match x {
        0..10 => 'small',
        10..=19 => 'medium',
        20..=29 if x % 2 == 0 => 'even',
        20..=29 => 'odd',
        _ => 'big',
    }
}

fn first_long(values: Option<Array<u32>>) -> u32 {
    match values {
        Some(arr) if arr.len() > 1 => *arr[0],
        Some(_) => 1,
        None => 0,
    }
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
fn test_range_patterns() {
    assert_eq!(bucket(0), 'small');
    assert_eq!(bucket(9), 'small');
    assert_eq!(bucket(10), 'medium');
    assert_eq!(bucket(19), 'medium');
    assert_eq!(bucket(20), 'even');
    assert_eq!(bucket(21), 'odd');
    assert_eq!(bucket(29), 'odd');
    assert_eq!(bucket(30), 'big');
}

#[test]
fn test_guard_on_non_copy_payload() {
    assert_eq!(first_long(Some(array![7, 8])), 7);
    assert_eq!(first_long(Some(array![7])), 1);
    assert_eq!(first_long(None), 0);
}
