#[test]
fn test_tuple_index_read() {
    let t = (1_felt252, 2_u8, 3_u16);
    assert_eq!(t.0, 1);
    assert_eq!(t.1, 2);
    assert_eq!(t.2, 3);
}

#[test]
fn test_tuple_index_nested_read() {
    let t = (1_felt252, (2_u8, 3_u16));
    assert_eq!(t.0, 1);
    assert_eq!(t.1.0, 2);
    assert_eq!(t.1.1, 3);
}

#[test]
fn test_tuple_index_assignment() {
    let mut t = (1_felt252, 2_u8);
    t.0 = 5;
    t.1 = 7;
    assert_eq!(t.0, 5);
    assert_eq!(t.1, 7);
}

#[test]
fn test_tuple_index_nested_assignment() {
    let mut t = (1_felt252, (2_u8, 3_u16));
    t.1.0 = 20;
    t.1.1 = 30;
    assert_eq!(t.0, 1);
    assert_eq!(t.1.0, 20);
    assert_eq!(t.1.1, 30);
}

fn set_to_seven(ref x: felt252) {
    x = 7;
}

#[test]
fn test_tuple_index_ref_argument() {
    let mut t = (1_felt252, 2_u8);
    set_to_seven(ref t.0);
    assert_eq!(t.0, 7);
    assert_eq!(t.1, 2);
}

#[test]
fn test_tuple_index_through_snapshot() {
    let t = (1_felt252, 2_u8);
    let s = @t;
    // In the corelib edition, member access through a snapshot auto-desnaps the element.
    assert_eq!(s.0, 1);
    assert_eq!(s.1, 2);
}
