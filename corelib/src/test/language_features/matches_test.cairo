#[test]
fn test_basic_match_case() {
    let a = Some(1);
    if !matches!(a, Some(_)) {
        panic!("Expected `a` to be Option::Some, not Option::None");
    }
}

enum TestEnum {
    A,
    B,
}

enum TestEnumWithData {
    A: u32,
    B: u32,
}

#[test]
fn test_enum_match_case() {
    let a = TestEnum::A;
    if !matches!(a, TestEnum::A) {
        panic!("Expected `a` to be TestEnum::A");
    }
}

#[test]
fn test_enum_with_data_match_case() {
    let a = TestEnumWithData::A(1);
    if !matches!(a, TestEnumWithData::A(_)) {
        panic!("Expected `a` to be TestEnumWithData::A");
    }
}
