#[test]
fn test_format() {
    let ba: ByteArray = "hello";
    assert!(format!("{}", ba) == ba);
    assert!(format!("{}", 97_felt252) == "97");
    assert!(format!("{}", 97_usize) == "97");
    assert!(format!("no_format") == "no_format");
    assert!(format!("{}{}", 12_usize, 14_u32) == "1214");
    assert!(format!("{0}{0}", 12_usize) == "1212");
    assert!(format!("{}{1}", 12_usize, 14_u32) == "1214");
    assert!(format!("{ba}_{}_{}_{1}", 12, 14_u32) == "hello_12_14_14");
    assert!(format!("{{{{}}}}") == "{{}}");
}

#[derive(Debug, Drop)]
struct StructExample {
    felt_value: felt252,
    bool_value: bool,
    byte_array_value: ByteArray,
    enum_value: EnumExample,
}

#[derive(Debug, Drop)]
enum EnumExample {
    Empty,
    FeltValue: felt252,
    BoolValue: bool,
}

#[test]
fn test_format_debug() {
    let ba: ByteArray = "hello";
    assert!(format!("{:?}", ba) == "\"hello\"");
    assert!(format!("{:?}", 97_felt252) == "97");
    assert!(format!("{:?}", 97_usize) == "97");
    assert!(format!("{:?}{:?}", 12_usize, 14_u32) == "1214");
    assert!(format!("{0:?}{0:?}", 12_usize) == "1212");
    assert!(format!("{:?}{1:?}", 12_usize, 14_u32) == "1214");
    assert!(format!("{ba:?}_{:?}_{:?}_{1:?}", 12, 14_u32) == "\"hello\"_12_14_14");
    assert!(format!("{:?}", EnumExample::Empty) == "EnumExample::Empty(())");
    assert!(format!("{:?}", EnumExample::FeltValue(4)) == "EnumExample::FeltValue(4)");
    assert!(
        format!(
            "{:?}",
            StructExample {
                felt_value: 6,
                bool_value: false,
                byte_array_value: "ByteArray",
                enum_value: EnumExample::BoolValue(true)
            }
        ) == "StructExample { felt_value: 6, bool_value: false, byte_array_value: \"ByteArray\", enum_value: EnumExample::BoolValue(true) }"
    );
}
