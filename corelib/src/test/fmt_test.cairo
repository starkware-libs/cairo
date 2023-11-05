#[test]
fn test_format() {
    let ba: ByteArray = "hello";
    assert(format!("{}", ba) == ba, 'byte array bad formatting');
    assert(format!("{}", 97_felt252) == "97", 'felt252 bad formatting');
    assert(format!("{}", 97_usize) == "97", 'usize bad formatting');
    assert(format!("no_format") == "no_format", 'no args bad formatting');
    assert(format!("{}{}", 12_usize, 14_u32) == "1214", 'two args bad formatting');
    assert(format!("{0}{0}", 12_usize) == "1212", 'positional bad formatting');
    assert(format!("{}{1}", 12_usize, 14_u32) == "1214", 'positional mix bad formatting');
    assert(format!("{ba}_{}_{}_{1}", 12, 14_u32) == "hello_12_14_14", 'full mix bad formatting');
    assert(format!("{{{{}}}}") == "{{}}", 'special cases bad formatting');
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
    assert(format!("{:?}", ba) == "\"hello\"", 'byte array bad formatting');
    assert(format!("{:?}", 97_felt252) == "97", 'felt252 bad formatting');
    assert(format!("{:?}", 97_usize) == "97", 'usize bad formatting');
    assert(format!("{:?}{:?}", 12_usize, 14_u32) == "1214", 'two args bad formatting');
    assert(format!("{0:?}{0:?}", 12_usize) == "1212", 'positional bad formatting');
    assert(format!("{:?}{1:?}", 12_usize, 14_u32) == "1214", 'positional mix bad formatting');
    assert(
        format!("{ba:?}_{:?}_{:?}_{1:?}", 12, 14_u32) == "\"hello\"_12_14_14",
        'full mix bad formatting'
    );
    assert(
        format!("{:?}", EnumExample::Empty) == "EnumExample::Empty(())", 'bad enum empty derive fmt'
    );
    assert(
        format!("{:?}", EnumExample::FeltValue(4)) == "EnumExample::FeltValue(4)",
        'bad derive enum fmt'
    );
    assert(
        format!(
            "{:?}",
            StructExample {
                felt_value: 6,
                bool_value: false,
                byte_array_value: "ByteArray",
                enum_value: EnumExample::BoolValue(true)
            }
        ) == "StructExample { felt_value: 6, bool_value: false, byte_array_value: \"ByteArray\", enum_value: EnumExample::BoolValue(true) }",
        'bad derive struct formatting'
    );
}
