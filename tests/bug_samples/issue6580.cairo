#[derive(Drop, Debug)]
struct SomeStruct {
    num: felt252,
}

#[test]
fn inner_index_access() {
    let arr_of_bytearrays: Array<ByteArray> = array!["str1", "str2", "str3"];
    let arr_of_arrays_of_nums: Array<Array<u64>> = array![array![9], array![2], array![19, 20, 21]];
    let arr_of_arrays_of_structs: Array<Array<SomeStruct>> = array![
        array![SomeStruct { num: 8 }], array![SomeStruct { num: 2 }],
        array![SomeStruct { num: 9 }, SomeStruct { num: 10 }, SomeStruct { num: 11 }],
    ];

    let index_1 = 2_usize;
    let index_2 = 2_usize;

    let _nested_item = arr_of_arrays_of_nums[index_1][index_2];
    let _char = arr_of_bytearrays[index_1][index_2];
    let _some_struct = arr_of_arrays_of_structs[index_1][index_2];
}
