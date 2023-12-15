use core::dict::Felt252DictEntryTrait;
use core::test::test_utils::{assert_eq, assert_ne};
use core::nullable;

#[test]
fn test_dict_new() -> Felt252Dict<felt252> {
    Default::default()
}

#[test]
fn test_dict_squash_empty() {
    let mut dict: Felt252Dict<felt252> = Default::default();
    let _squashed_dict = dict.squash();
}

#[test]
fn test_dict_default_val() {
    let mut dict: Felt252Dict = Default::default();
    let default_val = dict.get(0);
    assert_eq!(default_val, 0);
}

#[test]
fn test_dict_write_read() {
    let mut dict = Default::default();
    dict.insert(10, 110);
    dict.insert(11, 111);
    let val10 = dict[10];
    let val11 = dict[11];
    let val12 = dict[12];
    assert_eq!(val10, 110);
    assert_eq!(val11, 111);
    assert_eq!(val12, 0);
}

#[test]
fn test_dict_entry() {
    let mut dict = Default::default();
    dict.insert(10, 110);
    let (entry, value) = dict.entry(10);
    assert_eq!(value, 110);
    let mut dict = entry.finalize(11);
    assert_eq!(dict[10], 11);
}

#[test]
fn test_dict_entry_uninitialized() {
    let mut dict = Default::default();
    let (entry, value) = dict.entry(10);
    assert_eq!(value, 0_felt252);
    let mut dict = entry.finalize(110);
    assert_eq!(dict[10], 110);
}

#[test]
fn test_dict_update_twice() {
    let mut dict = Default::default();
    dict.insert(10, 110);
    let (entry, value) = dict.entry(10);
    assert_eq!(value, 110);
    dict = entry.finalize(11);
    assert_eq!(dict[10], 11);
    let (entry, value) = dict.entry(10);
    assert_eq!(value, 11);
    dict = entry.finalize(12);
    assert_eq(@dict[10], @12, 'dict[10] == 12');
}


/// Tests the destruction of a non-finalized `Felt252DictEntry`.
///
/// Calls the destructor of the entry, which in turn calls the destructor of the `Felt252Dict`.
#[test]
fn test_dict_entry_destruct() {
    let mut dict = Default::default();
    dict.insert(10, 110);
    let (_entry, _value) = dict.entry(10);
}

const KEY1: felt252 = 10;
const KEY2: felt252 = 21;
// KEY3 is ~37% * PRIME.
const KEY3: felt252 = 1343531647004637707094910297222796970954128321746173119103571679493202324940;
// KEY4 and KEY5 are ~92% * PRIME.
const KEY4: felt252 = 3334603141101959564751596861783084684819726025596122159217101666076094555684;
const KEY5: felt252 = 3334603141101959564751596861783084684819726025596122159217101666076094555685;

/// Tests the big-keys behavior of `felt252_dict_squash()`.
///
/// Uses a few keys to simulate the 3 possible cases in `validate_felt252_le`.
#[test]
fn test_dict_big_keys() {
    let mut dict = Default::default();

    dict.insert(KEY1, 1);
    dict.insert(KEY2, 2);
    dict.insert(KEY3, 3);
    dict.insert(KEY4, 4);
    dict.insert(KEY5, 5);

    assert_eq(@dict[KEY1], @1, 'KEY1');
    assert_eq(@dict[KEY2], @2, 'KEY2');
    assert_eq(@dict[KEY3], @3, 'KEY3');
    assert_eq(@dict[KEY4], @4, 'KEY4');
    assert_eq(@dict[KEY5], @5, 'KEY5');
}

#[test]
fn test_dict_of_nullable() {
    let mut dict = Default::default();
    dict.insert(10, nullable::nullable_from_box(BoxTrait::new(1)));
    dict.insert(11, nullable::nullable_from_box(BoxTrait::new(2)));
    let val10 = dict[10].deref();
    let val11 = dict[11].deref();
    let val12 = dict[12];
    assert_eq(@val10, @1, 'dict[10] == 1');
    assert_eq(@val11, @2, 'dict[11] == 2');
    assert(
        match nullable::match_nullable(val12) {
            nullable::FromNullableResult::Null => true,
            nullable::FromNullableResult::NotNull(_) => false,
        },
        'default_val == null'
    );
}

#[test]
fn test_bool_dict() {
    let mut bool_dict: Felt252Dict<bool> = Default::default();
    let _squashed_dict = bool_dict.squash();
    let mut bool_dict: Felt252Dict<bool> = Default::default();
    assert(!bool_dict.get(0), 'default_val != false');
    bool_dict.insert(1, true);
    assert(bool_dict.get(1), 'bool_dict[1] != true');
}

#[test]
fn test_array_dict() {
    let mut dict = Default::default();
    dict.insert(10, NullableTrait::new(array![1, 2, 3]));
    let (entry, value) = dict.entry(10);
    assert_eq(@value.deref(), @array![1, 2, 3], 'dict[10] == [1, 2, 3]');
    dict = entry.finalize(NullableTrait::new(array![4, 5]));
    let (entry, value) = dict.entry(10);
    assert_eq(@value.deref(), @array![4, 5], 'dict[10] == [4, 5]');
    dict = entry.finalize(nullable::null());
    let (_entry, value) = dict.entry(10);
    assert(value.is_null(), 'dict[10] == null');
}

