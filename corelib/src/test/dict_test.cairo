use dict::Felt252DictTrait;
use dict::Felt252DictEntry;
use dict::felt252_dict_entry_finalize;
use dict::felt252_dict_entry_clear;

#[test]
fn test_dict_new() -> Felt252Dict::<felt252> {
    Felt252DictTrait::new()
}

#[test]
fn test_dict_squash_empty() {
    let mut dict: Felt252Dict::<felt252> = Felt252DictTrait::new();
    let squashed_dict = dict.squash();
}

#[test]
fn test_dict_default_val() {
    let mut dict = Felt252DictTrait::new();
    let default_val = dict.get(0);
    assert(default_val == 0, 'default_val == 0');
}

#[test]
fn test_dict_write_read() {
    let mut dict = Felt252DictTrait::new();
    dict.insert(10, 110);
    dict.insert(11, 111);
    let val10 = dict[10];
    let val11 = dict[11];
    let val12 = dict[12];
    assert(val10 == 110, 'dict[10] == 110');
    assert(val11 == 111, 'dict[11] == 111');
    assert(val12 == 0, 'default_val == 0');
}

#[test]
fn test_dict_entry() {
    let mut dict = Felt252DictTrait::new();
    dict.insert(10, 110);
    let (entry, value) = dict.entry(10);
    assert(value == 110, 'dict[10] == 110');
    let mut dict = felt252_dict_entry_finalize(entry, 11);
    assert(dict[10] == 11, 'dict[10] == 11');
}

#[test]
fn test_dict_entry_clear() {
    let mut dict = Felt252DictTrait::new();
    dict.insert(10, 110);
    let (entry, value) = dict.entry(10);
    assert(value == 110, 'dict[10] == 110');
    let mut dict = felt252_dict_entry_clear(entry);
    assert(dict[10] == 0, 'dict[10] == 0');
}

#[test]
fn test_dict_entry_uninitialized() {
    let mut dict = Felt252DictTrait::new();
    let (entry, value) = dict.entry(10);
    assert(value == 0, 'dict[10] == 0');
    let mut dict = felt252_dict_entry_finalize(entry, 110);
    assert(dict[10] == 110, 'dict[10] == 0');
}

#[test]
fn test_dict_update_twice() {
    let mut dict = Felt252DictTrait::new();
    dict.insert(10, 110);
    let (entry, value) = dict.entry(10);
    assert(value == 110, 'dict[10] == 110');
    let mut dict = felt252_dict_entry_finalize(entry, 11);
    assert(dict[10] == 11, 'dict[10] == 11');
    let (entry, value) = dict.entry(10);
    assert(value == 11, 'dict[10] == 11');
    let mut dict = felt252_dict_entry_finalize(entry, 12);
    assert(dict[10] == 12, 'dict[10] == 12');
}
