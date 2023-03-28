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
    let val10 = dict.get(10);
    let val11 = dict.get(11);
    let val12 = dict.get(12);
    assert(val10 == 110, 'dict[10] == 110');
    assert(val11 == 111, 'dict[11] == 111');
    assert(val12 == 0, 'default_val == 0');
}
