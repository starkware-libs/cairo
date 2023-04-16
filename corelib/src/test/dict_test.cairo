use dict::Felt252DictTrait;
use traits::Index;

#[test]
fn test_dict_new() -> Felt252Dict<felt252> {
    Felt252DictTrait::new()
}

#[test]
fn test_dict_squash_empty() {
    let mut dict: Felt252Dict<felt252> = Felt252DictTrait::new();
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
    // TODO(spapini): Use indexing operator.
    let val10 = dict.index(10);
    let val11 = dict.index(11);
    let val12 = dict.index(12);
    assert(val10 == 110, 'dict[10] == 110');
    assert(val11 == 111, 'dict[11] == 111');
    assert(val12 == 0, 'default_val == 0');
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
    let mut dict = Felt252DictTrait::new();

    dict.insert(KEY1, 1);
    dict.insert(KEY2, 2);
    dict.insert(KEY3, 3);
    dict.insert(KEY4, 4);
    dict.insert(KEY5, 5);

    // TODO(spapini): Use indexing operator.
    assert(dict.index(KEY1) == 1, 'KEY1');
    assert(dict.index(KEY2) == 2, 'KEY2');
    assert(dict.index(KEY3) == 3, 'KEY3');
    assert(dict.index(KEY4) == 4, 'KEY4');
    assert(dict.index(KEY5) == 5, 'KEY5');
}
