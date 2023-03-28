use array::ArrayTCloneImpl;
use array::ArrayTrait;
use array::SpanTrait;
use box::BoxTrait;
use clone::Clone;
use option::OptionTrait;

fn test_array_helper() -> Array::<felt252> {
    let mut arr = ArrayTrait::new();
    arr.append(10);
    arr.append(11);
    arr.append(12);
    arr
}

#[test]
fn test_array() {
    let arr = test_array_helper();
    assert(*arr.at(0_usize) == 10, 'array[0] == 10');
    assert(*arr.at(1_usize) == 11, 'array[1] == 11');
    assert(*arr.at(2_usize) == 12, 'array[2] == 12');
}

#[test]
#[should_panic]
fn test_array_out_of_bound_1() {
    let arr = test_array_helper();
    arr.at(3_usize);
}

#[test]
#[should_panic]
fn test_array_out_of_bound_2() {
    let arr = test_array_helper();
    arr.at(11_usize);
}

#[test]
#[available_gas(100000)]
fn test_array_clone() {
    let felt252_snap_array = @test_array_helper();
    let felt252_snap_array_clone = felt252_snap_array.clone();
    assert(felt252_snap_array_clone.len() == 3_usize, 'array len == 3');
    assert(*felt252_snap_array_clone.at(0_usize) == 10, 'array[0] == 10');
    assert(*felt252_snap_array_clone.at(1_usize) == 11, 'array[1] == 11');
    assert(*felt252_snap_array_clone.at(2_usize) == 12, 'array[2] == 12');
}

#[test]
fn test_span() {
    let mut span = test_array_helper().span();

    assert(span.len() == 3_u32, 'Unexpected span length.');
    assert(*span.get(0_u32).unwrap().unbox() == 10, 'Unexpected element');
    assert(*span.pop_front().unwrap() == 10, 'Unexpected element');
    assert(span.len() == 2_u32, 'Unexpected span length.');
    assert(*span.at(1_u32) == 12, 'Unexpected element');
    assert(*span.pop_back().unwrap() == 12, 'Unexpected element');
    assert(span.len() == 1_u32, 'Unexpected span length.');
}
