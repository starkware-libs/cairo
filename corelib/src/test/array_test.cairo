use array::ArrayTCloneImpl;
use array::ArrayTrait;
use array::SpanIndex;
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
    assert(*arr[0_usize] == 10, 'array[0] == 10');
    assert(*arr[1_usize] == 11, 'array[1] == 11');
    assert(*arr[2_usize] == 12, 'array[2] == 12');
}

#[test]
#[should_panic]
fn test_array_out_of_bound_1() {
    let arr = test_array_helper();
    arr[3_usize];
}

#[test]
#[should_panic]
fn test_array_out_of_bound_2() {
    let arr = test_array_helper();
    arr[11_usize];
}

#[test]
#[available_gas(100000)]
fn test_array_clone() {
    let felt252_snap_array = @test_array_helper();
    let felt252_snap_array_clone = felt252_snap_array.clone();
    assert(felt252_snap_array_clone.len() == 3_usize, 'array len == 3');
    assert(*felt252_snap_array_clone[0_usize] == 10, 'array[0] == 10');
    assert(*felt252_snap_array_clone[1_usize] == 11, 'array[1] == 11');
    assert(*felt252_snap_array_clone[2_usize] == 12, 'array[2] == 12');
}

#[test]
fn test_span() {
    let mut span = test_array_helper().span();

    assert(span.len() == 3_u32, 'Unexpected span length.');
    assert(*span.get(0_u32).unwrap().unbox() == 10, 'Unexpected element');
    assert(*span.pop_front().unwrap() == 10, 'Unexpected element');
    assert(span.len() == 2_u32, 'Unexpected span length.');
    assert(*span[1_u32] == 12, 'Unexpected element');
    assert(*span.pop_back().unwrap() == 12, 'Unexpected element');
    assert(span.len() == 1_u32, 'Unexpected span length.');
}

#[test]
fn test_slice() {
    let mut span = test_array_helper().span();
    assert(span.slice(0_usize, 3_usize).len() == 3_u32, 'Unexpected span length.');
    assert(*span.slice(0_usize, 3_usize)[0_usize] == 10, 'Unexpected Element.');
    assert(span.slice(0_usize, 2_usize).len() == 2_u32, 'Unexpected span length.');
    assert(*span.slice(0_usize, 2_usize)[0_usize] == 10, 'Unexpected Element.');
    assert(span.slice(0_usize, 1_usize).len() == 1_u32, 'Unexpected span length.');
    assert(*span.slice(0_usize, 1_usize)[0_usize] == 10, 'Unexpected Element.');
    assert(span.slice(0_usize, 0_usize).len() == 0_u32, 'Unexpected span length.');
    assert(span.slice(1_usize, 2_usize).len() == 2_u32, 'Unexpected span length.');
    assert(*span.slice(1_usize, 2_usize)[0_usize] == 11, 'Unexpected Element.');
    assert(span.slice(1_usize, 1_usize).len() == 1_u32, 'Unexpected span length.');
    assert(*span.slice(1_usize, 1_usize)[0_usize] == 11, 'Unexpected Element.');
    assert(span.slice(1_usize, 0_usize).len() == 0_u32, 'Unexpected span length.');
}

#[test]
#[should_panic]
fn test_slice_out_of_bound_1() {
    test_array_helper().span().slice(3_u32, 1_u32);
}

#[test]
#[should_panic]
fn test_slice_out_of_bound_2() {
    test_array_helper().span().slice(0_u32, 4_u32);
}
