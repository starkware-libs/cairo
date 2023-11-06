#[derive(Copy, Drop)]
struct A {
    value: felt252,
}

#[derive(Copy, Drop)]
struct B {
    a: A,
}


#[test]
fn test_member_of_local_struct() {
    let a = A { value: 12 };
    test_member_of_local_struct_inner(a);
}

fn test_member_of_local_struct_inner(a: A) -> felt252 {
    let value = a.value;
    core::internal::revoke_ap_tracking();
    value
}


#[test]
fn member_of_temp_struct() -> felt252 {
    let a = A { value: 12 };
    let x = a.value;
    core::internal::revoke_ap_tracking();
    x
}

fn get_value_a(a: A) -> felt252 {
    a.value
}


#[test]
fn member_and_struct_of_temp_struct() -> felt252 {
    let a = A { value: 12 };
    let x = a.value;
    core::internal::revoke_ap_tracking();
    get_value_a(a) + x
}


#[test]
fn test_member_of_member_of_local_struct() {
    let b = B { a: A { value: 12 } };
    test_member_of_member_of_local_struct_inner(b);
}

fn test_member_of_member_of_local_struct_inner(b: B) -> felt252 {
    let value = b.a.value;
    core::internal::revoke_ap_tracking();
    value
}


#[test]
fn member_of_member_of_temp_struct() -> felt252 {
    let b = B { a: A { value: 12 } };
    let x = b.a.value;
    core::internal::revoke_ap_tracking();
    x
}

fn get_value_b(b: B) -> felt252 {
    b.a.value
}


#[test]
fn member_of_member_and_struct_of_temp_struct() -> felt252 {
    let b = B { a: A { value: 12 } };
    let a = b.a;
    let x = a.value;
    core::internal::revoke_ap_tracking();
    get_value_a(a) + get_value_b(b) + x
}
