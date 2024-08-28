use core::starknet::SyscallResultTrait;

#[test]
fn test_store_opt_and_result_at_offset() {
    let base = starknet::storage_access::storage_base_address_const::<1000>();
    let v = (Option::Some(1_u256), Result::<_, u128>::Ok(2_u8), Option::Some(3_u64));
    starknet::Store::write(0, base, v).unwrap_syscall();
    assert_eq!(starknet::Store::read(0, base), Result::Ok(v));
}
