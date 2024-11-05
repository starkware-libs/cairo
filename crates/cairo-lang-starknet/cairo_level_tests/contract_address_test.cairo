use starknet::ContractAddress;

#[test]
fn test_contract_address_ordering() {
    let addr10000: ContractAddress = starknet::const_value::<0x10000>();
    let addr20000: ContractAddress = starknet::const_value::<0x20000>();
    assert!(addr10000 < addr20000);
    assert!(addr10000 <= addr20000);
    assert!(!(addr10000 > addr20000));
    assert!(!(addr10000 >= addr20000));
    assert!(!(addr10000 < addr10000));
    assert!(addr10000 <= addr10000);
    assert!(!(addr10000 > addr10000));
    assert!(addr10000 >= addr10000);
}
