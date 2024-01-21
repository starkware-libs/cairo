use starknet::ContractAddress;

#[test]
fn test_contract_address_ordering() {
    let addr10000: ContractAddress = 0x10000.try_into().unwrap();
    let addr20000: ContractAddress = 0x20000.try_into().unwrap();
    assert!(addr10000 < addr20000);
    assert!(addr10000 <= addr20000);
    assert!(!(addr10000 > addr20000));
    assert!(!(addr10000 >= addr20000));
    assert!(!(addr10000 < addr10000));
    assert!(addr10000 <= addr10000);
    assert!(!(addr10000 > addr10000));
    assert!(addr10000 >= addr10000);
}
