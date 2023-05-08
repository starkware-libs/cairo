use array::ArrayTrait;
use array::SpanTrait;
use starknet::ContractAddress;

use super::utils::serialized_element;
use super::utils::single_deserialize;


#[derive(Drop,storage_access::StorageAccess)]
struct Mu {
    a: u32
}


#[derive(storage_access::StorageAccess)]
struct KuKu {
    a: u32,
    b: u256,
    c: Mu
}
