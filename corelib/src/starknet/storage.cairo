/// Trait for any contract/component storage member.
trait StorageMemberStateTrait<TMemberState, TValue> {
    fn address(self: @TMemberState) -> starknet::StorageBaseAddress;
    fn read(self: @TMemberState) -> TValue;
    fn write(ref self: TMemberState, value: TValue);
}
/// Trait for any contract/component mapping storage member.
trait StorageMapMemberStateTrait<TMemberState, TKey, TValue> {
    fn address(self: @TMemberState, key: TKey) -> starknet::StorageBaseAddress;
    fn read(self: @TMemberState, key: TKey) -> TValue;
    fn write(ref self: TMemberState, key: TKey, value: TValue);
}
