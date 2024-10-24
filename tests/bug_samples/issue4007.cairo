#[starknet::interface]
trait ISomeInterface<TContractState> {
    fn some_lib_call(self: @TContractState);
}

#[test]
#[should_panic(expected: ('CLASS_HASH_NOT_DECLARED',))]
fn call_not_declared() {
    ISomeInterfaceLibraryDispatcher { class_hash: 32.try_into().unwrap() }.some_lib_call();
}
