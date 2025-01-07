use starknet::ClassHash;
#[starknet::interface]
pub trait IUpgradable<TCS> {
    fn upgrade(ref self: TCS, new_class_hash: ClassHash);
}
#[starknet::component]
pub mod upgradable {
    use ownable_comp::OwnableHelperImpl;
    use starknet::ClassHash;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use starknet::syscalls::replace_class_syscall;
    use crate::components::ownable::ownable as ownable_comp;
    #[storage]
    pub struct Storage {
        pub current_implementation: ClassHash,
    }
    #[event]
    #[derive(Drop, starknet::Event)]
    pub enum Event {
        ContractUpgraded: ContractUpgraded,
    }
    #[derive(Drop, starknet::Event)]
    struct ContractUpgraded {
        old_class_hash: ClassHash,
        new_class_hash: ClassHash,
    }
    #[embeddable_as(UpgradableImpl)]
    impl Upgradable<
        TContractState,
        +HasComponent<TContractState>,
        impl Ownable: ownable_comp::HasComponent<TContractState>,
    > of super::IUpgradable<ComponentState<TContractState>> {
        fn upgrade(ref self: ComponentState<TContractState>, new_class_hash: ClassHash) {
            Ownable::get_component(self.get_contract()).validate_ownership();
            replace_class_syscall(new_class_hash).unwrap();
            let old_class_hash = self.current_implementation.read();
            self.emit(ContractUpgraded { old_class_hash, new_class_hash });
            self.current_implementation.write(new_class_hash);
        }
    }
}
