/// # Nonce Component
///
/// The Nonce component provides a simple mechanism for handling incremental nonce. It is commonly
/// used to prevent replay attacks when contracts accept signatures as input.
#[starknet::component]
pub mod OperatorNonceComponent {
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::introspection::src5::SRC5Component;
    use perpetuals::core::components::operator_nonce::interface::IOperatorNonce;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::components::roles::RolesComponent::InternalTrait as RolesInternal;


    #[storage]
    pub struct Storage {
        nonce: u64,
    }

    #[embeddable_as(OperatorNonceImpl)]
    impl OperatorNonceComponent<
        TContractState,
        +HasComponent<TContractState>,
        +Drop<TContractState>,
        +AccessControlComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl Roles: RolesComponent::HasComponent<TContractState>,
    > of IOperatorNonce<ComponentState<TContractState>> {
        /// Returns the next unused nonce.
        fn get_operator_nonce(self: @ComponentState<TContractState>) -> u64 {
            self.nonce.read()
        }
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        +Drop<TContractState>,
        +AccessControlComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl Roles: RolesComponent::HasComponent<TContractState>,
    > of InternalTrait<TContractState> {
        /// Consumes a nonce, returns the current value, and increments nonce.
        /// Panics if the nonce is not the expected value.
        fn use_checked_nonce(ref self: ComponentState<TContractState>, operator_nonce: u64) -> u64 {
            get_dep_component!(@self, Roles).only_operator();
            let current = self.nonce.read();
            self.nonce.write(current + 1);
            assert!(
                operator_nonce == current,
                "INVALID_NONCE: current!=received {}!={}",
                current,
                operator_nonce,
            );
            current
        }
    }
}
