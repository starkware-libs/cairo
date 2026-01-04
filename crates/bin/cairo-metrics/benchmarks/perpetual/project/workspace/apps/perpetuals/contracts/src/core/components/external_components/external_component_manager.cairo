#[starknet::component]
pub mod ExternalComponents {
    use RolesComponent::InternalTrait as RolesInternalTrait;
    use core::num::traits::Zero;
    use core::panic_with_felt252;
    use core::panics::panic_with_byte_array;
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::introspection::src5::SRC5Component;
    use starknet::ClassHash;
    use starknet::storage::{
        Map, StoragePathEntry, StoragePointerReadAccess, StoragePointerWriteAccess,
    };
    use starkware_utils::components::replaceability::ReplaceabilityComponent;
    use starkware_utils::components::replaceability::interface::IReplaceable;
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::time::time::{Time, TimeDelta, Timestamp};
    use crate::core::components::assets::assets_manager::IAssetsExternalLibraryDispatcher;
    use crate::core::components::deleverage::deleverage_manager::IDeleverageManagerLibraryDispatcher;
    use crate::core::components::deposit::deposit_manager::IDepositExternalLibraryDispatcher;
    use crate::core::components::external_components::events;
    use crate::core::components::external_components::interface::{
        EXTERNAL_COMPONENT_DELEVERAGES, IExternalComponents,
    };
    use crate::core::components::external_components::named_component::ITypedComponentLibraryDispatcher;
    use crate::core::components::liquidation::liquidation_manager::ILiquidationManagerLibraryDispatcher;
    use crate::core::components::transfer::transfer_manager::ITransferManagerLibraryDispatcher;
    use crate::core::components::vaults::vaults_contract::IVaultExternalLibraryDispatcher;
    use crate::core::components::withdrawal::withdrawal_manager::IWithdrawalManagerLibraryDispatcher;
    use super::super::interface::{
        EXTERNAL_COMPONENT_ASSETS, EXTERNAL_COMPONENT_DEPOSITS, EXTERNAL_COMPONENT_LIQUIDATIONS,
        EXTERNAL_COMPONENT_TRANSFERS, EXTERNAL_COMPONENT_VAULT, EXTERNAL_COMPONENT_WITHDRAWALS,
    };
    use super::super::named_component::ITypedComponentDispatcherTrait;

    #[event]
    #[derive(Drop, PartialEq, starknet::Event)]
    pub enum Event {
        ExternalComponentImplRegistered: events::ExternalComponentImplRegistered,
        ExternalComponentImplActivated: events::ExternalComponentImplActivated,
    }

    #[storage]
    pub struct Storage {
        //component storage
        pub registered_external_components: Map<felt252, (ClassHash, Timestamp)>,
        #[rename("external_components")]
        pub external_component_implementations: Map<felt252, ClassHash>,
    }

    #[embeddable_as(ExternalComponentsImpl)]
    impl ExternalComponentsComponent<
        TContractState,
        +HasComponent<TContractState>,
        +Drop<TContractState>,
        +AccessControlComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl Roles: RolesComponent::HasComponent<TContractState>,
        impl Replaceability: ReplaceabilityComponent::HasComponent<TContractState>,
    > of IExternalComponents<ComponentState<TContractState>> {
        fn register_external_component(
            ref self: ComponentState<TContractState>,
            component_type: felt252,
            component_address: ClassHash,
        ) {
            self._register_external_component(component_type, component_address);
        }

        fn activate_external_component(
            ref self: ComponentState<TContractState>,
            component_type: felt252,
            component_address: ClassHash,
        ) {
            self._activate_external_component(component_type, component_address);
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
        fn _get_vault_manager_dispatcher(
            ref self: ComponentState<TContractState>,
        ) -> IVaultExternalLibraryDispatcher {
            let class_hash = self
                .external_component_implementations
                .entry(EXTERNAL_COMPONENT_VAULT)
                .read();
            assert(class_hash.is_non_zero(), 'NO_VAULT_MANAGER');
            IVaultExternalLibraryDispatcher { class_hash: class_hash }
        }

        fn _get_withdrawal_manager_dispatcher(
            ref self: ComponentState<TContractState>,
        ) -> IWithdrawalManagerLibraryDispatcher {
            let class_hash = self
                .external_component_implementations
                .entry(EXTERNAL_COMPONENT_WITHDRAWALS)
                .read();
            assert(class_hash.is_non_zero(), 'NO_WITHDRAW_MANAGER');
            IWithdrawalManagerLibraryDispatcher { class_hash: class_hash }
        }

        fn _get_transfer_manager_dispatcher(
            ref self: ComponentState<TContractState>,
        ) -> ITransferManagerLibraryDispatcher {
            let class_hash = self
                .external_component_implementations
                .entry(EXTERNAL_COMPONENT_TRANSFERS)
                .read();
            assert(class_hash.is_non_zero(), 'NO_TRANSFER_MANAGER');
            ITransferManagerLibraryDispatcher { class_hash: class_hash }
        }

        fn _get_liquidation_manager_dispatcher(
            ref self: ComponentState<TContractState>,
        ) -> ILiquidationManagerLibraryDispatcher {
            let class_hash = self
                .external_component_implementations
                .entry(EXTERNAL_COMPONENT_LIQUIDATIONS)
                .read();
            assert(class_hash.is_non_zero(), 'NO_LIQUIDATION_MANAGER');
            ILiquidationManagerLibraryDispatcher { class_hash: class_hash }
        }

        fn _get_deleverage_manager_dispatcher(
            ref self: ComponentState<TContractState>,
        ) -> IDeleverageManagerLibraryDispatcher {
            let class_hash = self
                .external_component_implementations
                .entry(EXTERNAL_COMPONENT_DELEVERAGES)
                .read();
            assert(class_hash.is_non_zero(), 'NO_DELEVERAGE_MANAGER');
            IDeleverageManagerLibraryDispatcher { class_hash: class_hash }
        }

        fn _get_deposit_manager_dispatcher(
            self: @ComponentState<TContractState>,
        ) -> IDepositExternalLibraryDispatcher {
            let class_hash = self
                .external_component_implementations
                .entry(EXTERNAL_COMPONENT_DEPOSITS)
                .read();
            assert(class_hash.is_non_zero(), 'NO_DEPOSIT_MANAGER');
            IDepositExternalLibraryDispatcher { class_hash: class_hash }
        }

        fn _get_assets_manager_dispatcher(
            self: @ComponentState<TContractState>,
        ) -> IAssetsExternalLibraryDispatcher {
            let class_hash = self
                .external_component_implementations
                .entry(EXTERNAL_COMPONENT_ASSETS)
                .read();
            assert(class_hash.is_non_zero(), 'NO_ASSETS_MANAGER');
            IAssetsExternalLibraryDispatcher { class_hash: class_hash }
        }
    }

    #[generate_trait]
    impl PrivateImpl<
        TContractState,
        +HasComponent<TContractState>,
        +Drop<TContractState>,
        +AccessControlComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl Roles: RolesComponent::HasComponent<TContractState>,
        impl Replaceability: ReplaceabilityComponent::HasComponent<TContractState>,
    > of PrivateTrait<TContractState> {
        fn _register_external_component(
            ref self: ComponentState<TContractState>,
            component_type: felt252,
            class_hash: ClassHash,
        ) {
            get_dep_component!(@self, Roles).only_upgrade_governor();
            let update_delay = TimeDelta {
                seconds: (get_dep_component!(@self, Replaceability).get_upgrade_delay()),
            };
            let declared_type = ITypedComponentLibraryDispatcher { class_hash: class_hash }
                .component_type();

            if (declared_type != component_type) {
                let err = format!(
                    "Component type mismatch: declared {}, expected {}",
                    declared_type,
                    component_type,
                );
                panic_with_byte_array(err: @err);
            }
            if (component_type == EXTERNAL_COMPONENT_VAULT)
                || (component_type == EXTERNAL_COMPONENT_WITHDRAWALS)
                || (component_type == EXTERNAL_COMPONENT_DEPOSITS)
                || (component_type == EXTERNAL_COMPONENT_TRANSFERS)
                || (component_type == EXTERNAL_COMPONENT_LIQUIDATIONS)
                || (component_type == EXTERNAL_COMPONENT_DELEVERAGES)
                || (component_type == EXTERNAL_COMPONENT_ASSETS) {
                let now = Time::now();
                let activation_time = now.add(update_delay);
                let entry = self.registered_external_components.entry(component_type);
                entry.write((class_hash, activation_time));
                self
                    .emit(
                        events::ExternalComponentImplRegistered {
                            component_type, activation_time, implementation: class_hash,
                        },
                    );
            } else {
                panic_with_felt252('INVALID_EXTERNAL_COMPONENT_TYPE');
            }
        }

        fn _activate_external_component(
            ref self: ComponentState<TContractState>,
            component_type: felt252,
            class_hash: ClassHash,
        ) {
            get_dep_component!(@self, Roles).only_upgrade_governor();
            let entry = self.registered_external_components.entry(component_type);
            let (registered_class_hash, activation_time) = entry.read();
            if (registered_class_hash != class_hash) {
                let err = format!("{:?} not registered with hash {:?}", component_type, class_hash);
                panic_with_byte_array(err: @err);
            }
            let now = Time::now();
            if (now < activation_time) {
                let err = format!("Activation time not reached");
                panic_with_byte_array(err: @err);
            }
            let impl_entry = self.external_component_implementations.entry(component_type);
            impl_entry.write(class_hash);
            self
                .emit(
                    events::ExternalComponentImplActivated {
                        component_type, implementation: class_hash,
                    },
                );
        }
    }
}
