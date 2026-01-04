#[starknet::component]
pub(crate) mod Deposit {
    use core::num::traits::Zero;
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::introspection::src5::SRC5Component;
    use perpetuals::core::components::assets::AssetsComponent;
    use perpetuals::core::components::deposit::errors;
    use perpetuals::core::components::deposit::interface::{DepositStatus, IDeposit};
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent::InternalTrait as NonceInternal;
    use perpetuals::core::components::positions::Positions as PositionsComponent;
    use perpetuals::core::types::asset::AssetId;
    use perpetuals::core::types::position::PositionId;
    use starknet::storage::{
        Map, StorageMapReadAccess, StoragePointerReadAccess, StoragePointerWriteAccess,
    };
    use starknet::{ContractAddress, get_caller_address};
    use starkware_utils::components::pausable::PausableComponent;
    use starkware_utils::components::pausable::PausableComponent::InternalTrait as PausableInternal;
    use starkware_utils::components::request_approvals::RequestApprovalsComponent;
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::signature::stark::HashType;
    use starkware_utils::time::time::{Time, TimeDelta};
    use crate::core::components::assets::interface::IAssets;
    use crate::core::components::deposit::deposit_manager::IDepositExternalDispatcherTrait;
    use crate::core::components::external_components::external_component_manager::ExternalComponents as ExternalComponentsComponent;
    use crate::core::components::external_components::external_component_manager::ExternalComponents::InternalTrait as ExternalComponentsInternalTrait;
    use crate::core::components::vaults::vaults::Vaults as VaultsComponent;


    #[storage]
    pub struct Storage {
        pub registered_deposits: Map<HashType, DepositStatus>,
        pub cancel_delay: TimeDelta,
    }

    #[event]
    #[derive(Drop, PartialEq, starknet::Event)]
    pub enum Event {}


    #[embeddable_as(DepositImpl)]
    impl Deposit<
        TContractState,
        +HasComponent<TContractState>,
        +Drop<TContractState>,
        +AccessControlComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl Assets: AssetsComponent::HasComponent<TContractState>,
        impl OperatorNonce: OperatorNonceComponent::HasComponent<TContractState>,
        impl Pausable: PausableComponent::HasComponent<TContractState>,
        impl Positions: PositionsComponent::HasComponent<TContractState>,
        impl Roles: RolesComponent::HasComponent<TContractState>,
        impl RequestApprovals: RequestApprovalsComponent::HasComponent<TContractState>,
        impl Vaults: VaultsComponent::HasComponent<TContractState>,
        impl ExternalComponents: ExternalComponentsComponent::HasComponent<TContractState>,
    > of IDeposit<ComponentState<TContractState>> {
        fn deposit(
            ref self: ComponentState<TContractState>,
            position_id: PositionId,
            quantized_amount: u64,
            salt: felt252,
        ) {
            let assets = get_dep_component!(@self, Assets);
            let pnl_collateral_id = assets.get_collateral_id();
            self.deposit_asset(asset_id: pnl_collateral_id, :position_id, :quantized_amount, :salt)
        }

        /// Deposit is called by the user to add a deposit request.
        ///
        /// Validations:
        /// - The quantized amount must be greater than 0.
        /// - The deposit requested does not exists.
        ///
        /// Execution:
        /// - Transfers the quantized amount from the user to the contract.
        /// - Registers the deposit request.
        /// - Updates the deposit status to pending.
        /// - Emits a Deposit event.
        fn deposit_asset(
            ref self: ComponentState<TContractState>,
            asset_id: AssetId,
            position_id: PositionId,
            quantized_amount: u64,
            salt: felt252,
        ) {
            assert(quantized_amount.is_non_zero(), errors::ZERO_AMOUNT);
            let caller_address = get_caller_address();
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_deposit_manager_dispatcher()
                .deposit(
                    caller_address: caller_address,
                    asset_id: asset_id,
                    position_id: position_id,
                    quantized_amount: quantized_amount,
                    now: Time::now(),
                    salt: salt,
                );
        }

        /// Cancel deposit is called by the user to cancel a deposit request which did not take
        /// place yet.
        ///
        /// Validations:
        /// - The deposit requested to cancel exists, is not canceled and is not processed.
        /// - The cancellation delay has passed.
        ///
        /// Execution:
        /// - Transfers the quantized amount back to the user.
        /// - Updates the deposit status to canceled.
        /// - Emits a DepositCanceled event.
        fn cancel_deposit(
            ref self: ComponentState<TContractState>,
            asset_id: AssetId,
            position_id: PositionId,
            quantized_amount: u64,
            salt: felt252,
        ) {
            let depositor = get_caller_address();
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_deposit_manager_dispatcher()
                .cancel_deposit(
                    depositor: depositor,
                    asset_id: asset_id,
                    position_id: position_id,
                    quantized_amount: quantized_amount,
                    now: Time::now(),
                    salt: salt,
                );
        }


        /// Reject deposit is called by the operator to cancel a deposit request which did not take
        /// place yet.
        ///
        /// Validations:
        /// - The deposit requested to cancel exists, is not canceled and is not processed.
        /// - The cancellation delay has passed.
        /// - Only the operator can call this function.
        /// - The contract must not be paused.
        /// - The `operator_nonce` must be valid.
        ///
        /// Execution:
        /// - Transfers the quantized amount back to the user.
        /// - Updates the deposit status to canceled.
        /// - Emits a DepositCanceled event.
        fn reject_deposit(
            ref self: ComponentState<TContractState>,
            operator_nonce: u64,
            depositor: ContractAddress,
            asset_id: AssetId,
            position_id: PositionId,
            quantized_amount: u64,
            salt: felt252,
        ) {
            /// Validations:
            get_dep_component!(@self, Pausable).assert_not_paused();
            let mut nonce = get_dep_component_mut!(ref self, OperatorNonce);
            nonce.use_checked_nonce(:operator_nonce);
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_deposit_manager_dispatcher()
                .reject_deposit(
                    depositor: depositor,
                    asset_id: asset_id,
                    position_id: position_id,
                    quantized_amount: quantized_amount,
                    now: Time::now(),
                    salt: salt,
                );
        }


        /// Process deposit a collateral amount from the 'depositing_address' to a given position.
        ///
        /// Validations:
        /// - Only the operator can call this function.
        /// - The contract must not be paused.
        /// - The `operator_nonce` must be valid.
        /// - The `expiration` time has not passed.
        /// - The collateral asset exists in the system.
        /// - The collateral asset is active.
        /// - The funding validation interval has not passed since the last funding tick.
        /// - The prices of all assets in the system are valid.
        /// - The deposit message has not been fulfilled.
        /// - A fact was registered for the deposit message.
        /// - If position exists, validate the owner_public_key and owner_account are the same.
        ///
        /// Execution:
        /// - Transfer the collateral `amount` to the position from the pending deposits.
        /// - Update the position's collateral balance.
        /// - Mark the deposit message as fulfilled.
        fn process_deposit(
            ref self: ComponentState<TContractState>,
            operator_nonce: u64,
            depositor: ContractAddress,
            asset_id: AssetId,
            position_id: PositionId,
            quantized_amount: u64,
            salt: felt252,
        ) {
            /// Validations:
            get_dep_component!(@self, Pausable).assert_not_paused();
            let mut nonce = get_dep_component_mut!(ref self, OperatorNonce);
            nonce.use_checked_nonce(:operator_nonce);
            let external_components = get_dep_component!(@self, ExternalComponents);
            external_components
                ._get_deposit_manager_dispatcher()
                .process_deposit(
                    depositor: depositor,
                    asset_id: asset_id,
                    position_id: position_id,
                    quantized_amount: quantized_amount,
                    salt: salt,
                );
        }

        fn get_deposit_status(
            self: @ComponentState<TContractState>, deposit_hash: HashType,
        ) -> DepositStatus {
            self.registered_deposits.read(deposit_hash)
        }

        fn get_cancel_delay(self: @ComponentState<TContractState>) -> TimeDelta {
            self.cancel_delay.read()
        }
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        +Drop<TContractState>,
        +AccessControlComponent::HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        impl Assets: AssetsComponent::HasComponent<TContractState>,
        impl OperatorNonce: OperatorNonceComponent::HasComponent<TContractState>,
        impl Pausable: PausableComponent::HasComponent<TContractState>,
        impl Positions: PositionsComponent::HasComponent<TContractState>,
        impl Roles: RolesComponent::HasComponent<TContractState>,
        impl RequestApprovals: RequestApprovalsComponent::HasComponent<TContractState>,
        impl Vaults: VaultsComponent::HasComponent<TContractState>,
    > of InternalTrait<TContractState> {
        fn initialize(ref self: ComponentState<TContractState>, cancel_delay: TimeDelta) {
            assert(self.cancel_delay.read().is_zero(), errors::ALREADY_INITIALIZED);
            assert(cancel_delay.is_non_zero(), errors::INVALID_CANCEL_DELAY);
            self.cancel_delay.write(cancel_delay);
        }
    }
}
