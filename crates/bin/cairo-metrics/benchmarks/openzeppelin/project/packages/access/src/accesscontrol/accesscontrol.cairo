// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (access/src/accesscontrol/accesscontrol.cairo)

/// # AccessControl Component
///
/// The AccessControl component enables role-based access control mechanisms. This is a lightweight
/// implementation that doesn't support on-chain enumeration of role members, though role membership
/// can be tracked off-chain through contract events.
///
/// Roles can be granted and revoked dynamically via `grant_role` and `revoke_role`. Each role
/// has an associated admin role that controls who can grant and revoke it. By default, all roles
/// use `DEFAULT_ADMIN_ROLE` as their admin role.
/// Accounts can also renounce roles they have been granted by using `renounce_role`.
///
/// More complex role hierarchies can be created using `set_role_admin`.
///
/// WARNING: The `DEFAULT_ADMIN_ROLE` is its own admin, meaning it can grant and revoke itself.
/// Extra precautions should be taken to secure accounts with this role.
#[starknet::component]
pub mod AccessControlComponent {
    use core::panic_with_const_felt252;
    use openzeppelin_introspection::src5::SRC5Component;
    use openzeppelin_introspection::src5::SRC5Component::{
        InternalImpl as SRC5InternalImpl, SRC5Impl,
    };
    use starknet::ContractAddress;
    use starknet::storage::{Map, StorageMapReadAccess, StorageMapWriteAccess};
    use crate::accesscontrol::account_role_info::AccountRoleInfo;
    use crate::accesscontrol::interface;
    use crate::accesscontrol::interface::RoleStatus;

    pub const DEFAULT_ADMIN_ROLE: felt252 = 0;

    #[storage]
    pub struct Storage {
        pub AccessControl_role_admin: Map<felt252, felt252>,
        pub AccessControl_role_member: Map<(felt252, ContractAddress), AccountRoleInfo>,
    }

    #[event]
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub enum Event {
        RoleGranted: RoleGranted,
        RoleGrantedWithDelay: RoleGrantedWithDelay,
        RoleRevoked: RoleRevoked,
        RoleAdminChanged: RoleAdminChanged,
    }

    /// Emitted when `account` is granted `role`.
    ///
    /// `sender` is the account that originated the contract call, an account with the admin role
    /// or the deployer address if `_grant_role` is called from the constructor.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct RoleGranted {
        pub role: felt252,
        pub account: ContractAddress,
        pub sender: ContractAddress,
    }

    /// Emitted when `account` is granted `role` with a delay.
    ///
    /// `sender` is the account that originated the contract call, an account with the admin role
    /// or the deployer address if `_grant_role_with_delay` is called from the constructor.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct RoleGrantedWithDelay {
        pub role: felt252,
        pub account: ContractAddress,
        pub sender: ContractAddress,
        pub delay: u64,
    }

    /// Emitted when `role` is revoked for `account`.
    ///
    /// `sender` is the account that originated the contract call:
    ///   - If using `revoke_role`, it is the admin role bearer.
    ///   - If using `renounce_role`, it is the role bearer (i.e. `account`).
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct RoleRevoked {
        pub role: felt252,
        pub account: ContractAddress,
        pub sender: ContractAddress,
    }

    /// Emitted when `new_admin_role` is set as `role`'s admin role, replacing `previous_admin_role`
    ///
    /// `DEFAULT_ADMIN_ROLE` is the starting admin for all roles, despite
    /// `RoleAdminChanged` not being emitted signaling this.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct RoleAdminChanged {
        pub role: felt252,
        pub previous_admin_role: felt252,
        pub new_admin_role: felt252,
    }

    pub mod Errors {
        pub const INVALID_CALLER: felt252 = 'Can only renounce role for self';
        pub const MISSING_ROLE: felt252 = 'Caller is missing role';
        pub const INVALID_DELAY: felt252 = 'Delay must be greater than 0';
        pub const ALREADY_EFFECTIVE: felt252 = 'Role is already effective';
    }

    #[embeddable_as(AccessControlImpl)]
    impl AccessControl<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::IAccessControl<ComponentState<TContractState>> {
        /// Returns whether `account` can act as `role`.
        fn has_role(
            self: @ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) -> bool {
            self.is_role_effective(role, account)
        }

        /// Returns the admin role that controls `role`.
        fn get_role_admin(self: @ComponentState<TContractState>, role: felt252) -> felt252 {
            self.AccessControl_role_admin.read(role)
        }

        /// Grants `role` to `account`.
        ///
        /// If `account` has not been already granted `role`, emits a `RoleGranted` event.
        ///
        /// Requirements:
        ///
        /// - The caller must have `role`'s admin role.
        fn grant_role(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            let admin = Self::get_role_admin(@self, role);
            self.assert_only_role(admin);
            self._grant_role(role, account);
        }

        /// Revokes `role` from `account`.
        ///
        /// If `account` has been granted `role`, emits a `RoleRevoked` event.
        ///
        /// Requirements:
        ///
        /// - The caller must have `role`'s admin role.
        fn revoke_role(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            let admin = Self::get_role_admin(@self, role);
            self.assert_only_role(admin);
            self._revoke_role(role, account);
        }

        /// Revokes `role` from the calling account.
        ///
        /// Roles are often managed via `grant_role` and `revoke_role`: this function's
        /// purpose is to provide a mechanism for accounts to lose their privileges
        /// if they are compromised (such as when a trusted device is misplaced).
        ///
        /// If the calling account had been revoked `role`, emits a `RoleRevoked`
        /// event.
        ///
        /// Requirements:
        ///
        /// - The caller must be `account`.
        fn renounce_role(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            let caller = starknet::get_caller_address();
            assert(caller == account, Errors::INVALID_CALLER);
            self._revoke_role(role, account);
        }
    }

    /// Adds camelCase support for `IAccessControl`.
    #[embeddable_as(AccessControlCamelImpl)]
    impl AccessControlCamel<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::IAccessControlCamel<ComponentState<TContractState>> {
        fn hasRole(
            self: @ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) -> bool {
            AccessControl::has_role(self, role, account)
        }

        fn getRoleAdmin(self: @ComponentState<TContractState>, role: felt252) -> felt252 {
            AccessControl::get_role_admin(self, role)
        }

        fn grantRole(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            AccessControl::grant_role(ref self, role, account);
        }

        fn revokeRole(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            AccessControl::revoke_role(ref self, role, account);
        }

        fn renounceRole(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            AccessControl::renounce_role(ref self, role, account);
        }
    }

    #[embeddable_as(AccessControlWithDelayImpl)]
    impl AccessControlWithDelay<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::IAccessControlWithDelay<ComponentState<TContractState>> {
        /// Returns the account's status for the given role.
        ///
        /// The possible statuses are:
        ///
        /// - `NotGranted`: the role has not been granted to the account.
        /// - `Delayed`: The role has been granted to the account but is not yet active due to a
        /// time delay.
        /// - `Effective`: the role has been granted to the account and is currently active.
        fn get_role_status(
            self: @ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) -> RoleStatus {
            self.resolve_role_status(role, account)
        }

        /// Attempts to grant `role` to `account` with the specified activation delay.
        ///
        /// Requirements:
        ///
        /// - The caller must have `role`'s admin role.
        /// - delay must be greater than 0.
        /// - the `role` must not be already effective for `account`.
        ///
        /// May emit a `RoleGrantedWithDelay` event.
        fn grant_role_with_delay(
            ref self: ComponentState<TContractState>,
            role: felt252,
            account: ContractAddress,
            delay: u64,
        ) {
            let admin = AccessControl::get_role_admin(@self, role);
            self.assert_only_role(admin);
            self._grant_role_with_delay(role, account, delay);
        }
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the contract by registering the IAccessControl interface ID.
        fn initializer(ref self: ComponentState<TContractState>) {
            let mut src5_component = get_dep_component_mut!(ref self, SRC5);
            src5_component.register_interface(interface::IACCESSCONTROL_ID);
        }

        /// Validates that the caller can act as the given role. Otherwise it panics.
        fn assert_only_role(self: @ComponentState<TContractState>, role: felt252) {
            let caller = starknet::get_caller_address();
            let authorized = self.is_role_effective(role, caller);
            assert(authorized, Errors::MISSING_ROLE);
        }

        /// Returns whether the account can act as the given role.
        ///
        /// The account can act as the role if it is active and the `effective_from` time is before
        /// or equal to the current time.
        ///
        /// NOTE: If the `effective_from` timepoint is 0, the role is effective immediately.
        /// This is backwards compatible with implementations that didn't use delays but
        /// a single boolean flag.
        fn is_role_effective(
            self: @ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) -> bool {
            match self.resolve_role_status(role, account) {
                RoleStatus::Effective => true,
                RoleStatus::Delayed => false,
                RoleStatus::NotGranted => false,
            }
        }

        /// Returns the account's status for the given role.
        ///
        /// The possible statuses are:
        ///
        /// - `NotGranted`: the role has not been granted to the account.
        /// - `Delayed`: The role has been granted to the account but is not yet active due to a
        /// time delay.
        /// - `Effective`: the role has been granted to the account and is currently active.
        fn resolve_role_status(
            self: @ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) -> RoleStatus {
            let AccountRoleInfo {
                is_granted, effective_from,
            } = self.AccessControl_role_member.read((role, account));
            if is_granted {
                if effective_from == 0 {
                    RoleStatus::Effective
                } else {
                    let now = starknet::get_block_timestamp();
                    if effective_from <= now {
                        RoleStatus::Effective
                    } else {
                        RoleStatus::Delayed(effective_from)
                    }
                }
            } else {
                RoleStatus::NotGranted
            }
        }

        /// Returns whether the account has the given role granted.
        ///
        /// NOTE: The account may not be able to act as the role yet, if a delay was set and has not
        /// passed yet. Use `is_role_effective` to check if the account can act as the role.
        fn is_role_granted(
            self: @ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) -> bool {
            let account_role_info = self.AccessControl_role_member.read((role, account));
            account_role_info.is_granted
        }

        /// Sets `admin_role` as `role`'s admin role.
        ///
        /// Internal function without access restriction.
        ///
        /// Emits a `RoleAdminChanged` event.
        fn set_role_admin(
            ref self: ComponentState<TContractState>, role: felt252, admin_role: felt252,
        ) {
            let previous_admin_role = AccessControl::get_role_admin(@self, role);
            self.AccessControl_role_admin.write(role, admin_role);
            self.emit(RoleAdminChanged { role, previous_admin_role, new_admin_role: admin_role });
        }

        /// Attempts to grant `role` to `account`. The function does nothing if `role` is already
        /// effective for `account`. If `role` has been granted to `account`, but is not yet active
        /// due to a time delay, the delay is removed and `role` becomes effective immediately.
        ///
        /// Internal function without access restriction.
        ///
        /// May emit a `RoleGranted` event.
        fn _grant_role(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            match self.resolve_role_status(role, account) {
                RoleStatus::Effective => (),
                RoleStatus::Delayed |
                RoleStatus::NotGranted => {
                    let caller = starknet::get_caller_address();
                    let role_info = AccountRoleInfo { is_granted: true, effective_from: 0 };
                    self.AccessControl_role_member.write((role, account), role_info);
                    self.emit(RoleGranted { role, account, sender: caller });
                },
            };
        }

        /// Attempts to grant `role` to `account` with the specified activation delay.
        ///
        /// The role will become effective after the given delay has passed. If the role is already
        /// active (`Effective`) for the account, the function will panic. If the role has been
        /// granted but is not yet active (being in the `Delayed` state), the existing delay will be
        /// overwritten with the new `delay`.
        ///
        /// Internal function without access restriction.
        ///
        /// Requirements:
        ///
        /// - delay must be greater than 0.
        /// - the `role` must not be already effective for `account`.
        ///
        /// May emit a `RoleGrantedWithDelay` event.
        fn _grant_role_with_delay(
            ref self: ComponentState<TContractState>,
            role: felt252,
            account: ContractAddress,
            delay: u64,
        ) {
            assert(delay > 0, Errors::INVALID_DELAY);
            match self.resolve_role_status(role, account) {
                RoleStatus::Effective => panic_with_const_felt252::<Errors::ALREADY_EFFECTIVE>(),
                RoleStatus::Delayed |
                RoleStatus::NotGranted => {
                    let caller = starknet::get_caller_address();
                    let effective_from = starknet::get_block_timestamp() + delay;
                    let role_info = AccountRoleInfo { is_granted: true, effective_from };
                    self.AccessControl_role_member.write((role, account), role_info);
                    self.emit(RoleGrantedWithDelay { role, account, sender: caller, delay });
                },
            };
        }

        /// Attempts to revoke `role` from `account`.
        ///
        /// Internal function without access restriction.
        ///
        /// May emit a `RoleRevoked` event.
        fn _revoke_role(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            match self.resolve_role_status(role, account) {
                RoleStatus::NotGranted => (),
                RoleStatus::Effective |
                RoleStatus::Delayed => {
                    let caller = starknet::get_caller_address();
                    let role_info = AccountRoleInfo { is_granted: false, effective_from: 0 };
                    self.AccessControl_role_member.write((role, account), role_info);
                    self.emit(RoleRevoked { role, account, sender: caller });
                },
            };
        }
    }

    #[embeddable_as(AccessControlMixinImpl)]
    impl AccessControlMixin<
        TContractState,
        +HasComponent<TContractState>,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::AccessControlABI<ComponentState<TContractState>> {
        // IAccessControl
        fn has_role(
            self: @ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) -> bool {
            AccessControl::has_role(self, role, account)
        }

        fn get_role_admin(self: @ComponentState<TContractState>, role: felt252) -> felt252 {
            AccessControl::get_role_admin(self, role)
        }

        fn grant_role(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            AccessControl::grant_role(ref self, role, account);
        }

        fn revoke_role(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            AccessControl::revoke_role(ref self, role, account);
        }

        fn renounce_role(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            AccessControl::renounce_role(ref self, role, account);
        }

        // IAccessControlCamel
        fn hasRole(
            self: @ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) -> bool {
            AccessControlCamel::hasRole(self, role, account)
        }

        fn getRoleAdmin(self: @ComponentState<TContractState>, role: felt252) -> felt252 {
            AccessControlCamel::getRoleAdmin(self, role)
        }

        fn grantRole(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            AccessControlCamel::grantRole(ref self, role, account);
        }

        fn revokeRole(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            AccessControlCamel::revokeRole(ref self, role, account);
        }

        fn renounceRole(
            ref self: ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) {
            AccessControlCamel::renounceRole(ref self, role, account);
        }

        // IAccessControlWithDelay
        fn get_role_status(
            self: @ComponentState<TContractState>, role: felt252, account: ContractAddress,
        ) -> RoleStatus {
            AccessControlWithDelay::get_role_status(self, role, account)
        }

        fn grant_role_with_delay(
            ref self: ComponentState<TContractState>,
            role: felt252,
            account: ContractAddress,
            delay: u64,
        ) {
            AccessControlWithDelay::grant_role_with_delay(ref self, role, account, delay);
        }

        // ISRC5
        fn supports_interface(
            self: @ComponentState<TContractState>, interface_id: felt252,
        ) -> bool {
            let src5 = get_dep_component!(self, SRC5);
            src5.supports_interface(interface_id)
        }
    }
}
