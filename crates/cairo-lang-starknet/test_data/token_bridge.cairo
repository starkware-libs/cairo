use starknet::ContractAddress;
use serde::Serde;
use traits::Into;
use zeroable::Zeroable;

#[starknet::interface]
trait IMintableToken<T> {
    fn permissioned_mint(ref self: T, account: ContractAddress, amount: u256);
    fn permissioned_burn(ref self: T, account: ContractAddress, amount: u256);
}

#[starknet::contract]
mod TokenBridge {
    use array::ArrayTrait;
    use integer::{Felt252IntoU256, U128IntoFelt252};
    use option::OptionTrait;
    use serde::Serde;
    use starknet::contract_address::ContractAddressZeroable;
    use starknet::{
        ContractAddress, get_caller_address, EthAddress, EthAddressIntoFelt252, EthAddressSerde,
        EthAddressZeroable, syscalls::send_message_to_l1_syscall
    };
    use super::{
        IMintableTokenDispatcher, IMintableTokenLibraryDispatcher, IMintableTokenDispatcherTrait
    };
    use traits::Into;
    use zeroable::Zeroable;

    const WITHDRAW_MESSAGE: felt252 = 0;
    const CONTRACT_IDENTITY: felt252 = 'STARKGATE';
    const CONTRACT_VERSION: felt252 = 2;

    #[storage]
    struct Storage {
        // The address of the L2 governor of this contract. Only the governor can set the other
        // storage variables.
        governor: ContractAddress,
        // The L1 bridge address. Zero when unset.
        l1_bridge: felt252,
        // The L2 token contract address. Zero when unset.
        l2_token: ContractAddress,
    }

    #[derive(Drop, starknet::Event)]
    enum Event {
        #[nested]
        L1BridgeSet: L1BridgeSet,
        #[nested]
        L2TokenSet: L2TokenSet,
        #[nested]
        WithdrawInitiated: WithdrawInitiated,
        #[nested]
        DepositHandled: DepositHandled,
    }

    // An event that is emitted when set_l1_bridge is called.
    // * l1_bridge_address is the new l1 bridge address.
    #[derive(Drop, starknet::Event)]
    struct L1BridgeSet {
        l1_bridge_address: EthAddress, 
    }

    // An event that is emitted when set_l2_token is called.
    // * l2_token_address is the new l2 token address.
    #[derive(Drop, starknet::Event)]
    struct L2TokenSet {
        l2_token_address: ContractAddress, 
    }

    // An event that is emitted when initiate_withdraw is called.
    // * l1_recipient is the l1 recipient address.
    // * amount is the amount to withdraw.
    // * caller_address is the address from which the call was made.
    #[derive(Drop, starknet::Event)]
    struct WithdrawInitiated {
        l1_recipient: EthAddress,
        amount: u256,
        caller_address: ContractAddress,
    }

    // An event that is emitted when handle_deposit is called.
    // * account is the recipient address.
    // * amount is the amount to deposit.
    #[derive(Drop, starknet::Event)]
    struct DepositHandled {
        account: ContractAddress,
        amount: u256,
    }

    #[constructor]
    fn constructor(ref self: Storage, governor_address: ContractAddress) {
        assert(governor_address.is_non_zero(), 'ZERO_GOVERNOR_ADDRESS');
        self.governor.write(governor_address);
    }

    #[generate_trait]
    #[external(v0)]
    impl TokenBridgeImpl of ITokenBridge {
        // TODO(spapini): Consider adding a pure option, with no parameters.
        fn get_version(self: @Storage) -> felt252 {
            CONTRACT_VERSION
        }

        fn get_identity(self: @Storage) -> felt252 {
            CONTRACT_IDENTITY
        }

        fn set_l1_bridge(ref self: Storage, l1_bridge_address: EthAddress) {
            // The call is restricted to the governor.
            assert(get_caller_address() == self.governor.read(), 'GOVERNOR_ONLY');

            assert(self.l1_bridge.read().is_zero(), 'L1_BRIDGE_ALREADY_INITIALIZED');
            assert(l1_bridge_address.is_non_zero(), 'ZERO_BRIDGE_ADDRESS');

            self.l1_bridge.write(l1_bridge_address.into());
            self.emit(Event::L1BridgeSet(L1BridgeSet { l1_bridge_address }));
        }

        fn set_l2_token(ref self: Storage, l2_token_address: ContractAddress) {
            // The call is restricted to the governor.
            assert(get_caller_address() == self.governor.read(), 'GOVERNOR_ONLY');

            assert(self.l2_token.read().is_zero(), 'L2_TOKEN_ALREADY_INITIALIZED');
            assert(l2_token_address.is_non_zero(), 'ZERO_TOKEN_ADDRESS');

            self.l2_token.write(l2_token_address);
            self.emit(Event::L2TokenSet(L2TokenSet { l2_token_address }));
        }

        fn initiate_withdraw(ref self: Storage, l1_recipient: EthAddress, amount: u256) {
            // Call burn on l2_token contract.
            let caller_address = get_caller_address();
            IMintableTokenDispatcher {
                contract_address: self.read_initialized_l2_token()
            }.permissioned_burn(account: caller_address, :amount);

            // Send the message.
            let mut message_payload: Array<felt252> = Default::default();
            message_payload.append(WITHDRAW_MESSAGE);
            message_payload.append(l1_recipient.into());
            message_payload.append(amount.low.into());
            message_payload.append(amount.high.into());

            send_message_to_l1_syscall(
                to_address: self.read_initialized_l1_bridge(), payload: message_payload.span()
            );
            self
                .emit(
                    Event::WithdrawInitiated(
                        WithdrawInitiated { l1_recipient, amount, caller_address }
                    )
                );
        }
    }

    #[l1_handler]
    fn handle_deposit(
        ref self: Storage, from_address: felt252, account: ContractAddress, amount: u256
    ) {
        assert(from_address == self.l1_bridge.read(), 'EXPECTED_FROM_BRIDGE_ONLY');

        // Call mint on l2_token contract.
        IMintableTokenDispatcher {
            contract_address: self.read_initialized_l2_token()
        }.permissioned_mint(:account, :amount);

        self.emit(Event::DepositHandled(DepositHandled { account, amount }));
    }

    /// Helpers (internal functions)
    #[generate_trait]
    impl HelperImpl of HelperTrait {
        // Read l1_bridge and verify it's initialized.
        fn read_initialized_l1_bridge(self: @Storage) -> felt252 {
            let l1_bridge_address = self.l1_bridge.read();
            assert(l1_bridge_address.is_non_zero(), 'UNINITIALIZED_L1_BRIDGE_ADDRESS');
            l1_bridge_address
        }

        // Read l2_token and verify it's initialized.
        fn read_initialized_l2_token(self: @Storage) -> ContractAddress {
            let l2_token_address = self.l2_token.read();
            assert(l2_token_address.is_non_zero(), 'UNINITIALIZED_TOKEN');
            l2_token_address
        }
    }
}
