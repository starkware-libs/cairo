#[contract]
mod ERC20 {
    use zeroable::Zeroable;
    use starknet::get_caller_address;
    use starknet::contract_address_const;
    use starknet::ContractAddress;

    #[starknet::storage]
    struct Storage {
        name: felt252,
        symbol: felt252,
        decimals: u8,
        total_supply: u256,
        balances: LegacyMap::<ContractAddress, u256>,
        allowances: LegacyMap::<(ContractAddress, ContractAddress), u256>,
    }

    #[derive(Drop, starknet::Event)]
    enum Event {
        #[event]
        Transfer: Transfer,
        #[event]
        Approval: Approval,
    }
    #[derive(Drop, starknet::Event)]
    struct Transfer {
        from: ContractAddress,
        to: ContractAddress,
        value: u256,
    }
    #[derive(Drop, starknet::Event)]
    struct Approval {
        owner: ContractAddress,
        spender: ContractAddress,
        value: u256,
    }

    #[constructor]
    fn constructor(
        ref self: Storage,
        name_: felt252,
        symbol_: felt252,
        decimals_: u8,
        initial_supply: u256,
        recipient: ContractAddress
    ) {
        self.name.write(name_);
        self.symbol.write(symbol_);
        self.decimals.write(decimals_);
        assert(!recipient.is_zero(), 'ERC20: mint to the 0 address');
        self.total_supply.write(initial_supply);
        self.balances.write(recipient, initial_supply);
        self
            .emit(
                Event::Transfer(
                    Transfer {
                        from: contract_address_const::<0>(), to: recipient, value: initial_supply
                    }
                )
            );
    }

    #[external]
    fn get_name(self: @Storage) -> felt252 {
        self.name.read()
    }

    #[external]
    fn get_symbol(self: @Storage) -> felt252 {
        self.symbol.read()
    }

    #[external]
    fn get_decimals(self: @Storage) -> u8 {
        self.decimals.read()
    }

    #[external]
    fn get_total_supply(self: @Storage) -> u256 {
        self.total_supply.read()
    }

    #[external]
    fn balance_of(self: @Storage, account: ContractAddress) -> u256 {
        self.balances.read(account)
    }

    #[external]
    fn allowance(self: @Storage, owner: ContractAddress, spender: ContractAddress) -> u256 {
        self.allowances.read((owner, spender))
    }

    #[external]
    fn transfer(ref self: Storage, recipient: ContractAddress, amount: u256) {
        let sender = get_caller_address();
        self.transfer_helper(sender, recipient, amount);
    }

    #[external]
    fn transfer_from(
        ref self: Storage, sender: ContractAddress, recipient: ContractAddress, amount: u256
    ) {
        let caller = get_caller_address();
        self.spend_allowance(sender, caller, amount);
        self.transfer_helper(sender, recipient, amount);
    }

    #[external]
    fn approve(ref self: Storage, spender: ContractAddress, amount: u256) {
        let caller = get_caller_address();
        self.approve_helper(caller, spender, amount);
    }

    #[external]
    fn increase_allowance(ref self: Storage, spender: ContractAddress, added_value: u256) {
        let caller = get_caller_address();
        self.approve_helper(caller, spender, self.allowances.read((caller, spender)) + added_value);
    }

    #[external]
    fn decrease_allowance(ref self: Storage, spender: ContractAddress, subtracted_value: u256) {
        let caller = get_caller_address();
        self
            .approve_helper(
                caller, spender, self.allowances.read((caller, spender)) - subtracted_value
            );
    }

    trait StorageTrait {
        fn transfer_helper(
            ref self: Storage, sender: ContractAddress, recipient: ContractAddress, amount: u256
        );
        fn spend_allowance(
            ref self: Storage, owner: ContractAddress, spender: ContractAddress, amount: u256
        );
        fn approve_helper(
            ref self: Storage, owner: ContractAddress, spender: ContractAddress, amount: u256
        );
    }
    impl StorageImpl of StorageTrait {
        fn transfer_helper(
            ref self: Storage, sender: ContractAddress, recipient: ContractAddress, amount: u256
        ) {
            assert(!sender.is_zero(), 'ERC20: transfer from 0');
            assert(!recipient.is_zero(), 'ERC20: transfer to 0');
            self.balances.write(sender, self.balances.read(sender) - amount);
            self.balances.write(recipient, self.balances.read(recipient) + amount);
            self.emit(Event::Transfer(Transfer { from: sender, to: recipient, value: amount }));
        }

        fn spend_allowance(
            ref self: Storage, owner: ContractAddress, spender: ContractAddress, amount: u256
        ) {
            let current_allowance = self.allowances.read((owner, spender));
            let ONES_MASK = 0xffffffffffffffffffffffffffffffff_u128;
            let is_unlimited_allowance = current_allowance.low == ONES_MASK
                & current_allowance.high == ONES_MASK;
            if !is_unlimited_allowance {
                self.approve_helper(owner, spender, current_allowance - amount);
            }
        }

        fn approve_helper(
            ref self: Storage, owner: ContractAddress, spender: ContractAddress, amount: u256
        ) {
            assert(!spender.is_zero(), 'ERC20: approve from 0');
            self.allowances.write((owner, spender), amount);
            self.emit(Event::Approval(Approval { owner, spender, value: amount }));
        }
    }
}
