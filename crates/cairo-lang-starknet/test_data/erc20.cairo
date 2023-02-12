#[contract]
mod ERC20 {
    struct Storage {
        name: felt,
        symbol: felt,
        decimals: u8,
        total_supply: u256,
        balances: LegacyMap::<felt, u256>,
        allowances: LegacyMap::<(felt, felt), u256>,
    }

    // TODO(ilya): use `ContractAddress` for addresses in this contract and remove the following.
    // A wrapper around get_caller_address that returns a felt.
    fn get_caller_address() -> felt {
        starknet::contract_address_to_felt(starknet::get_caller_address())
    }

    #[event]
    fn Transfer(from: felt, to: felt, value: u256) {}

    #[event]
    fn Approval(owner: felt, spender: felt, value: u256) {}

    #[constructor]
    fn constructor(
        name_: felt, symbol_: felt, decimals_: u8, initial_supply: u256, recipient: felt
    ) {
        name::write(name_);
        symbol::write(symbol_);
        decimals::write(decimals_);
        assert(recipient != 0, 'ERC20: mint to the 0 address');
        total_supply::write(initial_supply);
        balances::write(recipient, initial_supply);
        Transfer(0, recipient, initial_supply);
    }

    #[view]
    fn get_name() -> felt {
        name::read()
    }

    #[view]
    fn get_symbol() -> felt {
        symbol::read()
    }

    #[view]
    fn get_decimals() -> u8 {
        decimals::read()
    }

    #[view]
    fn get_total_supply() -> u256 {
        total_supply::read()
    }

    #[view]
    fn balance_of(account: felt) -> u256 {
        balances::read(account)
    }

    #[view]
    fn allowance(owner: felt, spender: felt) -> u256 {
        allowances::read((owner, spender))
    }

    #[external]
    fn transfer(recipient: felt, amount: u256) {
        let sender = get_caller_address();
        transfer_helper(sender, recipient, amount);
    }

    #[external]
    fn transfer_from(sender: felt, recipient: felt, amount: u256) {
        let caller = get_caller_address();
        spend_allowance(sender, caller, amount);
        transfer_helper(sender, recipient, amount);
    }

    #[external]
    fn approve(spender: felt, amount: u256) {
        let caller = get_caller_address();
        approve_helper(caller, spender, amount);
    }

    #[external]
    fn increase_allowance(spender: felt, added_value: u256) {
        let caller = get_caller_address();
        approve_helper(caller, spender, allowances::read((caller, spender)) + added_value);
    }

    #[external]
    fn decrease_allowance(spender: felt, subtracted_value: u256) {
        let caller = get_caller_address();
        approve_helper(caller, spender, allowances::read((caller, spender)) - subtracted_value);
    }

    fn transfer_helper(sender: felt, recipient: felt, amount: u256) {
        assert(sender != 0, 'ERC20: transfer from 0');
        assert(recipient != 0, 'ERC20: transfer to 0');
        balances::write(sender, balances::read(sender) - amount);
        balances::write(recipient, balances::read(recipient) + amount);
        Transfer(sender, recipient, amount);
    }

    fn spend_allowance(owner: felt, spender: felt, amount: u256) {
        let current_allowance = allowances::read((owner, spender));
        let ONES_MASK = 0xffffffffffffffffffffffffffffffff_u128;
        let is_unlimited_allowance =
            current_allowance.low == ONES_MASK & current_allowance.high == ONES_MASK;
        if !is_unlimited_allowance {
            approve_helper(owner, spender, current_allowance - amount);
        }
    }

    fn approve_helper(owner: felt, spender: felt, amount: u256) {
        assert(spender != 0, 'ERC20: approve from 0');
        allowances::write((owner, spender), amount);
        Approval(owner, spender, amount);
    }
}
