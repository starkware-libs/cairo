#[starknet::interface]
trait IBalance<T> {
    // Returns the current balance.
    fn get(self: @T) -> u128;
    // Increases the balance by the given amount.
    fn increase(ref self: T, a: u128);
}

#[starknet::contract]
mod Balance {
    #[storage]
    struct Storage {
        value: u128,
    }

    #[constructor]
    fn constructor(ref self: ContractState, value_: u128) {
        self.value.write(value_);
    }

    #[abi(embed_v0)]
    impl Balance of super::IBalance<ContractState> {
        fn get(self: @ContractState) -> u128 {
            self.value.read()
        }
        fn increase(ref self: ContractState, a: u128) {
            self.value.write(self.value.read() + a);
        }
    }
}

#[cfg(test)]
mod tests {
    use starknet::syscalls::deploy_syscall;
    use super::{Balance, IBalanceDispatcher, IBalanceDispatcherTrait};

    #[test]
    fn test_flow() {
        let (address0, _) = deploy_syscall(Balance::TEST_CLASS_HASH, 0, [100].span(), false)
            .unwrap();
        let mut contract0 = IBalanceDispatcher { contract_address: address0 };

        let (address1, _) = deploy_syscall(Balance::TEST_CLASS_HASH, 0, [200].span(), false)
            .unwrap();
        let mut contract1 = IBalanceDispatcher { contract_address: address1 };

        assert_eq!(contract0.get(), 100);
        assert_eq!(contract1.get(), 200);
        @contract1.increase(200);
        assert_eq!(contract0.get(), 100);
        assert_eq!(contract1.get(), 400);
    }
}
