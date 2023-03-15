use array::ArrayTrait;
use traits::Into;
use starknet::ContractAddressIntoFelt252;
use option::Option;

// Usage:
//
// use debug::PrintTrait;
//
// 1.print();
//
// (1 == 2).print();
//
// get_caller_address().print();
//
// let mut arr = ArrayTrait::new();
// arr.append('1234567890123456789012345678901');
// arr.append('Sca');
// arr.append('SomeVeryLongMessage');
// arr.print();

extern fn print(message: Array<felt252>) nopanic;

fn print_felt252(message: felt252) {
    let mut arr = ArrayTrait::new();
    arr.append(message);
    print(arr);
}

trait PrintTrait<T> {
    fn print(self: T);
}

impl Felt252PrintImpl of PrintTrait::<felt252> {
    fn print(self: felt252) {
        print_felt252(self);
    }
}

impl BoolPrintImpl of PrintTrait::<bool> {
    fn print(self: bool) {
        if self {
            'true'.print();
        } else {
            'false'.print();
        }
    }
}

impl ContractAddressPrintImpl of PrintTrait::<starknet::ContractAddress> {
    fn print(self: starknet::ContractAddress) {
        self.into().print();
    }
}

impl U8PrintImpl of PrintTrait::<u8> {
    fn print(self: u8) {
        self.into().print();
    }
}

impl U64PrintImpl of PrintTrait::<u64> {
    fn print(self: u64) {
        self.into().print();
    }
}

impl U128PrintImpl of PrintTrait::<u128> {
    fn print(self: u128) {
        self.into().print();
    }
}

impl U256PrintImpl of PrintTrait::<u256> {
    fn print(self: u256) {
        self.low.into().print();
        self.high.into().print();
    }
}

impl ArrayGenericPrintImpl of PrintTrait::<Array::<felt252>> {
    fn print(mut self: Array::<felt252>) {
        print(self);
    }
}
