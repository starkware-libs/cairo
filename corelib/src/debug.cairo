use array::ArrayTrait;
use traits::Into;
use starknet::ContractAddressIntoFelt;


extern fn print(message: Array::<felt>) nopanic;

fn print_felt(message: felt) {
    let mut arr = ArrayTrait::new();
    arr.append(message);
    print(arr);
}

trait PrintTrait<T> {
    fn print(self: T);
}

impl FeltPrintImpl of PrintTrait::<felt> {
    fn print(self: felt) {
        print_felt(self);
    }
}

impl ContractAddressPrintImpl of PrintTrait::<ContractAddress> {
    fn print(self: ContractAddress) {
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
