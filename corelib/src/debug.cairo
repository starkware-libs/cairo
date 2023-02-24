use array::ArrayTrait;
use traits::Into;
use starknet::ContractAddressIntoFelt;
use option::Option;


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

impl BoolPrintImpl of PrintTrait::<bool> {
    fn print(self: bool) {
        if self {
            print_felt('true');
        } else {
            print_felt('false');
        }
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

impl ArrayGenericPrintImpl of PrintTrait::<Array::<felt>> {
    fn print(mut self: Array::<felt>) {
        match self.pop_front() {
            Option::Some(e) => {
                e.print();
                print(self);
            },
            Option::None(_) => {},
        }
    }
}
