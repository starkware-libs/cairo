use array::ArrayTrait;
use traits::Into;

// Usage:
//
// use debug::Print;
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

trait Print<T> {
    fn print(self: T);
}

impl PrintImpl<T, impl TInto: Into::<T, felt252>> of Print::<T> {
    fn print(self: T) {
        print_felt252(self.into());
    }
}

impl Felt252PrintImpl of Print::<felt252> {
    fn print(self: felt252) {
        print_felt252(self);
    }
}

impl BoolPrintImpl of Print::<bool> {
    fn print(self: bool) {
        if self {
            'true'.print();
        } else {
            'false'.print();
        }
    }
}

impl U256PrintImpl of Print::<u256> {
    fn print(self: u256) {
        self.low.into().print();
        self.high.into().print();
    }
}

// Array
impl ArrayTPrintImpl<T, impl TPrint: Print::<T>, impl TDrop: Drop::<T>> of Print::<Array::<T>> {
    fn print(mut self: Array::<T>) {
        match gas::withdraw_gas() {
            Option::Some(_) => {},
            Option::None(_) => {
                let mut data = ArrayTrait::new();
                data.append('Out of gas');
                panic(data);
            },
        }
        match self.pop_front() {
            Option::Some(value) => {
                value.print();
                self.print();
            },
            Option::None(_) => {},
        }
    }
}
