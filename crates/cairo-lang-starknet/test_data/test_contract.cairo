#[abi]
trait IAnotherContract {
    fn foo(a: u128) -> u128;
}


#[contract]
mod TestContract {
    struct Storage {
        my_storage_var: felt
    }

    fn internal_func() -> felt {
        1
    }

    #[external]
    fn test(ref arg: felt, arg1: felt, arg2: felt) -> felt {
        let mut x = my_storage_var::read();
        x += 1;
        my_storage_var::write(x);
        x + internal_func()
    }

    #[external]
    fn empty() {}

    #[external]
    fn call_foo(a: u128) -> u128 {
        // TODO(ilya): pass the address of foo as an argument.
        let foo_address = starknet::contract_address_const::<17>();
        super::IAnotherContractDispatcher::foo(foo_address, a)
    }
}

fn end_presentation(questions: Span::<Option::<Question>>) -> Option::<Array::<Answer>> {
    let mut answers = ArrayTrait::new();
    if true {
        answers.append(question?.answer());
    }
    Some(answers)
}

