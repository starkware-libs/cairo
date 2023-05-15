use array::ArrayTrait;
use option::OptionTrait;
use clone::Clone;
use array::ArrayTCloneImpl;

extern fn start_roll(
    block_number: felt252, target_contract_address: felt252
) -> Result::<(), felt252> nopanic;

extern fn stop_roll(target_contract_address: felt252) -> Result::<(), felt252> nopanic;

extern fn start_warp(
    block_timestamp: felt252, target_contract_address: felt252
) -> Result::<(), felt252> nopanic;

extern fn stop_warp(target_contract_address: felt252) -> Result::<(), felt252> nopanic;

extern fn start_prank(
    caller_address: felt252, target_contract_address: felt252
) -> Result::<(), felt252> nopanic;

extern fn stop_prank(target_contract_address: felt252) -> Result::<(), felt252> nopanic;

extern fn declare(contract: felt252) -> Result::<felt252, felt252> nopanic;

extern fn declare_cairo0(contract: felt252) -> Result::<felt252, felt252> nopanic;

#[derive(Drop, Clone)]
struct RevertedTransaction {
    panic_data: Array::<felt252>, 
}

trait RevertedTransactionTrait {
    fn first(self: @RevertedTransaction) -> felt252;
}

impl RevertedTransactionImpl of RevertedTransactionTrait {
    fn first(self: @RevertedTransaction) -> felt252 {
        *self.panic_data.at(0_usize)
    }
}

extern fn invoke_impl(
    contract_address: felt252, function_name: felt252, calldata: @Array::<felt252>
) -> Result::<(), Array<felt252>> nopanic;

fn invoke(
    contract_address: felt252, function_name: felt252, calldata: @Array::<felt252>
) -> Result::<(), RevertedTransaction> nopanic {
    match invoke_impl(contract_address, function_name, calldata) {
        Result::Ok(x) => Result::<(), RevertedTransaction>::Ok(x),
        Result::Err(x) => Result::<(),
        RevertedTransaction>::Err(RevertedTransaction { panic_data: x })
    }
}

extern fn mock_call(
    contract_address: felt252, function_name: felt252, response: @Array::<felt252>
) -> Result::<(), felt252> nopanic;

#[derive(Drop, Clone)]
struct PreparedContract {
    contract_address: felt252,
    class_hash: felt252,
    constructor_calldata: @Array::<felt252>,
}

// returns deployed `contract_address`
extern fn deploy_impl(
    prepared_contract_address: felt252,
    prepared_class_hash: felt252,
    prepared_constructor_calldata: @Array::<felt252>
) -> Result::<felt252, Array<felt252>> nopanic;

fn deploy(prepared_contract: PreparedContract) -> Result::<felt252, RevertedTransaction> nopanic {
    let PreparedContract{contract_address, class_hash, constructor_calldata } = prepared_contract;
    match deploy_impl(contract_address, class_hash, constructor_calldata) {
        Result::Ok(x) => Result::<felt252, RevertedTransaction>::Ok(x),
        Result::Err(x) => Result::<felt252,
        RevertedTransaction>::Err(RevertedTransaction { panic_data: x })
    }
}

extern fn prepare_impl(
    class_hash: felt252, calldata: @Array::<felt252>
) -> Result::<(Array::<felt252>, felt252, felt252), felt252> nopanic;

fn prepare(
    class_hash: felt252, calldata: @Array::<felt252>
) -> Result::<PreparedContract, felt252> nopanic {
    match prepare_impl(class_hash, calldata) {
        Result::Ok((
            constructor_calldata, contract_address, class_hash
        )) => Result::<PreparedContract,
        felt252>::Ok(
            PreparedContract {
                constructor_calldata: @constructor_calldata,
                contract_address: contract_address,
                class_hash: class_hash,
            }
        ),
        Result::Err(x) => Result::<PreparedContract, felt252>::Err(x)
    }
}

fn deploy_contract(
    contract: felt252, calldata: @Array::<felt252>
) -> Result::<felt252, RevertedTransaction> {
    let mut class_hash: Option::<felt252> = Option::None(());
    match declare(contract) {
        Result::Ok(x) => {
            class_hash = Option::Some(x);
        },
        Result::Err(x) => {
            let mut panic_data = ArrayTrait::new();
            panic_data.append(x);

            return Result::<felt252, RevertedTransaction>::Err(RevertedTransaction { panic_data });
        }
    }

    let mut prepared_contract: Option::<PreparedContract> = Option::None(());
    match prepare(class_hash.unwrap(), calldata) {
        Result::Ok(x) => {
            prepared_contract = Option::Some(x);
        },
        Result::Err(x) => {
            let mut panic_data = ArrayTrait::new();
            panic_data.append(x);

            return Result::<felt252, RevertedTransaction>::Err(RevertedTransaction { panic_data });
        }
    }
    deploy(prepared_contract.unwrap())
}

fn deploy_contract_cairo0(
    contract: felt252, calldata: @Array::<felt252>
) -> Result::<felt252, RevertedTransaction> {
    let mut class_hash: Option::<felt252> = Option::None(());
    match declare_cairo0(contract) {
        Result::Ok(x) => {
            class_hash = Option::Some(x);
        },
        Result::Err(x) => {
            let mut panic_data = ArrayTrait::new();
            panic_data.append(x);

            return Result::<felt252, RevertedTransaction>::Err(RevertedTransaction { panic_data });
        }
    }

    let mut prepared_contract: Option::<PreparedContract> = Option::None(());
    match prepare(class_hash.unwrap(), calldata) {
        Result::Ok(x) => {
            prepared_contract = Option::Some(x);
        },
        Result::Err(x) => {
            let mut panic_data = ArrayTrait::new();
            panic_data.append(x);

            return Result::<felt252, RevertedTransaction>::Err(RevertedTransaction { panic_data });
        }
    }
    deploy(prepared_contract.unwrap())
}

extern fn call_impl(
    contract: felt252, function_name: felt252, calldata: @Array::<felt252>
) -> Result::<Array<felt252>, Array<felt252>> nopanic;


fn call(
    contract: felt252, function_name: felt252, calldata: @Array::<felt252>
) -> Result::<Array<felt252>, RevertedTransaction> nopanic {
    match call_impl(contract, function_name, calldata) {
        Result::Ok(x) => Result::<Array<felt252>, RevertedTransaction>::Ok(x),
        Result::Err(x) => Result::<Array<felt252>,
        RevertedTransaction>::Err(RevertedTransaction { panic_data: x })
    }
}

