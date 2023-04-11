extern fn roll(address: felt252, caller_address: felt252) -> Result::<(), felt252> nopanic;

extern fn warp(
    blk_timestamp: felt252, target_contract_address: felt252
) -> Result::<(), felt252> nopanic;

extern fn start_prank(
    caller_address: felt252, target_contract_address: felt252
) -> Result::<(), felt252> nopanic;

extern fn stop_prank(target_contract_address: felt252) -> Result::<(), felt252> nopanic;

extern fn declare(contract: felt252) -> Result::<felt252, felt252> nopanic;

extern fn declare_cairo0(contract: felt252) -> Result::<felt252, felt252> nopanic;

extern fn invoke(
    contract_address: felt252, entry_point_selector: felt252, calldata: Array::<felt252>
) -> Result::<(), felt252> nopanic;

extern fn mock_call(
    contract_address: felt252, entry_point_selector: felt252, response: Array::<felt252>
) -> Result::<(), felt252> nopanic;

struct PreparedContract {
    contract_address: felt252,
    class_hash: felt252,
    constructor_calldata: Array::<felt252>,
}
// returns deployed `contract_address`
extern fn deploy_tp(
    prepared_contract_address: felt252,
    prepared_class_hash: felt252,
    prepared_constructor_calldata: Array::<felt252>
) -> Result::<felt252, felt252> nopanic;

fn deploy(prepared_contract: PreparedContract) -> Result::<felt252, felt252> nopanic {
    let PreparedContract{contract_address, class_hash, constructor_calldata } = prepared_contract;
    deploy_tp(contract_address, class_hash, constructor_calldata)
}

extern fn deploy_tp_cairo0(
    prepared_contract_address: felt252,
    prepared_class_hash: felt252,
    prepared_constructor_calldata: Array::<felt252>
) -> Result::<felt252, felt252> nopanic;

fn deploy_cairo0(prepared_contract: PreparedContract) -> Result::<felt252, felt252> nopanic {
    let PreparedContract{contract_address, class_hash, constructor_calldata } = prepared_contract;
    deploy_tp_cairo0(contract_address, class_hash, constructor_calldata)
}

extern fn prepare_tp(
    class_hash: felt252, calldata: Array::<felt252>
) -> Result::<(Array::<felt252>, felt252, felt252), felt252> nopanic;

fn prepare(
    class_hash: felt252, calldata: Array::<felt252>
) -> Result::<PreparedContract, felt252> nopanic {
    match prepare_tp(class_hash, calldata) {
        Result::Ok((
            constructor_calldata, contract_address, class_hash
        )) => Result::<PreparedContract,
        felt252>::Ok(
            PreparedContract {
                constructor_calldata: constructor_calldata,
                contract_address: contract_address,
                class_hash: class_hash,
            }
        ),
        Result::Err(x) => Result::<PreparedContract, felt252>::Err(x)
    }
}

extern fn prepare_tp_cairo0(
    class_hash: felt252, calldata: Array::<felt252>
) -> Result::<(Array::<felt252>, felt252, felt252), felt252> nopanic;

fn prepare_cairo0(
    class_hash: felt252, calldata: Array::<felt252>
) -> Result::<PreparedContract, felt252> nopanic {
    match prepare_tp_cairo0(class_hash, calldata) {
        Result::Ok((
            constructor_calldata, contract_address, class_hash
        )) => Result::<PreparedContract,
        felt252>::Ok(
            PreparedContract {
                constructor_calldata: constructor_calldata,
                contract_address: contract_address,
                class_hash: class_hash,
            }
        ),
        Result::Err(x) => Result::<PreparedContract, felt252>::Err(x)
    }
}

fn deploy_contract(
    contract: felt252, calldata: Array::<felt252>
) -> Result::<felt252, felt252> nopanic {
    let class_hash: felt252 = declare(contract)?;
    let prepared_contract = prepare(class_hash, calldata)?;
    deploy(prepared_contract)
}

fn deploy_contract_cairo0(
    contract: felt252, calldata: Array::<felt252>
) -> Result::<felt252, felt252> nopanic {
    let class_hash: felt252 = declare_cairo0(contract)?;
    let prepared_contract = prepare(class_hash, calldata)?;
    deploy(prepared_contract)
}

extern fn call(
    contract: felt252, entry_point_selector: felt252, calldata: Array::<felt252>
) -> Result::<(Array::<felt252>), felt252> nopanic;
