extern fn roll(address: felt, caller_address: felt) -> Result::<(), felt> nopanic;

extern fn warp(blk_timestamp: felt, target_contract_address: felt) -> Result::<(), felt> nopanic;

extern fn start_prank(
    caller_address: felt, target_contract_address: felt
) -> Result::<(), felt> nopanic;

extern fn stop_prank(target_contract_address: felt) -> Result::<(), felt> nopanic;

extern fn declare(contract: felt) -> Result::<felt, felt> nopanic;

extern fn declare_cairo0(contract: felt) -> Result::<felt, felt> nopanic;

extern fn invoke(
    contract_address: felt, entry_point_selector: felt, calldata: Array::<felt>
) -> Result::<(), felt> nopanic;

extern fn mock_call(
    contract_address: felt, entry_point_selector: felt, response: Array::<felt>
) -> Result::<(), felt> nopanic;

struct PreparedContract {
    contract_address: felt,
    class_hash: felt,
    constructor_calldata: Array::<felt>,
}
// returns deployed `contract_address`
extern fn deploy_tp(
    prepared_contract_address: felt,
    prepared_class_hash: felt,
    prepared_constructor_calldata: Array::<felt>
) -> Result::<felt, felt> nopanic;

fn deploy(prepared_contract: PreparedContract) -> Result::<felt, felt> nopanic {
    let PreparedContract{contract_address, class_hash, constructor_calldata } = prepared_contract;
    deploy_tp(contract_address, class_hash, constructor_calldata)
}

extern fn deploy_tp_cairo0(
    prepared_contract_address: felt,
    prepared_class_hash: felt,
    prepared_constructor_calldata: Array::<felt>
) -> Result::<felt, felt> nopanic;

fn deploy_cairo0(prepared_contract: PreparedContract) -> Result::<felt, felt> nopanic {
    let PreparedContract{contract_address, class_hash, constructor_calldata } = prepared_contract;
    deploy_tp_cairo0(contract_address, class_hash, constructor_calldata)
}

extern fn prepare_tp(
    class_hash: felt, calldata: Array::<felt>
) -> Result::<(Array::<felt>, felt, felt), felt> nopanic;

fn prepare(class_hash: felt, calldata: Array::<felt>) -> Result::<PreparedContract, felt> nopanic {
    match prepare_tp(class_hash, calldata) {
        Result::Ok((
            constructor_calldata, contract_address, class_hash
        )) => Result::<PreparedContract,
        felt>::Ok(
            PreparedContract {
                constructor_calldata: constructor_calldata,
                contract_address: contract_address,
                class_hash: class_hash,
            }
        ),
        Result::Err(x) => Result::<PreparedContract, felt>::Err(x)
    }
}

extern fn prepare_tp_cairo0(
    class_hash: felt, calldata: Array::<felt>
) -> Result::<(Array::<felt>, felt, felt), felt> nopanic;

fn prepare_cairo0(
    class_hash: felt, calldata: Array::<felt>
) -> Result::<PreparedContract, felt> nopanic {
    match prepare_tp_cairo0(class_hash, calldata) {
        Result::Ok((
            constructor_calldata, contract_address, class_hash
        )) => Result::<PreparedContract,
        felt>::Ok(
            PreparedContract {
                constructor_calldata: constructor_calldata,
                contract_address: contract_address,
                class_hash: class_hash,
            }
        ),
        Result::Err(x) => Result::<PreparedContract, felt>::Err(x)
    }
}

fn deploy_contract(contract: felt, calldata: Array::<felt>) -> Result::<felt, felt> nopanic {
    let class_hash: felt = declare(contract)?;
    let prepared_contract = prepare(class_hash, calldata)?;
    deploy(prepared_contract)
}

fn deploy_contract_cairo0(contract: felt, calldata: Array::<felt>) -> Result::<felt, felt> nopanic {
    let class_hash: felt = declare_cairo0(contract)?;
    let prepared_contract = prepare_cairo0(class_hash, calldata)?;
    deploy_cairo0(prepared_contract)
}

extern fn call(
    contract: felt, entry_point_selector: felt, calldata: Array::<felt>
) -> Result::<(Array::<felt>), felt> nopanic;
