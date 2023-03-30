use result::ResultTrait;
use array::ArrayTrait;


fn test_roll() {
    match roll(1, 2) {
        Result::Ok(_) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_warp() {
    match warp(1, 2) {
        Result::Ok(_) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_declare() {
    match declare('test') {
        Result::Ok(_) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_declare_cairo0() {
    match declare_cairo0('test') {
        Result::Ok(_) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_start_prank() {
    match start_prank(123, 123) {
        Result::Ok(_) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_stop_prank() {
    match stop_prank(123) {
        Result::Ok(class_hash) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_invoke() {
    let mut arr = ArrayTrait::new();
    arr.append(10);
    arr.append(11);
    arr.append(12);
    match invoke(123, 'test', arr) {
        Result::Ok(class_hash) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_mock_call() {
    let mut arr = ArrayTrait::new();
    arr.append(10);
    arr.append(11);
    arr.append(12);
    match mock_call(123, 'test', arr) {
        Result::Ok(()) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_deploy_tp() {
    let mut arr = ArrayTrait::new();
    arr.append(1);
    arr.append(2);
    match deploy_tp(123, 123, arr) {
        Result::Ok(deployed_contract_address) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_deploy() {
    let mut arr = ArrayTrait::new();
    arr.append(1);
    arr.append(2);
    arr.append(3);
    match deploy(
        PreparedContract { contract_address: 123, class_hash: 123, constructor_calldata: arr }
    ) {
        Result::Ok(deployed_contract_address) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_deploy_tp_cairo0() {
    let mut arr = ArrayTrait::new();
    arr.append(1);
    arr.append(2);
    match deploy_tp_cairo0(123, 123, arr) {
        Result::Ok(deployed_contract_address) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_deploy_cairo0() {
    let mut arr = ArrayTrait::new();
    arr.append(1);
    arr.append(2);
    arr.append(3);
    match deploy_cairo0(
        PreparedContract { contract_address: 123, class_hash: 123, constructor_calldata: arr }
    ) {
        Result::Ok(deployed_contract_address) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_prepare() {
    let mut arr = ArrayTrait::new();
    arr.append(0xBAD);
    arr.append(0xC0DE);
    match prepare(0xBEEF, arr) {
        Result::Ok(prepared_contract) => {
            drop(prepared_contract)
        },
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_prepare_cairo0() {
    let mut arr = ArrayTrait::new();
    arr.append(0xBAD);
    arr.append(0xC0DE);
    match prepare_cairo0(0xBEEF, arr) {
        Result::Ok(prepared_contract) => {
            drop(prepared_contract)
        },
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_deploy_contract() {
    let mut arr = ArrayTrait::new();
    arr.append(0xBAD);
    arr.append(0xC0DE);
    match deploy_contract(0xBEEF, arr) {
        Result::Ok(_) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_deploy_contract_cairo0() {
    let mut arr = ArrayTrait::new();
    arr.append(0xBAD);
    arr.append(0xC0DE);
    match deploy_contract_cairo0(0xBEEF, arr) {
        Result::Ok(_) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

fn test_call() {
    let mut arr = ArrayTrait::new();
    arr.append(12);
    arr.append(23);
    arr.append(34);
    match call(123, 'test', arr) {
        Result::Ok(return_data) => {},
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}
