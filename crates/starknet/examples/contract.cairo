trait IERC20 { func test() -> felt; }

impl ERC20Impl of IERC20 { func test() -> felt {
    return 1;
}
}

#[contract(ERC20Impl, imp2)]
struct ERC20 { }
