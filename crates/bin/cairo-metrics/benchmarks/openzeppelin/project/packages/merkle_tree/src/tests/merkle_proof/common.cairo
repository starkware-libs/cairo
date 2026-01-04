use openzeppelin_testing::AsAddressTrait;
use starknet::ContractAddress;

#[derive(Serde, Copy, Drop, Hash)]
pub(crate) struct Leaf {
    pub address: ContractAddress,
    pub amount: u128,
}

pub(crate) fn LEAVES() -> Span<Leaf> {
    [
        Leaf {
            address: 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc8.as_address(),
            amount: 0xfc104e31d098d1ab488fc1acaeb0269,
        },
        Leaf {
            address: 0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffc66ca5c000.as_address(),
            amount: 0xfc104e31d098d1ab488fc1acaeb0269,
        },
        Leaf {
            address: 0x6a1f098854799debccf2d3c4059ff0f02dbfef6673dc1fcbfffffffffffffc8.as_address(),
            amount: 0xfc104e31d098d1ab488fc1acaeb0269,
        },
        Leaf {
            address: 0xfa6541b7909bfb5e8585f1222fcf272eea352c7e0e8ed38c988bd1e2a85e82.as_address(),
            amount: 0xaa8565d732c2c9fa5f6c001d89d5c219,
        },
    ]
        .span()
}
