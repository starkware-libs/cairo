# OpenZeppelin Contracts for Cairo

[![Lint and test](https://github.com/OpenZeppelin/cairo-contracts/actions/workflows/test.yml/badge.svg)](https://github.com/OpenZeppelin/cairo-contracts/actions/workflows/test.yml)
[![License](https://img.shields.io/github/license/OpenZeppelin/cairo-contracts)](https://github.com/OpenZeppelin/cairo-contracts/blob/main/LICENSE)
[![Docs](https://img.shields.io/badge/docs-%F0%9F%93%84-yellow)](https://docs.openzeppelin.com/contracts-cairo/1.0.0/)

**A library for secure smart contract development** written in Cairo for [Starknet](https://starkware.co/product/starknet/), a decentralized ZK Rollup.

> [!TIP]
> :mage: **Not sure how to get started?** Check out [Contracts Wizard for Cairo](https://wizard.openzeppelin.com/cairo) — an interactive smart contract generator.

## Usage

### Prepare the environment

Simply [install Cairo and scarb](https://docs.swmansion.com/scarb/download).

### Set up your project

Create a new project and `cd` into it.

```bash
scarb new my_project && cd my_project
```

The contents of `my_project` should look like this:

```bash
$ ls

Scarb.toml src
```

### Install the library

Edit `scarb.toml` and add:

```toml
[dependencies]
openzeppelin = "2.0.0"
```

The previous example would import the entire library. We can also add each package as a separate dependency to improve the building time by not including modules that won't be used:

```toml
[dependencies]
openzeppelin_token = "2.0.0"
```

Build the project to download it:

```bash
$ scarb build

Updating git repository https://github.com/OpenZeppelin/cairo-contracts
Compiling my_project v0.1.0 (~/my_project/Scarb.toml)
Finished release target(s) in 6 seconds
```

### Using the library

Open `src/lib.cairo` and write your contract.

For example, this is how to write an ERC20-compliant contract:

```cairo
#[starknet::contract]
mod MyToken {
    use openzeppelin_token::erc20::{ERC20Component, ERC20HooksEmptyImpl, DefaultConfig};
    use starknet::ContractAddress;

    component!(path: ERC20Component, storage: erc20, event: ERC20Event);

    // ERC20 Mixin
    #[abi(embed_v0)]
    impl ERC20MixinImpl = ERC20Component::ERC20MixinImpl<ContractState>;
    impl ERC20InternalImpl = ERC20Component::InternalImpl<ContractState>;

    #[storage]
    struct Storage {
        #[substorage(v0)]
        erc20: ERC20Component::Storage
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        #[flat]
        ERC20Event: ERC20Component::Event
    }

    #[constructor]
    fn constructor(
        ref self: ContractState,
        initial_supply: u256,
        recipient: ContractAddress
    ) {
        let name = "MyToken";
        let symbol = "MTK";

        self.erc20.initializer(name, symbol);
        self.erc20.mint(recipient, initial_supply);
    }
}
```

## Learn

### Documentation

Check out the [full documentation site](https://docs.openzeppelin.com/contracts-cairo)!

### Cairo

- [Cairo book](https://book.cairo-lang.org/)
- [Cairo language documentation](https://docs.cairo-lang.org/)
- [Starknet documentation](https://docs.starknet.io/)
- [Cairopractice](https://cairopractice.com/)

### Tooling

- [Scarb](https://docs.swmansion.com/scarb)

## Development

> [!NOTE]
> You can track our roadmap and future milestones in our [Github Project](https://github.com/orgs/OpenZeppelin/projects/29/).

OpenZeppelin Contracts for Cairo exists thanks to its contributors. There are many ways you can participate and help build high quality software, make sure to check out the [contribution](CONTRIBUTING.md) guide in advance.

### Set up the project

Clone the repository:

```bash
git clone git@github.com:OpenZeppelin/cairo-contracts.git
```

`cd` into it and build:

```bash
cd cairo-contracts
scarb build -w
```

### Run tests

```bash
snforge test -w
```

## Security

This project is maintained by OpenZeppelin with the goal of providing a secure and reliable library of smart contract components
for the Starknet ecosystem. We address security through risk management in various areas such as engineering and open source best
practices, scoping and API design, multi-layered review processes, and incident response preparedness.

Refer to [SECURITY.md](SECURITY.md) for more details.

Past audits can be found in [`audits/`](./audits).

Smart contracts are an evolving technology and carry a high level of technical risk and uncertainty. Although OpenZeppelin is well known for its security audits, using OpenZeppelin Contracts for Cairo is not a substitute for a security audit.

## License

OpenZeppelin Contracts for Cairo is released under the [MIT License](LICENSE).
