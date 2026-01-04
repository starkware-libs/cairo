## Account

> **NOTE:** This document is better viewed at [https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/account](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/account)

This crate provides components to implement account contracts that can be used for interacting with the network.

- `Account` validates transactions from signatures over the
[STARK Curve](https://docs.starknet.io/architecture-and-concepts/cryptography/#the_stark_curve).

- `EthAccount` validates transactions from signatures over the
[Secp256k1 curve](https://en.bitcoin.it/wiki/Secp256k1).

### Interfaces

- [`ISRC6`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/account#ISRC6)
- [`ISRC9_V2`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/account#ISRC9_V2)

### Components

- [`AccountComponent`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/account#AccountComponent)
- [`EthAccountComponent`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/account#EthAccountComponent)
- [`SRC9Component`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/account#SRC9Component)
