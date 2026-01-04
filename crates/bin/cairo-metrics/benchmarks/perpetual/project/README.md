
<div align="center">

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/starknet-dark.png">
  <source media="(prefers-color-scheme: light)" srcset="assets/starknet-light.png">
  <img alt="Your logo" src="assets/starknet-light.png">
</picture>
</div>

<div align="center">

[![License: Apache2.0](https://img.shields.io/badge/License-Apache2.0-green.svg)](LICENSE)
</div>

# Starknet Perpetual <!-- omit from toc -->

## Table of contents <!-- omit from toc -->

 <!-- omit from toc -->
- [About](#about)
- [Disclaimer](#disclaimer)
- [Dependencies](#dependencies)
- [Installation](#installation)
- [Build and Test](#build-and-test)
- [Implementation specification](#implementation-specification)
- [Audit](#audit)
- [Security](#security)

## About

This repo holds the implementation of Starknet Perpetual Trading contracts.

## Disclaimer

Perpetual is a work in progress.

## Dependencies

- Cairo dependencies such as [Scarb](https://docs.swmansion.com/scarb/) and [Starknet foundry](https://foundry-rs.github.io/starknet-foundry/index.html) - install using [starkup](https://github.com/software-mansion/starkup).

## Installation

Clone the repo and from within the projects root folder run:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.starkup.dev | sh
```

## Build and Test

Build the contracts from the repo root:

```bash
scarb build

```

To run the tests, execute:

```bash
scarb test
```

## Implementation specification

Specs document found [here](docs/spec.md)

## Audit

Find the latest audit report in [docs/audit](docs/audit).

## Security

Starknet Perpetual follows good practices of security, but 100% security cannot be assured. Starknet Perpetual is provided "as is" without any warranty. Use at your own risk.

For more information and to report security issues, please refer to our [security documentation](https://github.com/starkware-libs/starknet-perpetual/blob/main/docs/SECURITY.md).
