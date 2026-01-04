# Starknet Foundry Cast Scripts

This crate provides scripts using Starknet Foundry's Cast package.

## Prerequisites

Running these scripts will likely require an already-deployed account with funds to execute.
This crate can also be tested with [starknet-devnet](https://github.com/0xSpaceShard/starknet-devnet-rs).

## Usage

`cd` into the `sncast_scripts/` directory.
Run the command:

```bash
sncast script run <SCRIPT> --url <URL>
```

## Scripts

### `declare_presets`

Declares the preset contracts.
Here's the command using a starkli-style account:

```bash
sncast \
--account path/to/account.json \
--keystore path/to/key.json \
script run declare_presets \
--url http://127.0.0.1:5050
```
