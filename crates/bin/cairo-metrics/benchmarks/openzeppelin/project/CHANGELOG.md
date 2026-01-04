<!-- markdownlint-disable MD024 -->

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 2.0.0 (2025-06-18)

### Added

- ERC4626Component (#1170)
- The openzeppelin_macros package with the `with_components` macro (#1282)
- Support for granting a role with delay in AccessControl component (#1317)
- The `type_hash` macro (#1399)
- Enable Governor modules in the `with_components` macro (#1414)
- `Math::u256_mul_div` (#1170)

### Changed

- Bump scarb to v2.11.4 (#1373)

### Changed (Breaking)

- Add SRC-107 to ERC20Component (#1294)
  - `decimals` are now configurable using the ImmutableConfig trait
- Update UDC interface and preset for backward compatibility with v1 (#1371)
  - Change `from_zero` argument to `not_from_zero` in both the interface and the
    ContractDeployed event
  - Add `deployContract` function to the preset
  - Update salt hashing algorithm from Poseidon to Pedersen
- Update ISRC6 interface to match latest changes reflected in the SNIP (#1383)
  - `__execute__` entry point now doesn't return any value
  - Account and EthAccount components SRC6 implementation updated accordingly

## 2.0.0-alpha.1 (2025-04-26)

### Added

- The `type_hash` macro (#1399)
- Enable Governor modules in the `with_components` macro (#1414)

## 2.0.0-alpha.0 (2025-03-20)

### Added

- Support for granting a role with delay in AccessControl component (#1317)
- The openzeppelin_macros package with the `with_components` macro (#1282)
- ERC4626Component (#1170)
- `Math::u256_mul_div` (#1170)

### Changed

- Bump scarb to v2.11.1 (#1373)
- Bump scarb to v2.10.1 (#1358)

### Changed (Breaking)

- Add SRC-107 to ERC20Component (#1294)
  - `decimals` are now configurable using the ImmutableConfig trait
- Update UDC interface and preset for backward compatibility with v1 (#1371)
  - Change `from_zero` argument to `not_from_zero` in both the interface and the
    ContractDeployed event
  - Add `deployContract` function to the preset
  - Update salt hashing algorithm from Poseidon to Pedersen
- Update ISRC6 interface to match latest changes reflected in the SNIP (#1383)
  - `__execute__` entry point now doesn't return any value
  - Account and EthAccount components SRC6 implementation updated accordingly

## 1.0.0 (2025-02-21)

### Added

- ERC721Component `initializer_no_metadata` (#1278)
- ERC1155Component `initializer_no_metadata` (#1287)
- Unsigned trait restriction to the `average` function (#1310)

### Changed (Breaking)

- Bump scarb to v2.9.4 (#1336)

### Fixed (Breaking)

- SNIP12 TimelockComponent `hash_operation` to use `hash_operation_batch` for single-call operations (#1313)
- Permit and Message SNIP12 type hashes (#1283)

### Fixed

- Multisig component issue arising when removing signers with unchanged quorum (#1315)
- Governor timelock extension salt generation panicking on overflow (#1306)
- SignersInfoStorePacking issue with bit operations (#1316)
- Message type hash in SNIP12 doc (#1274)

## 0.20.0 (2024-12-06)

### Added

- SRC9 (Outside Execution) integration to account presets (#1201)
- `SNIP12HashSpanImpl` to `openzeppelin_utils::cryptography::snip12` (#1180)
- GovernorComponent with the following extensions: (#1180)
  - GovernorCoreExecutionComponent
  - GovernorCountingSimpleComponent
  - GovernorSettingsComponent
  - GovernorTimelockExecutionComponent
  - GovernorVotesQuorumFractionComponent
  - GovernorVotesComponent
- `is_tx_version_valid` utility function to `openzeppelin_account::utils` (#1224)

### Changed

- Remove `mut` from `data` param in `compute_hash_on_elements` (#1206)
- Remove `mut` from `calls` param in `__execute__` function of Account and EthAccount components (#1224)
- Remove `mut` from `calls` param in `__validate__` function of Account and EthAccount components (#1224)

### Changed (Breaking)

- Bump snforge_std to v0.34.0 (#1239)
- Bump scarb to v2.9.1 (#1239)
- The initializer in `OwnableComponent` now checks that `owner` is not the zero address (#1221)
- Add `verifying_contract` member to the `Delegation` struct used in Votes `delegate_by_sig` (#1214)
- VotingUnitsTrait moved from `openzeppelin_governance::votes::votes` to `openzeppelin_governance::votes::VotesComponent` (#1214)
- VestingComponent `release` function won't emit an event or attempt to transfer when the amount is zero (#1209)
- Bump snforge_std to v0.33.0 (#1203)

### Fixed

- Scarb manifest dependencies (#1249):
  - Move `openzeppelin_utils` from dev dep to dep in governance manifest
  - Remove `openzeppelin_utils` as dep in access package
  - Change `openzeppelin_account` to `crate` in `src9.cairo`

## 0.19.0 (2024-11-08)

### Added

- Multisig component (#1193)
- `is_valid_p256_signature` utility function to `openzeppelin_account::utils::signature` (#1189)
- `Secp256r1KeyPair` type and helpers to `openzeppelin_testing::signing` (#1189)
- `all_tokens_of_owner` function to `ERC721EnumerableComponent` fetching all owner's tokens in a single call (#1196)
- Embeddable impls for ERC2981 component (#1173)
  - `ERC2981Info` with read functions for discovering the component's state
  - `ERC2981AdminOwnable` providing admin functions for a token that implements Ownable component
  - `ERC2981AdminAccessControl` providing admin functions for a token that implements AccessControl component

### Changed (Breaking)

- Refactor `openzeppelin_account::utils::secp256k1` module to `openzeppelin_account::utils::secp256_point` (#1189)
  - `Secp256k1PointStorePacking` replaced by a generic `Secp256PointStorePacking`
  - `Secp256k1PointPartialEq` replaced by a generic `Secp256PointPartialEq`
  - `DebugSecp256k1Point` replaced by a generic `DebugSecp256Point`
- Apply underscore pattern to the internal functions of `ERC2981Component` to prevent collisions
with new external functions (#1173)
- Move `Hash` and `PartialEq` impls of `Call` struct from `openzeppelin_governance::timelock::utils` to `openzeppelin_governance::utils` (#1193)

## 0.18.0 (2024-10-17)

### Added

- `VotesComponent` with implementation for ERC721 and ERC20 tokens (#1114)
- `IUpgradeAndCall` interface (#1148)
- `upgrade_and_call` function in UpgradeableComponent's InternalImpl (#1148)
- `ERC20Permit` impl for `ERC20Component` facilitating token approvals via off-chain signatures (#1140)
- `ISNIP12Metadata` interface for discovering name and version of a SNIP-12 impl (#1140)
- `SNIP12MetadataExternal` impl of `ISNIP12Metadata` interface for `ERC20Component` (#1140)

### Changed

- Bump scarb to v2.8.4 (#1146)

### Changed (Breaking)

- Remove `ERC20Votes` component in favor of `VotesComponent` (#1114)
  - `Trace` is now declared as a `storage_node` and now uses `Vec` instead of `StorageArray`.
  - `delegate_by_sig` `signature` param in the `IVotes` interface updated from `Array<felt252>` to `Span<felt252>`.
- Remove `StorageArray` from `openzeppelin_utils` (#1114)
- Bump snforge to 0.31.0
- Remove openzeppelin_utils::selectors (#1163)
- Remove `DualCase dispatchers` (#1163)
  - Remove `try_selector_with_fallback` from `openzeppelin_utils`
  - Remove `unwrap_and_cast` module from `openzeppelin_utils`
  - Remove `openzeppelin_access::accesscontrol::dual_accesscontrol`
  - Remove `openzeppelin_access::ownable::dual_ownable`
  - Remove `openzeppelin_account::dual_account`
  - Remove `openzeppelin_account::dual_eth_account`
  - Remove `openzeppelin_token::erc20::dual20`
  - Remove `openzeppelin_token::erc721::dual721`
  - Remove `openzeppelin_token::erc721::dual721_receiver`
  - Remove `openzeppelin_token::erc1155::dual1155`
  - Remove `openzeppelin_token::erc1155::dual1155_receiver`
- `SRC9Component` now uses `ISRC6Dispatcher` instead of `DualCaseAccount` (#1163)
- `ERC20VotesComponent` now uses `ISRC6Dispatcher` instead of `DualCaseAccount` (#1163)
- `ERC721Component` now uses `IERC721ReceiverDispatcher` instead of `DualCaseERC721Receiver` (#1163)
- `ERC1155Component` now uses `IERC1155ReceiverDispatcher` instead of `DualCaseERC1155Receiver` (#1163)

## 0.17.0 (2024-09-23)

### Added

- `into_base_16_string_no_padding` function to the test helpers (#1137)
- SRC9 (OutsideExecution) component (#1150)
- Vesting component and VestingWallet preset (#1116)

### Changed (Breaking)

- Bump scarb to v2.8.2 (#1137)
- Bump snforge to 0.30.0 (#1137)
- `declare_class`, `declare_and_deploy`, and `declare_and_deploy_at` will skip declaration if the contract is already declared (#1137)
- Bump edition to 2024_07 (#1138)
- `execute_calls` function from account utils (#1150)
  - calls param type changed from `Array<Call>` to `Span<Call>`

### Deprecated

- DualCase dispatchers

## 0.16.0 (2024-08-30)

### Added

- ERC721Enumerable component (#983)
- ERC2981 (NFT Royalty Standard) component (#1091)
- `merkle_tree` package with utilities to verify proofs and multi proofs (#1101)

### Changed

- Bump snforge to v0.27.0 (#1107)
- Bump scarb to v2.8.0 (#1120)

### Changed (Breaking)

- Changed ABI suffix to Trait in dual case account and eth account modules (#1096)
  - `DualCaseAccountABI` renamed to `DualCaseAccountTrait`
  - `DualCaseEthAccountABI` renamed to `DualCaseEthAccountTrait`
- Removed `_accept_ownership` from `OwnableComponent::InternalImpl`

### Fixed

- `OwnableTwoStep` allowing a pending owner to accept ownership after the original owner has renounced ownership (#1119)

## 0.15.1 (2024-08-13)

### Changed

- Remove token dependency from account package (#1100)
- Fix docsite links (#1094)

## 0.15.0 (2024-08-08)

### Added

- TimelockController component (#996)
- HashCall implementation (#996)
- Separated package for each submodule (#1065)
  - `openzeppelin_access`
  - `openzeppelin_account`
  - `openzeppelin_governance`
  - `openzeppelin_introspection`
  - `openzeppelin_presets`
  - `openzeppelin_security`
  - `openzeppelin_token`
  - `openzeppelin_upgrades`
  - `openzeppelin_utils`
- Separated packages intended as [dev-dependencies] (#1084)
  - `openzeppelin_testing`
  - `openzeppelin_test_common`

### Changed

- Bump scarb to v2.7.0-rc.1 (#1025)
- Bump scarb to v2.7.0-rc.2 (#1052)
- Bump scarb to v2.7.0-rc.4 (#1064)
- Bump scarb to v2.7.0 (#1065)

### Changed (Breaking)

- Test utilities moved out of the utils module (#1084).
- Test utilities refactored to match the snforge test runner (#1084).

## 0.15.0-rc.0 (2024-07-8)

### Changed

- `Trace`, `Checkpoint`, and `StorageArray` structs made public.

### Changed (Breaking)

- Removed `num_checkpoints` and `checkpoints` from `ERC20VotesABI`.

## 0.14.0 (2024-06-14)

### Changed (Breaking)

- Migrated to the `2023_11` edition (#995):
  - Component implementations annotated with `#[embeddable_as()]` (e.g: `AccessControlComponent::AccessControl`) are not public anymore. Note that the embeddable versions are still public (e.g: `AccessControlComponent::AccessControlImpl`).
  - Implementations that can be compiler-derived from traits are not public anymore (e.g: `DualCaseAccessControlImpl` is not public while `DualCaseAccessControlTrait` is).
  - `Secp256k1PointPartialEq` and `DebugSecp256k1Point` are not public anymore.
  - `account::utils::execute_single_call` is not public anymore.
  - Presets are not public anymore, since they should be copied into projects, and not directly imported.
  - `Trace` and `Checkpoint` structs are not public anymore, since they are intended to be used in `ERC20Votes`, and not as generic utilities.
  - `StorageArray` is not public anymore, since this implementation is specific to `ERC20Votes`, and is not intended as a generic utility, but as a temporary solution until Starknet native implementation arrives.

- Apply underscore pattern to modules (#993):
  - AccessControlComponent
    - `_set_role_admin` function renamed to `set_role_admin`
  - PausableComponent
    - `_pause` function renamed to `pause`
    - `_unpause` function renamed to `unpause`
  - UpgradeableComponent
    - `_upgrade` function renamed to `upgrade`
  - ERC20Component:
    - `_mint` function renamed to `mint`
    - `_burn` function renamed to `burn`
    - `_update` function renamed to `update`
  - ERC721Component:
    - `_safe_transfer` function renamed to `safe_transfer`
    - `_safe_mint` function renamed to `safe_mint`
    - `_mint` function renamed to `mint`
    - `_transfer` function renamed to `transfer`
    - `_burn` function renamed to `burn`
    - `_update` function renamed to `update`
  - ERC1155Component:
    - `set_base_uri` function renamed to `_set_base_uri`

## 0.13.0 (2024-05-20)

### Added

- Sending transactions section in account docs (#981)
- before_update and after_update hooks to ERC721Component (#978)
- before_update and after_update hooks to ERC1155Component (#982)

### Changed (Breaking)

- ERC721Component internal implementation to support transfer, mint, and burn flows going through an `_update` function (#978)
- ERC721Component implementations now require an ERC721HooksTrait implementation in scope (#978)
- ERC1155Component implementations now require an ERC1155HooksTrait implementation in scope (#982)
- AccountComponent, preset, and dispatcher now require a `signature` param in the public-key-setter functions (#989)
- EthAccountComponent, preset, and dispatcher now require a `signature` param in the public-key-setter functions (#990)

## 0.12.0 (2024-04-21)

### Added

- before_update and after_update hooks to ERC20Component (#951)
- INSUFFICIENT_BALANCE and INSUFFICIENT_ALLOWANCE errors to ERC20Component (#951)
- ERC20Votes component (#951)
- Preset interfaces (#964)
- UDC docs (#954)
- Util functions to precompute addresses (#954)

### Changed

- Allow testing utilities to be importable (#963)
- Utilities documentation (#963)
- Parameter name in `tests::utils::drop_events` (`count` -> `n_events`) (#963)
- Presets to include upgradeable functionality (#964)
- ERC20, ERC721, and ERC1155 presets include Ownable functionality (#964)
- EthAccount
  - Expected signature format changed from `(r, s, y)` to `(r, s)` (#940)

## 0.11.0 (2024-03-29)

### Added

- SNIP12 utilities for on-chain typed messages hash generation (#935)
- Nonces component utility (#935)
- Presets Usage guide (#949)
- UDC preset contract (#919)
- ERC1155Component and ERC1155ReceiverComponent mixins (#941)
- ERC721ReceiverComponent documentation (#945)

### Fixed

- ERC721ReceiverComponent mixin embeddable implementation name (#945)

### Removed

- DualCase SRC5 (#882, #952)

## 0.10.0 (2024-03-07)

### Added

- ERC1155 component and preset (#896)
- Mixin implementations in components (#863)
- ERC721Component functions and Storage member
  - `InternalTrait::_set_base_uri` and `InternalTrait::_base_uri` to handle ByteArrays (#857)
  - `ERC721_base_uri` Storage member to store the base URI (#857)

### Changed

- Change unwrap to unwrap_syscall (#901)
- ERC20Component
  - `IERC20::name` and `IERC20::symbol` return ByteArrays instead of felts (#857)
- ERC721Component
  - `IERC721::name`, `IERC721::symbol`, and `IERC721Metadata::token_uri` return ByteArrays instead of felts (#857)
  - `InternalTrait::initializer` accepts an additional `base_uri` ByteArray parameter (#857)
  - IERC721Metadata SRC5 interface ID. This is changed because of the ByteArray integration (#857)

### Removed

- ERC721Component function and Storage member
  - `InternalTrait::_set_token_uri` because individual token URIs are no longer stored (#857)
  - `ERC721_token_uri` Storage member because individual token URIs are no longer stored (#857)

## 0.9.0 (2024-02-08)

### Added

- EthAccount component and preset (#853)
- Ownable two-step functionality (#809)

### Changed

- Bump scarb to v2.4.4 (#853)
- Bump scarb to v2.5.3 (#898)
- OwnershipTransferred event args are indexed (#809)

### Removed

- Non-standard increase_allowance and decrease_allowance functions in ERC20 contract (#881)

## 0.8.1 (2024-01-23)

### Added

- Documentation for SRC5 migration (#821)
- Usage docs (#823)
- Utilities documentation (#825)
- Documentation for presets (#832)
- Backwards compatibility notice (#861)
- Add automatic version bump to CI (#862)

### Changed

- Use ComponentState in tests (#836)
- Docsite navbar (#838)
- Account events indexed keys (#853)
- Support higher tx versions in Account (#858)
- Bump scarb to v2.4.1 (#858)
- Add security section to Upgrades docs (#861)
