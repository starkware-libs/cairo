// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0
// (token/src/erc20/extensions/erc4626/interface.cairo)

use starknet::ContractAddress;

#[starknet::interface]
pub trait IERC4626<TState> {
    /// Returns the address of the underlying token used for the Vault for accounting, depositing,
    /// and withdrawing.
    ///
    /// Requirements:
    ///
    /// - MUST be an ERC20 token contract.
    /// - MUST NOT panic.
    fn asset(self: @TState) -> ContractAddress;

    /// Returns the total amount of the underlying asset that is “managed” by Vault.
    ///
    /// Requirements:
    ///
    /// - SHOULD include any compounding that occurs from yield.
    /// - MUST be inclusive of any fees that are charged against assets in the Vault.
    /// - MUST NOT panic.
    fn total_assets(self: @TState) -> u256;

    /// Returns the amount of shares that the Vault would exchange for the amount of assets
    /// provided irrespective of slippage or fees.
    ///
    /// Requirements:
    ///
    /// - MUST NOT be inclusive of any fees that are charged against assets in the Vault.
    /// - MUST NOT show any variations depending on the caller.
    /// - MUST NOT reflect slippage or other on-chain conditions, when performing the actual
    /// exchange.
    /// - MUST NOT panic unless due to integer overflow caused by an unreasonably large input.
    /// - MUST round down towards 0.
    ///
    /// NOTE: This calculation MAY NOT reflect the "per-user" price-per-share, and instead should
    /// reflect the "average-user's" price-per-share, meaning what the average user should expect to
    /// see when exchanging to and from.
    fn convert_to_shares(self: @TState, assets: u256) -> u256;

    /// Returns the amount of assets that the Vault would exchange for the amount of shares
    /// provided irrespective of slippage or fees.
    ///
    /// Requirements:
    ///
    /// - MUST NOT be inclusive of any fees that are charged against assets in the Vault.
    /// - MUST NOT show any variations depending on the caller.
    /// - MUST NOT reflect slippage or other on-chain conditions, when performing the actual
    /// exchange.
    /// - MUST NOT panic unless due to integer overflow caused by an unreasonably large input.
    /// - MUST round down towards 0.
    ///
    /// NOTE: This calculation MAY NOT reflect the “per-user” price-per-share, and instead
    /// should reflect the “average-user’s” price-per-share, meaning what the average user
    /// should expect to see when exchanging to and from.
    fn convert_to_assets(self: @TState, shares: u256) -> u256;

    /// Returns the maximum amount of the underlying asset that can be deposited into the Vault for
    /// `receiver`, through a deposit call.
    ///
    /// Requirements:
    ///
    /// - MUST return a limited value if receiver is subject to some deposit limit.
    /// - MUST return 2 ** 256 - 1 if there is no limit on the maximum amount of assets that may be
    ///   deposited.
    /// - MUST NOT panic.
    fn max_deposit(self: @TState, receiver: ContractAddress) -> u256;

    /// Allows an on-chain or off-chain user to simulate the effects of their deposit at the current
    /// block, given current on-chain conditions.
    ///
    /// Requirements:
    ///
    /// - MUST return as close to and no more than the exact amount of Vault shares that would be
    ///   minted in a deposit call in the same transaction i.e. deposit should return the same or
    ///   more shares as `preview_deposit` if called in the same transaction.
    /// - MUST NOT account for deposit limits like those returned from `max_deposit` and should
    /// always
    ///   act as though the deposit would be accepted, regardless if the user has enough tokens
    ///   approved, etc.
    /// - MUST be inclusive of deposit fees. Integrators should be aware of the existence of deposit
    ///   fees.
    /// - MUST NOT panic.
    ///
    /// NOTE: Any unfavorable discrepancy between `convert_to_shares` and `preview_deposit`
    /// SHOULD be considered slippage in share price or some other type of condition, meaning the
    /// depositor will lose assets by depositing.
    fn preview_deposit(self: @TState, assets: u256) -> u256;

    /// Mints Vault shares to `receiver` by depositing exactly amount of `assets`.
    ///
    /// Requirements:
    ///
    /// - MUST emit the Deposit event.
    /// - MAY support an additional flow in which the underlying tokens are owned by the Vault
    ///   contract before the deposit execution, and are accounted for during deposit.
    /// - MUST panic if all of assets cannot be deposited (due to deposit limit being reached,
    ///   slippage, the user not approving enough underlying tokens to the Vault contract, etc).
    ///
    /// NOTE: Most implementations will require pre-approval of the Vault with the Vault’s
    /// underlying asset token.
    fn deposit(ref self: TState, assets: u256, receiver: ContractAddress) -> u256;

    /// Returns the maximum amount of the Vault shares that can be minted for the receiver, through
    /// a mint call.
    ///
    /// Requirements:
    ///
    /// - MUST return a limited value if receiver is subject to some mint limit.
    /// - MUST return 2 ** 256 - 1 if there is no limit on the maximum amount of shares that may be
    ///   minted.
    /// - MUST NOT panic.
    fn max_mint(self: @TState, receiver: ContractAddress) -> u256;

    /// Allows an on-chain or off-chain user to simulate the effects of their mint at the current
    /// block, given current on-chain conditions.
    ///
    /// Requirements:
    ///
    /// - MUST return as close to and no fewer than the exact amount of assets that would be
    ///   deposited in a `mint` call in the same transaction. I.e. `mint` should return the same or
    ///   fewer assets as `preview_mint` if called in the same transaction.
    /// - MUST NOT account for mint limits like those returned from `max_mint` and should always act
    ///   as though the mint would be accepted, regardless if the user has enough tokens approved,
    ///   etc.
    /// - MUST be inclusive of deposit fees. Integrators should be aware of the existence of deposit
    ///   fees.
    /// - MUST NOT panic.
    ///
    /// NOTE: Any unfavorable discrepancy between convertToAssets and previewMint SHOULD be
    /// considered slippage in share price or some other type of condition, meaning the depositor
    /// will lose assets by minting.
    fn preview_mint(self: @TState, shares: u256) -> u256;

    /// Mints exactly shares Vault shares to receiver by depositing amount of underlying tokens.
    ///
    /// Requirements:
    ///
    /// - MUST emit the `Deposit` event.
    /// - MAY support an additional flow in which the underlying tokens are owned by the Vault
    ///   contract before the mint execution, and are accounted for during mint.
    /// - MUST panic if all of shares cannot be minted (due to deposit limit being reached,
    /// slippage,
    ///   the user not approving enough underlying tokens to the Vault contract, etc).
    ///
    /// NOTE: Most implementations will require pre-approval of the Vault with the Vault’s
    /// underlying asset token.
    fn mint(ref self: TState, shares: u256, receiver: ContractAddress) -> u256;

    /// Returns the maximum amount of the underlying asset that can be withdrawn from the owner
    /// balance in the Vault, through a withdraw call.
    ///
    /// Requirements:
    ///
    /// - MUST return a limited value if owner is subject to some withdrawal limit or timelock.
    /// - MUST NOT panic.
    fn max_withdraw(self: @TState, owner: ContractAddress) -> u256;

    /// Allows an on-chain or off-chain user to simulate the effects of their withdrawal at the
    /// current block, given current on-chain conditions.
    ///
    /// Requirements:
    ///
    /// - MUST return as close to and no fewer than the exact amount of Vault shares that would be
    ///   burned in a withdraw call in the same transaction i.e. withdraw should return the same or
    ///   fewer shares as preview_withdraw if called in the same transaction.
    /// - MUST NOT account for withdrawal limits like those returned from max_withdraw and should
    ///   always act as though the withdrawal would be accepted, regardless if the user has enough
    ///   shares, etc.
    /// - MUST be inclusive of withdrawal fees. Integrators should be aware of the existence of
    ///   withdrawal fees.
    /// - MUST not panic.
    ///
    /// NOTE: Any unfavorable discrepancy between `convert_to_shares` and `preview_withdraw`
    /// SHOULD be considered slippage in share price or some other type of condition, meaning the
    /// depositor will lose assets by depositing.
    fn preview_withdraw(self: @TState, assets: u256) -> u256;

    /// Burns shares from owner and sends exactly assets of underlying tokens to receiver.
    ///
    /// Requirements:
    ///
    /// - MUST emit the `Withdraw` event.
    /// - MAY support an additional flow in which the underlying tokens are owned by the Vault
    ///   contract before the withdraw execution, and are accounted for during withdraw.
    /// - MUST revert if all of assets cannot be withdrawn (due to withdrawal limit being reached,
    ///   slippage, the owner not having enough shares, etc).
    ///
    /// NOTE: Some implementations will require pre-requesting to the Vault before a withdrawal
    /// may be performed.
    /// Those methods should be performed separately.
    fn withdraw(
        ref self: TState, assets: u256, receiver: ContractAddress, owner: ContractAddress,
    ) -> u256;

    /// Returns the maximum amount of Vault shares that can be redeemed from the owner balance in
    /// the Vault, through a redeem call.
    ///
    /// Requirements:
    ///
    /// - MUST return a limited value if owner is subject to some withdrawal limit or timelock.
    /// - MUST return `ERC20::balance_of(owner)` if `owner` is not subject to any withdrawal limit
    /// or
    ///   timelock.
    /// - MUST NOT panic.
    fn max_redeem(self: @TState, owner: ContractAddress) -> u256;

    /// Allows an on-chain or off-chain user to simulate the effects of their redemption at the
    /// current block, given current on-chain conditions.
    ///
    /// Requirements:
    ///
    /// - MUST return as close to and no more than the exact amount of assets that would be
    /// withdrawn
    ///   in a redeem call in the same transaction i.e. redeem should return the same or more assets
    ///   as preview_redeem if called in the same transaction.
    /// - MUST NOT account for redemption limits like those returned from max_redeem and should
    /// always
    ///   act as though the redemption would be accepted, regardless if the user has enough shares,
    ///   etc.
    /// - MUST be inclusive of withdrawal fees. Integrators should be aware of the existence of
    ///   withdrawal fees.
    /// - MUST NOT panic.
    ///
    /// NOTE: Any unfavorable discrepancy between `convert_to_assets` and `preview_redeem` SHOULD be
    /// considered slippage in share price or some other type of condition, meaning the depositor
    /// will lose assets by redeeming.
    fn preview_redeem(self: @TState, shares: u256) -> u256;

    /// Burns exactly shares from owner and sends assets of underlying tokens to receiver.
    ///
    /// Requirements:
    ///
    /// - MUST emit the `Withdraw` event.
    /// - MAY support an additional flow in which the underlying tokens are owned by the Vault
    ///   contract before the redeem execution, and are accounted for during redeem.
    /// - MUST revert if all of shares cannot be redeemed (due to withdrawal limit being reached,
    ///   slippage, the owner not having enough shares, etc).
    ///
    /// NOTE: Some implementations will require pre-requesting to the Vault before a withdrawal may
    /// be performed.
    /// Those methods should be performed separately.
    fn redeem(
        ref self: TState, shares: u256, receiver: ContractAddress, owner: ContractAddress,
    ) -> u256;
}

#[starknet::interface]
pub trait ERC4626ABI<TState> {
    // IERC4626
    fn asset(self: @TState) -> ContractAddress;
    fn total_assets(self: @TState) -> u256;
    fn convert_to_shares(self: @TState, assets: u256) -> u256;
    fn convert_to_assets(self: @TState, shares: u256) -> u256;
    fn max_deposit(self: @TState, receiver: ContractAddress) -> u256;
    fn preview_deposit(self: @TState, assets: u256) -> u256;
    fn deposit(ref self: TState, assets: u256, receiver: ContractAddress) -> u256;
    fn max_mint(self: @TState, receiver: ContractAddress) -> u256;
    fn preview_mint(self: @TState, shares: u256) -> u256;
    fn mint(ref self: TState, shares: u256, receiver: ContractAddress) -> u256;
    fn max_withdraw(self: @TState, owner: ContractAddress) -> u256;
    fn preview_withdraw(self: @TState, assets: u256) -> u256;
    fn withdraw(
        ref self: TState, assets: u256, receiver: ContractAddress, owner: ContractAddress,
    ) -> u256;
    fn max_redeem(self: @TState, owner: ContractAddress) -> u256;
    fn preview_redeem(self: @TState, shares: u256) -> u256;
    fn redeem(
        ref self: TState, shares: u256, receiver: ContractAddress, owner: ContractAddress,
    ) -> u256;

    // IERC20
    fn total_supply(self: @TState) -> u256;
    fn balance_of(self: @TState, account: ContractAddress) -> u256;
    fn allowance(self: @TState, owner: ContractAddress, spender: ContractAddress) -> u256;
    fn transfer(ref self: TState, recipient: ContractAddress, amount: u256) -> bool;
    fn transfer_from(
        ref self: TState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) -> bool;
    fn approve(ref self: TState, spender: ContractAddress, amount: u256) -> bool;

    // IERC20Metadata
    fn name(self: @TState) -> ByteArray;
    fn symbol(self: @TState) -> ByteArray;
    fn decimals(self: @TState) -> u8;

    // IERC20CamelOnly
    fn totalSupply(self: @TState) -> u256;
    fn balanceOf(self: @TState, account: ContractAddress) -> u256;
    fn transferFrom(
        ref self: TState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) -> bool;
}
