pub mod erc20;
pub mod extensions;
pub mod interface;
pub mod snip12_utils;

pub use erc20::{DefaultConfig, ERC20Component, ERC20HooksEmptyImpl};
pub use interface::{ERC20ABIDispatcher, ERC20ABIDispatcherTrait};
