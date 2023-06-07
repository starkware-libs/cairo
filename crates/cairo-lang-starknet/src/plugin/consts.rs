pub const ABI_TRAIT: &str = "__abi";
pub const EXTERNAL_MODULE: &str = "__external";
pub const L1_HANDLER_MODULE: &str = "__l1_handler";
pub const CONSTRUCTOR_MODULE: &str = "__constructor";
pub const STORAGE_STRUCT_NAME: &str = "Storage";

// TODO(spapini): Remove this attribute. It's for the old contract syntax.
pub const EVENT_ATTR: &str = "event";
pub const INTERFACE_ATTR: &str = "starknet::interface";
pub const IMPL_ATTR: &str = "starknet::imp";
pub(super) const CONTRACT_ATTR: &str = "starknet::contract";
pub const STORAGE_ATTR: &str = "starknet::storage";
pub const EXTERNAL_ATTR: &str = "starknet::external";
pub const L1_HANDLER_ATTR: &str = "starknet::l1_handler";
pub const CONSTRUCTOR_ATTR: &str = "starknet::constructor";
pub(super) const RAW_OUTPUT_ATTR: &str = "raw_output";

pub(super) const L1_HANDLER_FIRST_PARAM_NAME: &str = "from_address";
pub(super) const CALLDATA_PARAM_NAME: &str = "__calldata__";

/// Starknet OS required implicit precedence.
pub(super) const IMPLICIT_PRECEDENCE: &[&str] = &[
    "Pedersen",
    "RangeCheck",
    "Bitwise",
    "EcOp",
    "Poseidon",
    "SegmentArena",
    "GasBuiltin",
    "System",
];
