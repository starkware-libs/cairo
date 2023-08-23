pub const EXTERNAL_MODULE: &str = "__external";
pub const L1_HANDLER_MODULE: &str = "__l1_handler";
pub const CONSTRUCTOR_MODULE: &str = "__constructor";
pub const WRAPPER_PREFIX: &str = "__wrapper_";
pub const STORAGE_STRUCT_NAME: &str = "Storage";
pub const EVENT_TYPE_NAME: &str = "Event";
pub const CONTRACT_STATE_NAME: &str = "ContractState";

// TODO(spapini): Remove this attribute. It's for the old contract syntax.
pub const DEPRECATED_ABI_ATTR: &str = "abi";
pub const EVENT_ATTR: &str = "event";
pub const INTERFACE_ATTR: &str = "starknet::interface";
pub(super) const DEPRECATED_CONTRACT_ATTR: &str = "contract";
pub(super) const CONTRACT_ATTR: &str = "starknet::contract";
pub(super) const COMPONENT_ATTR: &str = "starknet::component";
pub const STORAGE_ATTR: &str = "storage";
pub const EXTERNAL_ATTR: &str = "external";
pub const EMBEDDABLE_ATTR: &str = "starknet::embeddable";
pub const EMBED_ATTR: &str = "embed";
pub const L1_HANDLER_ATTR: &str = "l1_handler";
pub const CONSTRUCTOR_ATTR: &str = "constructor";
pub const CONSTRUCTOR_NAME: &str = "constructor";
pub(super) const RAW_OUTPUT_ATTR: &str = "raw_output";
pub const EMBEDDABLE_AS_ATTR: &str = "embeddable_as";

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
