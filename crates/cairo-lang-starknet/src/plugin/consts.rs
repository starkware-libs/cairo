use const_format::formatcp;

pub const EXTERNAL_MODULE: &str = "__external";
pub const L1_HANDLER_MODULE: &str = "__l1_handler";
pub const CONSTRUCTOR_MODULE: &str = "__constructor";
pub const WRAPPER_PREFIX: &str = "__wrapper__";
pub const STORAGE_STRUCT_NAME: &str = "Storage";
pub const EVENT_TYPE_NAME: &str = "Event";
pub const STORAGE_MAPPING: &str = "Map";
pub const CONTRACT_STATE_NAME: &str = "ContractState";
pub const GENERIC_CONTRACT_STATE_NAME: &str = "TContractState";
pub const COMPONENT_STATE_NAME: &str = "ComponentState";
pub const GENERIC_COMPONENT_STATE_NAME: &str =
    formatcp!("{}<{}>", COMPONENT_STATE_NAME, GENERIC_CONTRACT_STATE_NAME);
pub const CONCRETE_COMPONENT_STATE_NAME: &str =
    formatcp!("{}<{}>", COMPONENT_STATE_NAME, CONTRACT_STATE_NAME);

// TODO(spapini): Remove this attribute. It's for the old contract syntax.
pub const DEPRECATED_ABI_ATTR: &str = "abi";
pub const EVENT_ATTR: &str = "event";
pub const EVENT_TRAIT: &str = "starknet::Event";
pub const STORE_TRAIT: &str = "starknet::Store";
pub const DERIVE_STORAGE_TRAIT: &str = "starknet::storage_access::DeriveStorage";
pub const STORAGE_AS_POINTER_TRAIT: &str = "starknet::storage::StorageAsPointer";
pub const STORAGE_AS_PATH_TRAIT: &str = "starknet::storage::StorageAsPath";
pub const STORAGE_NODE_ATTR: &str = "starknet::storage_node";
pub const STORAGE_SUB_POINTERS_ATTR: &str = "starknet::sub_pointers";
pub const INTERFACE_ATTR: &str = "starknet::interface";
pub(super) const DEPRECATED_CONTRACT_ATTR: &str = "contract";
pub const CONTRACT_ATTR: &str = "starknet::contract";
pub const CONTRACT_ATTR_ACCOUNT_ARG: &str = "account";
pub const COMPONENT_ATTR: &str = "starknet::component";
pub const STORAGE_ATTR: &str = "storage";
pub const EXTERNAL_ATTR: &str = "external";
pub const EMBEDDABLE_ATTR: &str = "starknet::embeddable";
pub const L1_HANDLER_ATTR: &str = "l1_handler";
pub const CONSTRUCTOR_ATTR: &str = "constructor";
pub const CONSTRUCTOR_NAME: &str = "constructor";
pub(super) const RAW_OUTPUT_ATTR: &str = "raw_output";
pub const EMBEDDABLE_AS_ATTR: &str = "embeddable_as";
pub const COMPONENT_INLINE_MACRO: &str = "component";
pub const HAS_COMPONENT_TRAIT: &str = "HasComponent";
pub const SUBSTORAGE_ATTR: &str = "substorage";
pub const RENAME_ATTR: &str = "rename";
pub const NESTED_ATTR: &str = "nested";
pub const FLAT_ATTR: &str = "flat";
pub const KEY_ATTR: &str = "key";
pub const SERDE_ATTR: &str = "serde";

pub const VALIDATE_ENTRY_POINT_SELECTOR: &str = "__validate__";
pub const EXECUTE_ENTRY_POINT_SELECTOR: &str = "__execute__";
pub const ACCOUNT_CONTRACT_ENTRY_POINT_SELECTORS: &[&str] =
    &[VALIDATE_ENTRY_POINT_SELECTOR, EXECUTE_ENTRY_POINT_SELECTOR];
pub const VALIDATE_DEPLOY_ENTRY_POINT_SELECTOR: &str = "__validate_deploy__";

// ABI attribute
pub const ABI_ATTR: &str = "abi";
pub const ABI_ATTR_PER_ITEM_ARG: &str = "per_item";
pub const ABI_ATTR_EMBED_V0_ARG: &str = "embed_v0";

pub(super) const L1_HANDLER_FIRST_PARAM_NAME: &str = "from_address";
pub(super) const CALLDATA_PARAM_NAME: &str = "__calldata__";

/// Starknet OS required implicit precedence.
pub(super) const IMPLICIT_PRECEDENCE: &[&str] = &[
    "core::pedersen::Pedersen",
    "core::RangeCheck",
    "core::integer::Bitwise",
    "core::ec::EcOp",
    "core::poseidon::Poseidon",
    "core::SegmentArena",
    "core::circuit::RangeCheck96",
    "core::circuit::AddMod",
    "core::circuit::MulMod",
    "core::gas::GasBuiltin",
    "System",
];
