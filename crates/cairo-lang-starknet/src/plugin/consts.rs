pub const ABI_TRAIT: &str = "__abi";
pub const EXTERNAL_MODULE: &str = "__external";
pub const L1_HANDLER_MODULE: &str = "__l1_handler";
pub const CONSTRUCTOR_MODULE: &str = "__constructor";
pub const STORAGE_STRUCT_NAME: &str = "Storage";

pub const VIEW_ATTR: &str = "view";
pub const EVENT_ATTR: &str = "event";
pub(super) const ABI_ATTR: &str = "abi";
pub(super) const ACCOUNT_CONTRACT_ATTR: &str = "account_contract";
pub(super) const CONTRACT_ATTR: &str = "contract";
pub(super) const EXTERNAL_ATTR: &str = "external";
pub(super) const L1_HANDLER_ATTR: &str = "l1_handler";
pub(super) const CONSTRUCTOR_ATTR: &str = "constructor";
pub(super) const RAW_OUTPUT_ATTR: &str = "raw_output";

pub(super) const EXECUTE_ENTRY_POINT_NAME: &str = "__execute__";
pub(super) const VALIDATE_ENTRY_POINT_NAME: &str = "__validate__";
pub(super) const VALIDATE_DECLARE_ENTRY_POINT_NAME: &str = "__validate_declare__";
pub(super) const VALIDATE_DEPLOY_ENTRY_POINT_NAME: &str = "__validate_deploy__";

pub(super) const ACCOUNT_CONTRACT_ENTRY_POINTS: [&str; 4] = [
    EXECUTE_ENTRY_POINT_NAME,
    VALIDATE_ENTRY_POINT_NAME,
    VALIDATE_DECLARE_ENTRY_POINT_NAME,
    VALIDATE_DEPLOY_ENTRY_POINT_NAME,
];

pub(super) const L1_HANDLER_FIRST_PARAM_NAME: &str = "from_address";
