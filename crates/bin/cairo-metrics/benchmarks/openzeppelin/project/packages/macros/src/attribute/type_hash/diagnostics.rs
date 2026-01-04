//! List of errors and warnings for the type hash macro.

pub mod errors {
    /// Error when the type hash macro is applied to a struct containing a custom type.
    pub const CUSTOM_TYPE_NOT_SUPPORTED: &str = "Inner custom types are not supported yet.\n";
    /// Error when the type hash macro is applied to an empty block.
    pub const EMPTY_TYPE_FOUND: &str = "No valid type found in the input.\n";
    /// Error when the type hash macro is applied to a non-struct/enum type.
    pub const NOT_VALID_TYPE_TO_DECORATE: &str = "Only structs and enums are supported.\n";
    /// Error when the format of the type_hash attribute is invalid.
    pub const INVALID_TYPE_HASH_ATTRIBUTE_FORMAT: &str =
        "Invalid format for the type_hash attribute. The only valid arguments are: name, debug.\n";
    /// Error when the format of the snip12 attribute is invalid.
    pub const INVALID_SNIP12_ATTRIBUTE_FORMAT: &str =
        "Invalid format for the snip12 attribute. The only valid arguments are: name, kind.\n";
    /// Error when the string argument is invalid.
    pub const INVALID_STRING_ARGUMENT: &str =
        "Invalid string argument. Expected a non-empty string between double quotes.\n";
}
