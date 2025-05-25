// Remove when these are added as actual keywords.
pub const SELF_TYPE_KW: &str = "Self";
pub const SELF_PARAM_KW: &str = "self";
pub const SUPER_KW: &str = "super";
pub const CRATE_KW: &str = "crate";

// Macro related keywords. Notice that the `$` is not included here as it is only a prefix and not a
// part of the segment.
/// The modifier for a macro definition site.
pub const MACRO_DEF_SITE: &str = "defsite";
pub const MACRO_CALL_SITE: &str = "callsite";
