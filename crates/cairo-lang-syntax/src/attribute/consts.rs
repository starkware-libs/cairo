// Attributes which does not invoke any plugin, and thus are not declared in the plugin crate.

/// An attribute that can be used to make the formatter ignore the formatting of a statement or an
/// item, and keep the user defined formatting.
pub const FMT_SKIP_ATTR: &str = "cairofmt::skip";

/// An attribute to mark a function as a function that should be inlined.
pub const INLINE_ATTR: &str = "inline";

/// An attribute to define a type as a type that must be used, or a function as a function that its
/// return value must be used.
pub const MUST_USE_ATTR: &str = "must_use";

/// An attribute to define a type as an unstable type that shouldn't be used, or a function as an
/// unstable function that shouldn't be used. Usage of these items will result in a warning, unless
/// the using crate is marked with their feature active.
pub const UNSTABLE_ATTR: &str = "unstable";

/// An attribute to allow usage of a feature under a statement.
pub const FEATURE_ATTR: &str = "feature";

/// An attribute to define the order of implicit arguments.
pub const IMPLICIT_PRECEDENCE_ATTR: &str = "implicit_precedence";

/// An attribute for the declaration of a starknet interface.
/// It is used in the starknet crate, however it is defined here because it is currently used in the
/// corelib.
/// TODO(Gil): Remove this once `starknet` is removed from corelib.
pub const STARKNET_INTERFACE_ATTR: &str = "starknet::interface";
