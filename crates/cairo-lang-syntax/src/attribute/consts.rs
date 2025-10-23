// Attributes which do not invoke any plugin, and thus are not declared in the plugin crate.

/// An attribute that can be used to make the formatter ignore the formatting of a statement or an
/// item, and keep the user defined formatting.
pub const FMT_SKIP_ATTR: &str = "cairofmt::skip";

/// An attribute to mark a function as a function that should be inlined.
pub const INLINE_ATTR: &str = "inline";

/// An attribute to define a type as a type that must be used, or a function as a function that its
/// return value must be used.
pub const MUST_USE_ATTR: &str = "must_use";

/// An attribute to define an item as unstable. Usage of these items will result in a warning,
/// unless the using crate is marked with their feature active.
pub const UNSTABLE_ATTR: &str = "unstable";

/// An attribute to define an item as deprecated. Usage of these items will result in a warning,
/// unless the using crate is marked with their feature active.
pub const DEPRECATED_ATTR: &str = "deprecated";

/// An attribute to define an item as internal. Usage of these items will result in an error, unless
/// the usage is marked with their feature active.
pub const INTERNAL_ATTR: &str = "internal";

/// An attribute to allow code that would normally result in a warning.
pub const ALLOW_ATTR: &str = "allow";

/// An attribute to represent the unused lint-group, which represents all the unused lints.
/// An argument to the `allow` attribute that represents a lint-group which suppresses all `unused`
/// lints.
pub const UNUSED: &str = "unused";

/// An argument to the `allow` attribute that suppresses warnings for unused variables.
/// Also included in the [`UNUSED`] lint group.
pub const UNUSED_VARIABLES: &str = "unused_variables";

/// An argument to the `allow` attribute that suppresses warnings for unused imports.
/// Also included in the [`UNUSED`] lint group.
pub const UNUSED_IMPORTS: &str = "unused_imports";

/// An attribute to allow additional attributes on an item.
pub const ALLOW_ATTR_ATTR: &str = "allow_attr";

/// An attribute to allow usage of a feature under a statement.
pub const FEATURE_ATTR: &str = "feature";

/// An attribute to define the order of implicit arguments.
pub const IMPLICIT_PRECEDENCE_ATTR: &str = "implicit_precedence";

/// An attribute for the declaration of a starknet interface.
///
/// It is used in the starknet crate, however it is defined here because it is currently used in the
/// corelib.
/// TODO(Gil): Remove this once `starknet` is removed from corelib.
pub const STARKNET_INTERFACE_ATTR: &str = "starknet::interface";

/// An attribute to define a type as a phantom type, phantom types cannot be created at run time and
/// are typically used for meta-programming.
pub const PHANTOM_ATTR: &str = "phantom";

/// An attribute to override a submodule's source file path.
///
/// Usage: #[path("relative/or/absolute/path.cairo")]
/// Applies to: `mod` items with a semicolon body (file-based modules).
pub const PATH_ATTR: &str = "path";
