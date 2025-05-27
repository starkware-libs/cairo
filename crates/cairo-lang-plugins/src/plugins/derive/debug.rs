use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::PluginTypeInfo;
use crate::plugins::utils::TypeVariant;

/// Adds derive result for the `Debug` trait.
pub fn handle_debug(info: &PluginTypeInfo<'_>) -> String {
    const DEBUG_TRAIT: &str = "core::fmt::Debug";
    let header = info.impl_header(DEBUG_TRAIT, &[DEBUG_TRAIT]);
    let full_typename = info.full_typename();
    let name = &info.name;
    let body = indent_by(
        8,
        match &info.type_variant {
            TypeVariant::Enum => {
                formatdoc! {"
                match self {{
                    {}
                }}",
                indent_by(4,
                    info.members_info.iter().map(|variant|
                    formatdoc!(
                        "
                            {ty}::{variant}(x) => {{
                                write!(f, \"{ty}::{variant}(\")?;
                                {imp}::fmt(x, ref f)?;
                                write!(f, \")\")
                            }},
                        ",
                        ty=info.name,
                        variant=variant.name,
                        imp=variant.impl_name(DEBUG_TRAIT),
                    )).join("\n"))}
            }
            TypeVariant::Struct => {
                formatdoc!(
                    "
                        write!(f, \"{name} {{{{\")?;{}
                        write!(f, \" }}}}\")",
                    info.members_info
                        .iter()
                        .map(|member| formatdoc!(
                            "

                                write!(f, \" {member}: \")?;
                                {imp}::fmt(self.{member}, ref f)?;",
                            member = member.name,
                            imp = member.impl_name(DEBUG_TRAIT),
                        ))
                        .join("\nwrite!(f, \",\")?;"),
                )
            }
        },
    );

    formatdoc! {"
        {header} {{
            fn fmt(self: @{full_typename}, ref f: core::fmt::Formatter) -> core::result::Result::<(), core::fmt::Error> {{
                {body}
            }}
        }}
    "}
}
