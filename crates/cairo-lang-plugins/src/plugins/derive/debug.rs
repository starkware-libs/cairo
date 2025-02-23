use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::DeriveInfo;
use crate::plugins::derive::TypeVariantInfo;

/// Adds derive result for the `Debug` trait.
pub fn handle_debug(info: &DeriveInfo) -> Option<String> {
    let header = info.format_impl_header("core::fmt", "Debug", &["core::fmt::Debug"]);
    let full_typename = info.full_typename();
    let name = &info.name;
    let body = indent_by(
        8,
        match &info.specific_info {
            TypeVariantInfo::Enum(variants) => {
                formatdoc! {"
                match self {{
                    {}
                }}",
                indent_by(4,
                variants.iter().map(|variant|
                    formatdoc!(
                        "
                            {ty}::{variant}(x) => {{
                                write!(f, \"{ty}::{variant}(\")?;
                                core::fmt::Debug::fmt(x, ref f)?;
                                write!(f, \")\")
                            }},
                        ",
                        ty=info.name,
                        variant=variant.name,
                    )).join("\n"))}
            }
            TypeVariantInfo::Struct(members) => {
                formatdoc!(
                    "
                        write!(f, \"{name} {{{{\")?;{}
                        write!(f, \" }}}}\")",
                    members
                        .iter()
                        .map(|member| formatdoc!(
                            "

                                write!(f, \" {member}: \")?;
                                core::fmt::Debug::fmt(self.{member}, ref f)?;",
                            member = member.name
                        ))
                        .join("\nwrite!(f, \",\")?;"),
                )
            }
        },
    );

    Some(formatdoc! {"
        {header} {{
            fn fmt(self: @{full_typename}, ref f: core::fmt::Formatter) -> core::result::Result::<(), core::fmt::Error> {{
                {body}
            }}
        }}
    "})
}
