use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::PluginTypeInfo;
use crate::plugins::utils::TypeVariant;

/// Adds derive result for the `PartialEq` trait.
pub fn handle_partial_eq(info: &PluginTypeInfo<'_>) -> String {
    const PARTIAL_EQ_TRAIT: &str = "core::traits::PartialEq";
    let header = info.impl_header(PARTIAL_EQ_TRAIT, &[PARTIAL_EQ_TRAIT]);
    let full_typename = info.full_typename();
    let body = indent_by(
        8,
        match &info.type_variant {
            TypeVariant::Enum => {
                let ty = &info.name;
                formatdoc! {"
                        match lhs {{
                            {}
                        }}",
                    info.members_info.iter().map(|lhs_variant| {
                    indent_by(4, formatdoc! {"
                        {ty}::{lhs_variant}(x) => match rhs {{
                            {}
                        }},",
                        info.members_info.iter().map(|rhs_variant|{
                            if lhs_variant.name == rhs_variant.name {
                                format!("{ty}::{}(y) => {}::eq(x, y),", rhs_variant.name,
                                rhs_variant.impl_name(PARTIAL_EQ_TRAIT))
                            } else {
                                format!("{ty}::{}(_y) => false,", rhs_variant.name)
                            }
                        }).join("\n    "),
                    lhs_variant=lhs_variant.name,
                    })
                }).join("\n    ")}
            }
            TypeVariant::Struct => {
                if info.members_info.is_empty() {
                    "true".to_string()
                } else {
                    info.members_info
                        .iter()
                        .map(|member| {
                            format!(
                                "{imp}::eq(lhs.{member}, rhs.{member})",
                                member = member.name,
                                imp = member.impl_name(PARTIAL_EQ_TRAIT),
                            )
                        })
                        .join(" && ")
                }
            }
        },
    );
    formatdoc! {"
        {header} {{
            fn eq(lhs: @{full_typename}, rhs: @{full_typename}) -> bool {{
                {body}
            }}
        }}
    "}
}
