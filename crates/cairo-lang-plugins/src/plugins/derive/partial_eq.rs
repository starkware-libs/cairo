use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::DeriveInfo;
use crate::plugins::derive::TypeVariantInfo;

/// Returns the derived implementation of the `PartialEq` trait.
pub fn get_partial_eq_impl(info: &DeriveInfo) -> String {
    let header = info.format_impl_header("PartialEq", &["PartialEq"]);
    let full_typename = info.full_typename();
    let body = indent_by(
        8,
        match &info.specific_info {
            TypeVariantInfo::Enum(variants) => {
                let ty = &info.name;
                formatdoc! {"
                        match lhs {{
                            {}
                        }}",
                variants.iter().map(|lhs_variant| {
                    indent_by(4, formatdoc! {"
                        {ty}::{lhs_variant}(x) => match rhs {{
                            {}
                        }},",
                        variants.iter().map(|rhs_variant|{
                            if lhs_variant.name == rhs_variant.name {
                                format!("{ty}::{}(y) => x == y,", rhs_variant.name)
                            } else {
                                format!("{ty}::{}(y) => false,", rhs_variant.name)
                            }
                        }).join("\n    "),
                    lhs_variant=lhs_variant.name,
                    })
                }).join("\n    ")}
            }
            TypeVariantInfo::Struct(members) => {
                if members.is_empty() {
                    "true".to_string()
                } else {
                    members
                        .iter()
                        .map(|member| format!("lhs.{member} == rhs.{member}", member = member.name))
                        .join(" && ")
                }
            }
            TypeVariantInfo::Extern => unreachable!(),
        },
    );
    formatdoc! {"
        {header} {{
            fn eq(lhs: @{full_typename}, rhs: @{full_typename}) -> bool {{
                {body}
            }}
            #[inline(always)]
            fn ne(lhs: @{full_typename}, rhs: @{full_typename}) -> bool {{
                !(lhs == rhs)
            }}
        }}
    "}
}
