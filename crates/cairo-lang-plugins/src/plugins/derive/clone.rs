use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::DeriveInfo;
use crate::plugins::derive::TypeVariantInfo;

/// Returns the derived implementation of the `Clone` trait.
pub fn get_clone_impl(info: &DeriveInfo) -> String {
    let header = info.format_impl_header("Clone", &["Clone", "Destruct"]);
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
                variants.iter().map(|variant|
                    format!(
                        "{ty}::{variant}(x) => {ty}::{variant}(Clone::clone(x)),",
                        ty=info.name,
                        variant=variant.name,
                    )).join("\n    ")}
            }
            TypeVariantInfo::Struct(members) => {
                formatdoc! {"
                {name} {{
                    {}
                }}",
                    indent_by(4, members.iter().map(|member| {
                        format!(
                            "{member}: Clone::clone(self.{member}),",
                            member=member.name,
                        )
                    }).join("\n"))
                }
            }
            TypeVariantInfo::Extern => unreachable!(),
        },
    );
    formatdoc! {"
        {header} {{
            fn clone(self: @{full_typename}) -> {full_typename} {{
                {body}
            }}
        }}
    "}
}
