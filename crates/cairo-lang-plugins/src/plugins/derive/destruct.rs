use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::DeriveInfo;
use crate::plugins::derive::TypeVariantInfo;

/// Returns the derived implementation of the `Destruct` trait.
pub fn get_destruct_impl(info: &DeriveInfo) -> String {
    let full_typename = info.full_typename();
    let header = info.format_impl_header("Destruct", &["Destruct"]);
    let body = indent_by(
        8,
        match &info.specific_info {
            TypeVariantInfo::Enum(variants) => {
                formatdoc! {"
                match self {{
                    {}
                }}",
                    variants.iter().map(|variant| {
                    format!(
                        "{ty}::{variant}(x) => traits::Destruct::destruct(x),",
                        ty=info.name,
                        variant=variant.name,
                    )
                }).join("\n    ")}
            }
            TypeVariantInfo::Struct(members) => members
                .iter()
                .map(|member| {
                    format!("traits::Destruct::destruct(self.{member});", member = member.name,)
                })
                .join("\n"),
            TypeVariantInfo::Extern => unreachable!(),
        },
    );
    formatdoc! {"
        {header} {{
            fn destruct(self: {full_typename}) nopanic {{
                {body}
            }}
        }}
    "}
}
