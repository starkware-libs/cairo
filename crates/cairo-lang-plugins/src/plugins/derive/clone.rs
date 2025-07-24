use indoc::formatdoc;
use itertools::Itertools;

use super::PluginTypeInfo;
use crate::plugins::utils::TypeVariant;

/// Adds derive result for the `Clone` trait.
pub fn handle_clone(info: &PluginTypeInfo<'_>) -> String {
    const CLONE_TRAIT: &str = "core::clone::Clone";
    const DESTRUCT_TRAIT: &str = "core::traits::Destruct";
    let full_typename = info.full_typename();
    let name = &info.name;
    match &info.type_variant {
        TypeVariant::Enum => {
            let header = info.impl_header(CLONE_TRAIT, &[CLONE_TRAIT]);
            formatdoc! {"
                {header} {{
                    fn clone(self: @{full_typename}) -> {full_typename} {{
                        match self {{
                            {}
                        }}
                    }}
                }}
                ",
                info.members_info.iter().map(|variant|
                    format!(
                        "{ty}::{variant}(x) => {ty}::{variant}({imp}::clone(x)),",
                        ty=info.name,
                        variant=variant.name,
                        imp=variant.impl_name(CLONE_TRAIT),
                    )).join("\n    "),
            }
        }
        TypeVariant::Struct => {
            let header = info.impl_header(CLONE_TRAIT, &[CLONE_TRAIT, DESTRUCT_TRAIT]);
            formatdoc! {"
                {header} {{
                    fn clone(self: @{full_typename}) -> {full_typename} {{
                        {}
                        {name} {{
                            {}
                        }}
                    }}
                }}
                ",
                info.members_info.iter().map(|member| {
                    format!(
                        "let {member} = {destruct_with} {{ value: {imp}::clone(self.{member}) }};",
                        member=member.name,
                        destruct_with=member.destruct_with(),
                        imp=member.impl_name(CLONE_TRAIT),
                    )
                }).join("\n        "),
                info.members_info.iter().map(|member| {
                    format!("{member}: {member}.value,", member=member.name)
                }).join("\n            "),
            }
        }
    }
}
