use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::PluginTypeInfo;
use crate::plugins::utils::TypeVariant;

/// Adds derive result for the `Destruct` trait.
pub fn handle_destruct(info: &PluginTypeInfo<'_>) -> String {
    const DESTRUCT_TRAIT: &str = "core::traits::Destruct";
    let full_typename = info.full_typename();
    let ty = &info.name;
    let header = info.impl_header(DESTRUCT_TRAIT, &[DESTRUCT_TRAIT]);
    let body = indent_by(
        8,
        match &info.type_variant {
            TypeVariant::Enum => {
                formatdoc! {"
                    match self {{
                        {}
                    }}",
                    info.members_info.iter().map(|variant| {
                        format!(
                            "{ty}::{}(x) => {imp}::destruct(x),",
                            variant.name,
                            imp=variant.impl_name(DESTRUCT_TRAIT),
                        )
                    }).join("\n    ")
                }
            }
            TypeVariant::Struct => {
                format!(
                    "let {ty} {{ {} }} = self;{}",
                    info.members_info.iter().map(|member| &member.name).join(", "),
                    info.members_info
                        .iter()
                        .map(|member| format!(
                            "\n{imp}::destruct({});",
                            member.name,
                            imp = member.impl_name(DESTRUCT_TRAIT),
                        ))
                        .join(""),
                )
            }
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
