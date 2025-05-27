use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::PluginTypeInfo;
use crate::plugins::utils::TypeVariant;

/// Adds derive result for the `PanicDestruct` trait.
pub fn handle_panic_destruct(info: &PluginTypeInfo<'_>) -> String {
    const PANIC_DESTRUCT_TRAIT: &str = "core::traits::PanicDestruct";
    let header = info.impl_header(PANIC_DESTRUCT_TRAIT, &[PANIC_DESTRUCT_TRAIT]);
    let full_typename = info.full_typename();
    let ty = &info.name;
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
                        "{ty}::{}(x) => \
                        {imp}::panic_destruct(x, ref panic),",
                        variant.name,
                        imp=variant.impl_name(PANIC_DESTRUCT_TRAIT),
                    )
                }).join("\n    ")}
            }
            TypeVariant::Struct => {
                format!(
                    "let {ty} {{ {} }} = self;{}",
                    info.members_info.iter().map(|member| &member.name).join(", "),
                    info.members_info
                        .iter()
                        .map(|member| format!(
                            "\n{imp}::panic_destruct({}, ref panic);",
                            member.name,
                            imp = member.impl_name(PANIC_DESTRUCT_TRAIT),
                        ))
                        .join(""),
                )
            }
        },
    );
    formatdoc! {"
        {header} {{
            fn panic_destruct(self: {full_typename}, ref panic: Panic) nopanic {{
                {body}
            }}
        }}
    "}
}
