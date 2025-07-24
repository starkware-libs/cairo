use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::PluginTypeInfo;
use crate::plugins::utils::TypeVariant;

/// Adds derive result for the `Serde` trait.
pub fn handle_serde(info: &PluginTypeInfo<'_>) -> String {
    const SERDE_TRAIT: &str = "core::serde::Serde";
    const DESTRUCT_TRAIT: &str = "core::traits::Destruct";
    let ty = &info.name;
    let full_typename = info.full_typename();
    let header = match &info.type_variant {
        TypeVariant::Enum => {
            let impl_additional_generics =
                info.impl_generics(&[SERDE_TRAIT], |trt, ty| format!("{trt}<{ty}>")).join(", ");
            formatdoc! {"
            impl {ty}Serde<
                    {impl_additional_generics}
                > of {SERDE_TRAIT}<{full_typename}>
            "}
        }
        TypeVariant::Struct => info.impl_header(SERDE_TRAIT, &[SERDE_TRAIT, DESTRUCT_TRAIT]),
    };
    let serialize_body = indent_by(
        8,
        match &info.type_variant {
            TypeVariant::Enum => {
                formatdoc! {"
                match self {{
                    {}
                }}",
                info.members_info.iter().enumerate().map(|(idx, variant)| {
                    format!(
                        "{ty}::{variant}(x) => {{ {SERDE_TRAIT}::<felt252>::serialize(@{idx}, ref output); \
                        {imp}::serialize(x, ref output); }},",
                        variant=variant.name,
                        imp=variant.impl_name(SERDE_TRAIT),
                    )
                }).join("\n    ")}
            }
            TypeVariant::Struct => info
                .members_info
                .iter()
                .map(|member| {
                    format!(
                        "{imp}::serialize(self.{member}, ref output)",
                        member = member.name,
                        imp = member.impl_name(SERDE_TRAIT),
                    )
                })
                .join(";\n"),
        },
    );
    let deserialize_body = indent_by(
        8,
        match &info.type_variant {
            TypeVariant::Enum => {
                formatdoc! {"
                    let idx: felt252 = {SERDE_TRAIT}::<felt252>::deserialize(ref serialized)?;
                    core::option::Option::Some(
                        match idx {{
                            {}
                            _ => {{ return core::option::Option::None; }}
                        }}
                    )",
                    info.members_info.iter().enumerate().map(|(idx, variant)| {
                        format!(
                            "{idx} => {ty}::{variant}(\
                                {imp}::deserialize(ref serialized)?),",
                            variant=variant.name,
                            imp=variant.impl_name(SERDE_TRAIT),
                        )
                    }).join("\n        ")
                }
            }
            TypeVariant::Struct => {
                formatdoc! {"
                    {}
                    core::option::Option::Some({ty} {{
                        {}
                    }})",
                    info.members_info.iter().map(|member|format!(
                        "let {member} = {destruct_with} {{ value: {imp}::deserialize(ref serialized)? }};",
                        member=member.name,
                        destruct_with=member.destruct_with(),
                        imp=member.impl_name(SERDE_TRAIT),
                    )).join("\n"),
                    info.members_info.iter().map(|member|format!(
                        "{member}: {member}.value,", member=member.name
                    )).join("\n    "),
                }
            }
        },
    );
    formatdoc! {"
        {header} {{
            fn serialize(self: @{full_typename}, ref output: core::array::Array<felt252>) {{
                {serialize_body}
            }}
            fn deserialize(ref serialized: core::array::Span<felt252>) -> core::option::Option<{full_typename}> {{
                {deserialize_body}
            }}
        }}
    "}
}
