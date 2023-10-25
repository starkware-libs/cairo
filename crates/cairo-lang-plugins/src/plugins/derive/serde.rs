use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::{unsupported_for_extern_diagnostic, DeriveInfo, DeriveResult};
use crate::plugins::derive::TypeVariantInfo;

/// Adds derive result for the `Serde` trait.
pub fn handle_serde(info: &DeriveInfo, stable_ptr: SyntaxStablePtrId, result: &mut DeriveResult) {
    let header = info.format_impl_header("Serde", &["Serde", "Destruct"]);
    let full_typename = info.full_typename();
    let ty = &info.name;
    let serialize_body = indent_by(
        8,
        match &info.specific_info {
            TypeVariantInfo::Enum(variants) => {
                formatdoc! {"
                match self {{
                    {}
                }}",
                    variants.iter().enumerate().map(|(idx, variant)| {
                    format!(
                        "{ty}::{variant}(x) => {{ Serde::serialize(@{idx}, ref output); \
                        Serde::serialize(x, ref output); }},",
                        variant=variant.name,
                    )
                }).join("\n    ")}
            }
            TypeVariantInfo::Struct(members) => members
                .iter()
                .map(|member| {
                    format!("Serde::serialize(self.{member}, ref output)", member = member.name)
                })
                .join(";\n"),
            TypeVariantInfo::Extern => {
                result.diagnostics.push(unsupported_for_extern_diagnostic(stable_ptr));
                return;
            }
        },
    );
    let deserialize_body = indent_by(
        8,
        match &info.specific_info {
            TypeVariantInfo::Enum(variants) => {
                formatdoc! {"
                    let idx: felt252 = Serde::deserialize(ref serialized)?;
                    Option::Some(
                        {}
                        else {{ return Option::None; }}
                    )",
                    variants.iter().enumerate().map(|(idx, variant)| {
                        format!(
                            "if idx == {idx} {{ {ty}::{variant}(\
                                Serde::deserialize(ref serialized)?) }}",
                            variant=variant.name,
                        )
                    }).join("\n    else ")
                }
            }
            TypeVariantInfo::Struct(members) => {
                formatdoc! {"
                    Option::Some({ty} {{
                        {}
                    }})",
                    members.iter().map(|member|format!(
                        "{member}: Serde::deserialize(ref serialized)?,",
                        member=member.name
                    )).join("\n    "),
                }
            }
            TypeVariantInfo::Extern => {
                result.diagnostics.push(unsupported_for_extern_diagnostic(stable_ptr));
                return;
            }
        },
    );
    result.impls.push(formatdoc! {"
        {header} {{
            fn serialize(self: @{full_typename}, ref output: Array<felt252>) {{
                {serialize_body}
            }}
            fn deserialize(ref serialized: Span<felt252>) -> Option<{full_typename}> {{
                {deserialize_body}
            }}
        }}
    "});
}
