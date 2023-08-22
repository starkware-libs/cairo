use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::{unsupported_for_extern_diagnostic, DeriveInfo, DeriveResult};
use crate::plugins::derive::TypeVariantInfo;

/// Adds drive result for the `Destruct` trait.
pub fn handle_destruct(
    info: &DeriveInfo,
    stable_ptr: SyntaxStablePtrId,
    result: &mut DeriveResult,
) {
    let full_typename = info.full_typename();
    let ty = &info.name;
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
                            "{ty}::{}(x) => traits::Destruct::destruct(x),",
                            variant.name,
                        )
                    }).join("\n    ")
                }
            }
            TypeVariantInfo::Struct(members) => {
                format!(
                    "let {ty} {{ {} }} = self;{}",
                    members.iter().map(|member| &member.name).join(", "),
                    members
                        .iter()
                        .map(|member| format!("\ntraits::Destruct::destruct({});", member.name))
                        .join(""),
                )
            }
            TypeVariantInfo::Extern => {
                result.diagnostics.push(unsupported_for_extern_diagnostic(stable_ptr));
                return;
            }
        },
    );
    result.impls.push(formatdoc! {"
        {header} {{
            fn destruct(self: {full_typename}) nopanic {{
                {body}
            }}
        }}
    "});
}
