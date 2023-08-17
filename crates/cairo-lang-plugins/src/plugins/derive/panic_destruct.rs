use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::{unsupported_for_extern_diagnostic, DeriveInfo, DeriveResult};
use crate::plugins::derive::TypeVariantInfo;

/// Adds drive result for the `PanicDestruct` trait.
pub fn handle_panic_destruct(
    info: &DeriveInfo,
    stable_ptr: SyntaxStablePtrId,
    result: &mut DeriveResult,
) {
    let header = info.format_impl_header("PanicDestruct", &["PanicDestruct"]);
    let full_typename = info.full_typename();
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
                        "{ty}::{variant}(x) => \
                        traits::PanicDestruct::panic_destruct(x, ref panic),",
                        ty=info.name,
                        variant=variant.name,
                    )
                }).join("\n    ")}
            }
            TypeVariantInfo::Struct(members) => members
                .iter()
                .map(|member| {
                    format!(
                        "traits::PanicDestruct::panic_destruct(self.{member}, ref panic);",
                        member = member.name,
                    )
                })
                .join("\n"),
            TypeVariantInfo::Extern => {
                result.diagnostics.push(unsupported_for_extern_diagnostic(stable_ptr));
                return;
            }
        },
    );
    result.impls.push(formatdoc! {"
        {header} {{
            fn panic_destruct(self: {full_typename}, ref panic: Panic) nopanic {{
                {body}
            }}
        }}
    "});
}
