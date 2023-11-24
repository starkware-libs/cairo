use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::TypedSyntaxNode;
use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::{unsupported_for_extern_diagnostic, DeriveInfo, DeriveResult};
use crate::plugins::derive::TypeVariantInfo;

pub const DEFAULT_ATTR: &str = "default";

/// Adds derive result for the `Default` trait.
pub fn handle_default(
    db: &dyn SyntaxGroup,
    info: &DeriveInfo,
    stable_ptr: SyntaxStablePtrId,
    result: &mut DeriveResult,
) {
    let header = info.format_impl_header(
        "core::traits",
        "Default",
        &["core::traits::Default", "core::traits::Destruct"],
    );
    let full_typename = info.full_typename();
    let ty = &info.name;
    let body = indent_by(
        8,
        match &info.specific_info {
            TypeVariantInfo::Enum(variants) => {
                let mut default_variants = variants.iter().filter_map(|variant| {
                    Some((variant, variant.attributes.find_attr(db, DEFAULT_ATTR)?))
                });
                let Some((default_variant, _)) = default_variants.next() else {
                    result.diagnostics.push(PluginDiagnostic::error(
                        stable_ptr,
                        "derive `Default` for enum only supported with a default variant.".into(),
                    ));
                    return;
                };
                for (_, extra_default_attr) in default_variants {
                    result.diagnostics.push(PluginDiagnostic::error(
                        extra_default_attr.as_syntax_node().stable_ptr(),
                        "Multiple variants annotated with `#[default]`".into(),
                    ));
                }
                let default_variant = &default_variant.name;
                formatdoc!("{ty}::{default_variant}(core::traits::Default::default())")
            }
            TypeVariantInfo::Struct(members) => {
                formatdoc! {"
                    {ty} {{
                        {}
                    }}",
                    indent_by(4, members.iter().map(|member| {
                        format!(
                            "{member}: core::traits::Default::default(),",
                            member=member.name,
                        )
                    }).join("\n"))
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
            fn default() -> {full_typename} {{
                {body}
            }}
        }}
    "});
}
