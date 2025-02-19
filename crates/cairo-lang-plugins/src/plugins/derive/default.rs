use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::DeriveInfo;
use crate::plugins::derive::TypeVariantInfo;

pub const DEFAULT_ATTR: &str = "default";

/// Adds derive result for the `Default` trait.
pub fn handle_default(
    db: &dyn SyntaxGroup,
    info: &DeriveInfo,
    derived: &ast::ExprPath,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<String> {
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
                    diagnostics.push(PluginDiagnostic::error(
                        derived,
                        "derive `Default` for enum only supported with a default variant.".into(),
                    ));
                    return None;
                };
                for (_, extra_default_attr) in default_variants {
                    diagnostics.push(PluginDiagnostic::error(
                        &extra_default_attr,
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
        },
    );
    Some(formatdoc! {"
        {header} {{
            fn default() -> {full_typename} {{
                {body}
            }}
        }}
    "})
}
