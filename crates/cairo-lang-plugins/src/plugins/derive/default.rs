use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};
use indoc::formatdoc;
use itertools::{Itertools, chain};
use salsa::Database;

use super::PluginTypeInfo;
use crate::plugins::utils::TypeVariant;

pub const DEFAULT_ATTR: &str = "default";

/// Adds derive result for the `Default` trait.
pub fn handle_default<'db>(
    db: &'db dyn Database,
    info: &PluginTypeInfo<'db>,
    derived: &ast::ExprPath<'db>,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) -> Option<String> {
    const DEFAULT_TRAIT: &str = "core::traits::Default";
    const DESTRUCT_TRAIT: &str = "core::traits::Destruct";
    let full_typename = info.full_typename();
    let ty = &info.name;
    match &info.type_variant {
        TypeVariant::Enum => {
            let mut default_variants = info.members_info.iter().filter_map(|variant| {
                Some((variant, variant.attributes.find_attr(db, DEFAULT_ATTR)?))
            });
            let Some((default_variant, _)) = default_variants.next() else {
                diagnostics.push(PluginDiagnostic::error(
                    derived.stable_ptr(db),
                    "derive `Default` for enum only supported with a default variant.".into(),
                ));
                return None;
            };
            for (_, extra_default_attr) in default_variants {
                diagnostics.push(PluginDiagnostic::error(
                    extra_default_attr.stable_ptr(db),
                    "Multiple variants annotated with `#[default]`".into(),
                ));
            }
            let default_variant_name = &default_variant.name;
            let imp = default_variant.impl_name(DEFAULT_TRAIT);

            let header = format!(
                "impl {ty}Default<{generics}> of {DEFAULT_TRAIT}::<{full_typename}>",
                generics = chain!(
                    info.generics.full_params.iter().map(ToString::to_string),
                    default_variant
                        .is_generics_dependent
                        .then(|| format!("impl {imp}: {DEFAULT_TRAIT}<{}>", default_variant.ty))
                )
                .join(", "),
            );

            Some(formatdoc! {"
                {header} {{
                    fn default() -> {full_typename} {{
                        {ty}::{default_variant_name}({imp}::default())
                    }}
                }}
                "
            })
        }
        TypeVariant::Struct => {
            let header = info.impl_header(DEFAULT_TRAIT, &[DEFAULT_TRAIT, DESTRUCT_TRAIT]);
            Some(formatdoc! {"
                {header} {{
                    fn default() -> {full_typename} {{
                        {}
                        {ty} {{
                            {}
                        }}
                    }}
                }}
                ",
                info.members_info.iter().map(|member| {
                    format!(
                        "let {member} = {destruct_with} {{ value: {imp}::default() }};",
                        member=member.name,
                        destruct_with=member.destruct_with(),
                        imp=member.impl_name(DEFAULT_TRAIT),
                    )
                }).join("\n        "),
                info.members_info.iter().map(|member| {
                    format!("{member}: {member}.value,", member=member.name)
                }).join("\n            ")
            })
        }
    }
}
