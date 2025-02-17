use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::ast;
use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::{DeriveInfo, unsupported_for_extern_diagnostic};
use crate::plugins::derive::TypeVariantInfo;

/// Adds derive result for the `Clone` trait.
pub fn handle_clone(
    info: &DeriveInfo,
    derived: &ast::ExprPath,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<String> {
    let header =
        info.format_impl_header("core::clone", "Clone", &["core::clone::Clone", "Destruct"]);
    let full_typename = info.full_typename();
    let name = &info.name;
    let body = indent_by(
        8,
        match &info.specific_info {
            TypeVariantInfo::Enum(variants) => {
                formatdoc! {"
                match self {{
                    {}
                }}",
                variants.iter().map(|variant|
                    format!(
                        "{ty}::{variant}(x) => {ty}::{variant}(core::clone::Clone::clone(x)),",
                        ty=info.name,
                        variant=variant.name,
                    )).join("\n    ")}
            }
            TypeVariantInfo::Struct(members) => {
                formatdoc! {"
                {name} {{
                    {}
                }}",
                    indent_by(4, members.iter().map(|member| {
                        format!(
                            "{member}: core::clone::Clone::clone(self.{member}),",
                            member=member.name,
                        )
                    }).join("\n"))
                }
            }
            TypeVariantInfo::Extern => {
                diagnostics.push(unsupported_for_extern_diagnostic(derived));
                return None;
            }
        },
    );
    Some(formatdoc! {"
        {header} {{
            fn clone(self: @{full_typename}) -> {full_typename} {{
                {body}
            }}
        }}
    "})
}
