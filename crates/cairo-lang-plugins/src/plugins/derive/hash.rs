use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use indent::indent_by;
use indoc::formatdoc;
use itertools::Itertools;

use super::{unsupported_for_extern_diagnostic, DeriveInfo, DeriveResult};
use crate::plugins::derive::TypeVariantInfo;

/// Adds derive result for the `Serde` trait.
pub fn handle_hash(info: &DeriveInfo, stable_ptr: SyntaxStablePtrId, result: &mut DeriveResult) {
    let full_typename = info.full_typename();
    let ty = &info.name;
    let body = indent_by(
        8,
        match &info.specific_info {
            TypeVariantInfo::Enum(variants) => {
                formatdoc! {"
                    match value {{
                        {}
                    }}",
                    indent_by(4,
                    variants.iter().enumerate().map(|(idx, variant)| formatdoc!{"
                            {ty}::{variant}(x) => {{
                                let state = hash::Hash::update_state(state, @{idx});
                                hash::Hash::update_state(state, x)
                            }},",
                            variant=variant.name,
                        }
                    ).join("\n"))
                }
            }
            TypeVariantInfo::Struct(members) => format!(
                "{}\nstate",
                members
                    .iter()
                    .map(|member| {
                        format!(
                            "let state = hash::Hash::update_state(state, value.{});",
                            member.name
                        )
                    })
                    .join("\n")
            ),
            TypeVariantInfo::Extern => {
                return result.diagnostics.push(unsupported_for_extern_diagnostic(stable_ptr));
            }
        },
    );
    let impl_additional_generics = info.generics.format_generics_with_trait_params_only(|t| {
        vec![format!("impl {t}Hash: Hash<{t}>"), format!("impl {t}Drop: Drop<{t}>")]
    });
    result.impls.push(formatdoc! {"
        impl {ty}Hash<
            __State,
            impl __SHashState: hash::HashStateTrait<__State>,
            impl SDrop: Drop<__State>{impl_additional_generics}
        > of hash::Hash<{full_typename}, __State, __SHashState> {{
            #[inline(always)]
            fn update_state(state: __State, value: {full_typename}) -> __State {{
                {body}
            }}
        }}
    "});
}
