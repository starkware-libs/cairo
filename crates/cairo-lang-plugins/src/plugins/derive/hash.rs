use indent::indent_by;
use indoc::formatdoc;
use itertools::{Itertools, chain};

use super::PluginTypeInfo;
use crate::plugins::utils::TypeVariant;

/// Adds derive result for the `Hash` trait.
pub fn handle_hash(info: &PluginTypeInfo<'_>) -> String {
    const HASH_TRAIT: &str = "core::hash::Hash";
    const DROP_TRAIT: &str = "core::traits::Drop";
    let full_typename = info.full_typename();
    let ty = &info.name;
    match &info.type_variant {
        TypeVariant::Enum => {
            let impl_additional_generics = info
                .impl_generics(&[HASH_TRAIT], |trt, ty| {
                    format!("{trt}<{ty}, __State, __SHashState>")
                })
                .join(", ");
            let extra_comma = if impl_additional_generics.is_empty() { "" } else { ",\n    " };
            formatdoc! {"
                impl {ty}Hash<
                    __State,
                    impl __SHashState: core::hash::HashStateTrait<__State>,
                    +{DROP_TRAIT}<__State>{extra_comma}{impl_additional_generics}
                > of {HASH_TRAIT}<{full_typename}, __State, __SHashState> {{
                    #[inline(always)]
                    fn update_state(state: __State, value: {full_typename}) -> __State {{
                        match value {{
                            {}
                        }}
                    }}
                }}
                ",
                indent_by(12,
                info.members_info.iter().enumerate().map(|(idx, variant)| formatdoc!{"
                            {ty}::{variant}(x) => {{
                                let state = {HASH_TRAIT}::update_state(state, {idx}_felt252);
                                {imp}::update_state(state, x)
                            }},",
                        variant=variant.name,
                        imp = variant.impl_name(HASH_TRAIT),
                    }
                ).join("\n"))
            }
        }
        TypeVariant::Struct => {
            let impl_additional_generics =
                chain!(info.impl_generics(&[HASH_TRAIT, DROP_TRAIT], |trt, ty| {
                    if trt == HASH_TRAIT {
                        format!("{HASH_TRAIT}<{ty}, __State, __SHashState>")
                    } else {
                        format!("{trt}<{ty}>")
                    }
                }))
                .join(", ");
            let extra_comma = if impl_additional_generics.is_empty() { "" } else { ",\n    " };
            formatdoc! {"
                impl {ty}Hash<
                    __State,
                    impl __SHashState: core::hash::HashStateTrait<__State>,
                    +{DROP_TRAIT}<__State>{extra_comma}{impl_additional_generics}
                > of {HASH_TRAIT}<{full_typename}, __State, __SHashState> {{
                    #[inline(always)]
                    fn update_state(state: __State, value: {full_typename}) -> __State {{
                        let __hash_derive_state = state;
                        {}
                        __hash_derive_state
                    }}
                }}
                ",
                chain!(
                    info.members_info.iter().map(|member| {
                        format!(
                            "let {member} = {drop_with} {{ value: value.{member} }};",
                            member = member.name,
                            drop_with = member.drop_with(),
                        )
                    }),
                    info.members_info.iter().map(|member| {
                        format!(
                            "let __hash_derive_state = {imp}::update_state(__hash_derive_state, {}.value);",
                            member.name,
                            imp = member.impl_name(HASH_TRAIT),
                        )
                    })
                ).join("\n        ")
            }
        }
    }
}
