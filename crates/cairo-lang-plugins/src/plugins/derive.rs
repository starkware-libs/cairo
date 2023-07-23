use std::sync::Arc;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin, TrivialPluginAuxData};
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{
    AttributeList, ItemStruct, MemberList, OptionWrappedGenericParamList, Variant,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use indoc::formatdoc;
use itertools::Itertools;
use smol_str::SmolStr;

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct DerivePlugin;

impl MacroPlugin for DerivePlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Struct(struct_ast) => generate_derive_code_for_type(
                db,
                struct_ast.name(db),
                struct_ast.attributes(db),
                extract_struct_extra_info(db, &struct_ast),
            ),
            ast::Item::Enum(enum_ast) => generate_derive_code_for_type(
                db,
                enum_ast.name(db),
                enum_ast.attributes(db),
                ExtraInfo::Enum(enum_ast.variants(db).elements(db).into_iter().collect()),
            ),
            ast::Item::ExternType(extern_type_ast) => generate_derive_code_for_type(
                db,
                extern_type_ast.name(db),
                extern_type_ast.attributes(db),
                ExtraInfo::Extern,
            ),
            _ => PluginResult::default(),
        }
    }
}
impl AsDynMacroPlugin for DerivePlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}
impl SemanticPlugin for DerivePlugin {}

enum ExtraInfo {
    Enum(Vec<Variant>),
    Struct { members: Vec<SmolStr>, type_generics: Vec<SmolStr>, other_generics: Vec<String> },
    Extern,
}

fn member_names(db: &dyn SyntaxGroup, members: MemberList) -> Vec<SmolStr> {
    members.elements(db).into_iter().map(|member| member.name(db).text(db)).collect()
}

fn extract_struct_extra_info(db: &dyn SyntaxGroup, struct_ast: &ItemStruct) -> ExtraInfo {
    let members = member_names(db, struct_ast.members(db));
    let mut type_generics = vec![];
    let mut other_generics = vec![];
    match struct_ast.generic_params(db) {
        OptionWrappedGenericParamList::WrappedGenericParamList(gens) => gens
            .generic_params(db)
            .elements(db)
            .into_iter()
            .map(|member| match member {
                ast::GenericParam::Type(t) => {
                    type_generics.push(t.name(db).text(db));
                }
                ast::GenericParam::Impl(i) => {
                    other_generics.push(i.as_syntax_node().get_text_without_trivia(db))
                }
                ast::GenericParam::Const(c) => {
                    other_generics.push(c.as_syntax_node().get_text_without_trivia(db))
                }
            })
            .collect(),
        OptionWrappedGenericParamList::Empty(_) => vec![],
    };
    ExtraInfo::Struct { members, type_generics, other_generics }
}

fn format_generics_with_trait(
    type_generics: &[SmolStr],
    other_generics: &[String],
    f: impl Fn(&SmolStr) -> String,
) -> String {
    format!(
        "<{}{}{}>",
        type_generics.iter().map(|s| format!("{}, ", s)).collect::<String>(),
        other_generics.iter().map(|s| format!("{}, ", s)).collect::<String>(),
        type_generics.iter().map(f).join(", "),
    )
}

fn format_generics(type_generics: &[SmolStr], other_generics: &[String]) -> String {
    format!(
        "<{}{}>",
        type_generics.iter().map(|s| format!("{}, ", s)).collect::<String>(),
        other_generics.iter().map(|s| format!("{}, ", s)).collect::<String>(),
    )
}

/// Adds an implementation for all requested derives for the type.
fn generate_derive_code_for_type(
    db: &dyn SyntaxGroup,
    ident: ast::TerminalIdentifier,
    attributes: AttributeList,
    extra_info: ExtraInfo,
) -> PluginResult {
    let mut diagnostics = vec![];
    let mut impls = vec![];
    for attr in attributes.query_attr(db, "derive") {
        let attr = attr.structurize(db);

        if attr.args.is_empty() {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: attr.args_stable_ptr.untyped(),
                message: "Expected args.".into(),
            });
            continue;
        }

        for arg in attr.args {
            let AttributeArg {
                variant:
                    AttributeArgVariant::Unnamed {
                        value: ast::Expr::Path(path), value_stable_ptr, ..
                    },
                ..
            } = arg
            else {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: arg.arg_stable_ptr.untyped(),
                    message: "Expected path.".into(),
                });
                continue;
            };

            let [ast::PathSegment::Simple(segment)] = &path.elements(db)[..] else {
                continue;
            };

            let name = ident.text(db);
            let derived = segment.ident(db).text(db);
            match derived.as_str() {
                "Copy" | "Drop" => impls.push(get_empty_impl(&name, &derived, &extra_info)),
                "Clone" if !matches!(extra_info, ExtraInfo::Extern) => {
                    impls.push(get_clone_impl(&name, &extra_info, db))
                }
                "Destruct" if !matches!(extra_info, ExtraInfo::Extern) => {
                    impls.push(get_destruct_impl(&name, &extra_info, db))
                }
                "PanicDestruct" if !matches!(extra_info, ExtraInfo::Extern) => {
                    impls.push(get_panic_destruct_impl(&name, &extra_info, db))
                }
                "PartialEq" if !matches!(extra_info, ExtraInfo::Extern) => {
                    impls.push(get_partial_eq_impl(&name, &extra_info, db))
                }
                "Serde" if !matches!(extra_info, ExtraInfo::Extern) => {
                    impls.push(get_serde_impl(&name, &extra_info, db))
                }
                "Default" if !matches!(extra_info, ExtraInfo::Extern) => {
                    generate_default_impl(
                        &name,
                        &extra_info,
                        value_stable_ptr.untyped(),
                        &mut diagnostics,
                        &mut impls,
                        db,
                    );
                }
                "Clone" | "Destruct" | "PartialEq" | "Serde" => {
                    diagnostics.push(PluginDiagnostic {
                        stable_ptr: value_stable_ptr.untyped(),
                        message: "Unsupported trait for derive for extern types.".into(),
                    })
                }
                _ => {
                    // TODO(spapini): How to allow downstream derives while also
                    //  alerting the user when the derive doesn't exist?
                }
            }
        }
    }
    PluginResult {
        code: if impls.is_empty() {
            None
        } else {
            Some(PluginGeneratedFile {
                name: "impls".into(),
                content: impls.join(""),
                aux_data: DynGeneratedFileAuxData(Arc::new(TrivialPluginAuxData {})),
            })
        },
        diagnostics,
        remove_original_item: false,
    }
}

fn get_clone_impl(name: &str, extra_info: &ExtraInfo, db: &dyn SyntaxGroup) -> String {
    match extra_info {
        ExtraInfo::Enum(variants) => {
            formatdoc! {"
                    impl {name}Clone of Clone::<{name}> {{
                        fn clone(self: @{name}) -> {name} {{
                            match self {{
                                {}
                            }}
                        }}
                    }}
                ", variants.iter().map(|variant| {
                format!("{name}::{variant_name}(x) => {name}::{variant_name}(x.clone()),", variant_name = variant.name(db).text(db))
            }).join("\n            ")}
        }
        ExtraInfo::Struct { members, type_generics, other_generics } => {
            formatdoc! {"
                    impl {name}Clone{generics_impl} of Clone::<{name}{generics}> {{
                        fn clone(self: @{name}{generics}) -> {name}{generics} {{
                            {name} {{
                                {}
                            }}
                        }}
                    }}
                ", members.iter().map(|member| {
                    format!("{member}: self.{member}.clone(),")
                }).join("\n            "),
                generics = format_generics(type_generics, other_generics),
                generics_impl = format_generics_with_trait(type_generics, other_generics,
                    |t| format!("impl {t}Clone: Clone<{t}>, impl {t}Destruct: Destruct<{t}>"))
            }
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn get_destruct_impl(name: &str, extra_info: &ExtraInfo, db: &dyn SyntaxGroup) -> String {
    match extra_info {
        ExtraInfo::Enum(variants) => {
            formatdoc! {"
                    impl {name}Destruct of Destruct::<{name}> {{
                        fn destruct(self: {name}) nopanic {{
                            match self {{
                                {}
                            }}
                        }}
                    }}
                ", variants.iter().map(|variant| {
                format!("{name}::{}(x) => traits::Destruct::destruct(x),", variant.name(db).text(db))
            }).join("\n            ")}
        }
        ExtraInfo::Struct { members, type_generics, other_generics } => {
            formatdoc! {"
                    impl {name}Destruct{generics_impl} of Destruct::<{name}{generics}> {{
                        fn destruct(self: {name}{generics}) nopanic {{
                            {}
                        }}
                    }}
                ", members.iter().map(|member| {
                    format!("traits::Destruct::destruct(self.{member});")
                }).join("\n        "),
                generics = format_generics(type_generics, other_generics),
                generics_impl = format_generics_with_trait(type_generics, other_generics,
                    |t| format!("impl {t}Destruct: Destruct<{t}>"))
            }
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn get_panic_destruct_impl(name: &str, extra_info: &ExtraInfo, db: &dyn SyntaxGroup) -> String {
    match extra_info {
        ExtraInfo::Enum(variants) => {
            formatdoc! {"
                    impl {name}PanicDestruct of PanicDestruct::<{name}> {{
                        fn panic_destruct(self: {name}, ref panic: Panic) nopanic {{
                            match self {{
                                {}
                            }}
                        }}
                    }}
                ", variants.iter().map(|variant| {
                format!(
                    "{name}::{}(x) => traits::PanicDestruct::panic_destruct(x, ref panic),", variant.name(db).text(db)
                )
            }).join("\n            ")}
        }
        ExtraInfo::Struct { members, type_generics, other_generics } => {
            formatdoc! {"
                    impl {name}PanicDestruct{generics_impl} of PanicDestruct::<{name}{generics}> {{
                        fn panic_destruct(self: {name}{generics}, ref panic: Panic) nopanic {{
                            {}
                        }}
                    }}
                ", members.iter().map(|member| {
                    format!("traits::PanicDestruct::panic_destruct(self.{member}, ref panic);")
                }).join("\n        "),
                generics = format_generics(type_generics, other_generics),
                generics_impl = format_generics_with_trait(type_generics, other_generics,
                    |t| format!("impl {t}PanicDestruct: PanicDestruct<{t}>"))
            }
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn get_partial_eq_impl(name: &str, extra_info: &ExtraInfo, db: &dyn SyntaxGroup) -> String {
    match extra_info {
        ExtraInfo::Enum(variants) => {
            formatdoc! {"
                    impl {name}PartialEq of PartialEq::<{name}> {{
                        fn eq(lhs: @{name}, rhs: @{name}) -> bool {{
                            match lhs {{
                                {}
                            }}
                        }}
                        #[inline(always)]
                        fn ne(lhs: @{name}, rhs: @{name}) -> bool {{
                            !(lhs == rhs)
                        }}
                    }}
                ", variants.iter().map(|lhs_variant| {
                format!(
                    "{name}::{lhs_variant_name}(x) => match rhs {{\n                {match_body}\n            }},",
                    lhs_variant_name = lhs_variant.name(db).text(db),
                    match_body = variants.iter().map(|rhs_variant|{
                        if lhs_variant.name(db).text(db) == rhs_variant.name(db).text(db) {
                            format!("{name}::{rhs_variant_name}(y) => x == y,", rhs_variant_name = rhs_variant.name(db).text(db))
                        } else {
                            format!("{name}::{rhs_variant_name}(y) => false,", rhs_variant_name = rhs_variant.name(db).text(db))
                        }
                    }).join("\n                "),
                )
            }).join("\n            ")}
        }
        ExtraInfo::Struct { members, type_generics, other_generics } => {
            let generics = format_generics(type_generics, other_generics);
            let generics_impl = format_generics_with_trait(type_generics, other_generics, |t| {
                format!("impl {t}PartialEq: PartialEq<{t}>, impl {t}Destruct: Destruct<{t}>")
            });
            if members.is_empty() {
                formatdoc! {"
                    impl {name}PartialEq{generics_impl} of PartialEq::<{name}{generics}> {{
                        fn eq(lhs: @{name}{generics}, rhs: @{name}{generics}) -> bool {{ true }}
                        fn ne(lhs: @{name}{generics}, rhs: @{name}{generics}) -> bool {{ false }}
                    }}
                "}
            } else {
                formatdoc! {"
                    impl {name}PartialEq{generics_impl} of PartialEq::<{name}{generics}> {{
                        #[inline(always)]
                        fn eq(lhs: @{name}{generics}, rhs: @{name}{generics}) -> bool {{
                            {}
                        }}
                        #[inline(always)]
                        fn ne(lhs: @{name}{generics}, rhs: @{name}{generics}) -> bool {{
                            !(lhs == rhs)
                        }}
                    }}
                ", members.iter().map(|member| format!("lhs.{member} == rhs.{member}")).join(" && ")
                }
            }
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn get_serde_impl(name: &str, extra_info: &ExtraInfo, db: &dyn SyntaxGroup) -> String {
    match extra_info {
        ExtraInfo::Enum(variants) => {
            formatdoc! {"
                    impl {name}Serde of serde::Serde::<{name}> {{
                        fn serialize(self: @{name}, ref output: array::Array<felt252>) {{
                            match self {{
                                {}
                            }}
                        }}
                        fn deserialize(ref serialized: array::Span<felt252>) -> Option<{name}> {{
                            let idx: felt252 = serde::Serde::deserialize(ref serialized)?;
                            Option::Some(
                                {}
                                else {{ return Option::None; }}
                            )
                        }}
                    }}
                ",
                variants.iter().enumerate().map(|(idx, variant)| {
                    format!(
                        "{name}::{variant_name}(x) => {{ serde::Serde::serialize(@{idx}, ref output); \
                        serde::Serde::serialize(x, ref output); }},",
                        variant_name = variant.name(db).text(db)
                    )
                }).join("\n            "),
                variants.iter().enumerate().map(|(idx, variant)| {
                    format!(
                        "if idx == {idx} {{ {name}::{variant_name}(serde::Serde::deserialize(ref serialized)?) }}",
                        variant_name = variant.name(db).text(db)
                    )
                }).join("\n            else "),
            }
        }
        ExtraInfo::Struct { members, type_generics, other_generics } => {
            formatdoc! {"
                    impl {name}Serde{generics_impl} of serde::Serde::<{name}{generics}> {{
                        fn serialize(self: @{name}{generics}, ref output: array::Array<felt252>) {{
                            {}
                        }}
                        fn deserialize(ref serialized: array::Span<felt252>) -> Option<{name}{generics}> {{
                            Option::Some({name} {{
                                {}
                            }})
                        }}
                    }}
                ",
                members.iter().map(|member| format!("serde::Serde::serialize(self.{member}, ref output)")).join(";\n        "),
                members.iter().map(|member| format!("{member}: serde::Serde::deserialize(ref serialized)?,")).join("\n            "),
                generics = format_generics(type_generics, other_generics),
                generics_impl = format_generics_with_trait(type_generics, other_generics,
                    |t| format!("impl {t}Serde: serde::Serde<{t}>, impl {t}Destruct: Destruct<{t}>"))
            }
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn generate_default_impl(
    name: &str,
    extra_info: &ExtraInfo,
    diagnostics_ptr: SyntaxStablePtrId,
    diagnostics: &mut Vec<PluginDiagnostic>,
    impls: &mut Vec<String>,
    db: &dyn SyntaxGroup,
) {
    match extra_info {
        ExtraInfo::Enum(variants) => {
            let default_variants: Vec<&Variant> =
                variants.iter().filter(|variant| variant.has_attr(db, "default")).collect();

            if default_variants.len() != 1 {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: diagnostics_ptr,
                    message: "Exactly one variant must be anotated with #[default]".into(),
                });
            } else {
                let default_impl_for_enum = formatdoc! {"
                        impl {name}Default of Default::<{name}> {{
                            fn default() -> {name} {{
                                {}
                            }}
                        }}
                    ",
                    format!("{name}::{}(Default::default())", default_variants[0].name(db).text(db))
                };
                impls.push(default_impl_for_enum);
            }
        }
        ExtraInfo::Struct { ref members, ref type_generics, ref other_generics } => {
            let default_impl_for_struct = formatdoc! {"
                    impl {name}Default{generics_impl} of Default::<{name}{generics}> {{
                        fn default() -> {name} {{
                            {name} {{
                                {}
                            }}
                        }}
                    }}
                ",
                members.iter().map(|member| format!("{member}: Default::default()")).join(",\n        "),
                generics = format_generics(type_generics, other_generics),
                generics_impl = format_generics_with_trait(type_generics, other_generics,
                    |t| format!("impl {t}Destruct: Destruct<{t}>"))
            };
            impls.push(default_impl_for_struct);
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn get_empty_impl(name: &str, derived_trait: &str, extra_info: &ExtraInfo) -> String {
    match extra_info {
        ExtraInfo::Struct { type_generics, other_generics, .. } => format!(
            "impl {name}{derived_trait}{generics_impl} of {derived_trait}::<{name}{generics}>;\n",
            generics = format_generics(type_generics, other_generics),
            generics_impl = format_generics_with_trait(type_generics, other_generics, |t| format!(
                "impl {t}{derived_trait}: {derived_trait}<{t}>"
            ))
        ),
        _ => format!("impl {name}{derived_trait} of {derived_trait}::<{name}>;\n"),
    }
}
