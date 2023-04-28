use std::sync::Arc;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin, TrivialPluginAuxData};
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{
    AttributeList, ItemStruct, MemberList, OptionWrappedGenericParamList,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal};
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
                ExtraInfo::Struct(
                    member_names(db, struct_ast.members(db)),
                    extract_generics(db, &struct_ast),
                ),
            ),
            ast::Item::Enum(enum_ast) => generate_derive_code_for_type(
                db,
                enum_ast.name(db),
                enum_ast.attributes(db),
                ExtraInfo::Enum(member_names(db, enum_ast.variants(db))),
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
    Enum(Vec<SmolStr>),
    Struct(Vec<SmolStr>, Vec<SmolStr>),
    Extern,
}

fn member_names(db: &dyn SyntaxGroup, members: MemberList) -> Vec<SmolStr> {
    members.elements(db).into_iter().map(|member| member.name(db).text(db)).collect()
}

fn extract_generics(db: &dyn SyntaxGroup, struct_ast: &ItemStruct) -> Vec<SmolStr> {
    match struct_ast.generic_params(db) {
        OptionWrappedGenericParamList::WrappedGenericParamList(gens) => gens
            .generic_params(db)
            .elements(db)
            .into_iter()
            .map(|member| match member {
                ast::GenericParam::Type(t) => t.name(db).text(db),
                ast::GenericParam::Const(c) => c.name(db).text(db),
                ast::GenericParam::Impl(i) => i.name(db).text(db),
            })
            .collect(),
        OptionWrappedGenericParamList::Empty(_) => vec![],
    }
}

fn format_generics_with_trait(
    generic_args: &Vec<SmolStr>,
    f: impl Fn(&SmolStr) -> String,
) -> String {
    match generic_args.is_empty() {
        true => "".into(),
        false => {
            format!("<{}, {}>", generic_args.join(", "), generic_args.iter().map(f).join(", "))
        }
    }
}

fn format_generics(generic_args: &Vec<SmolStr>) -> String {
    match generic_args.is_empty() {
        true => "".into(),
        false => format!("<{}>", generic_args.iter().join(", ")),
    }
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
            let AttributeArg{
                variant: AttributeArgVariant::Unnamed {
                    value: ast::Expr::Path(path),
                    value_stable_ptr,
                    ..
                },
                ..
            } = arg else {
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
                    impls.push(get_clone_impl(&name, &extra_info))
                }
                "Destruct" if !matches!(extra_info, ExtraInfo::Extern) => {
                    impls.push(get_destruct_impl(&name, &extra_info))
                }
                "PartialEq" if !matches!(extra_info, ExtraInfo::Extern) => {
                    impls.push(get_partial_eq_impl(&name, &extra_info))
                }
                "Serde" if !matches!(extra_info, ExtraInfo::Extern) => {
                    impls.push(get_serde_impl(&name, &extra_info))
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

fn get_clone_impl(name: &str, extra_info: &ExtraInfo) -> String {
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
                format!("{name}::{variant}(x) => {name}::{variant}(x.clone()),")
            }).join("\n            ")}
        }
        ExtraInfo::Struct(members, generic_args) => {
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
                generics = format_generics(generic_args),
                generics_impl = format_generics_with_trait(generic_args, |t| format!("impl {t}Clone: Clone<{t}>"))
            }
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn get_destruct_impl(name: &str, extra_info: &ExtraInfo) -> String {
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
                format!("{name}::{variant}(x) => traits::Destruct::destruct(x),")
            }).join("\n            ")}
        }
        ExtraInfo::Struct(members, generic_args) => {
            formatdoc! {"
                    impl {name}Destruct{generics_impl} of Destruct::<{name}{generics}> {{
                        fn destruct(self: {name}{generics}) nopanic {{
                            {}
                        }}
                    }}
                ", members.iter().map(|member| {
                            format!("traits::Destruct::destruct(self.{member});")
                        }).join("\n        "),
                        generics = format_generics(generic_args),
                        generics_impl = format_generics_with_trait(generic_args, |t| format!("impl {t}Destruct: Destruct<{t}>"))
            }
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn get_partial_eq_impl(name: &str, extra_info: &ExtraInfo) -> String {
    match extra_info {
        ExtraInfo::Enum(variants) => {
            formatdoc! {"
                    impl {name}PartialEq of PartialEq::<{name}> {{
                        fn eq(lhs: {name}, rhs: {name}) -> bool {{
                            match lhs {{
                                {}
                            }}
                        }}
                        #[inline(always)]
                        fn ne(lhs: {name}, rhs: {name}) -> bool {{
                            !(lhs == rhs)
                        }}
                    }}
                ", variants.iter().map(|lhs_variant| {
                format!(
                    "{name}::{lhs_variant}(x) => match rhs {{\n                {}\n            }},",
                    variants.iter().map(|rhs_variant|{
                        if lhs_variant == rhs_variant {
                            format!("{name}::{rhs_variant}(y) => x == y,")
                        } else {
                            format!("{name}::{rhs_variant}(y) => false,")
                        }
                    }).join("\n                "),
                )
            }).join("\n            ")}
        }
        ExtraInfo::Struct(members, generic_args) => {
            formatdoc! {"
                    impl {name}PartialEq{generics_impl} of PartialEq::<{name}{generics}> {{
                        #[inline(always)]
                        fn eq(lhs: {name}{generics}, rhs: {name}{generics}) -> bool {{
                            {}
                            true
                        }}
                        #[inline(always)]
                        fn ne(lhs: {name}{generics}, rhs: {name}{generics}) -> bool {{
                            !(lhs == rhs)
                        }}
                    }}
                ", members.iter().map(|member| {
                            // TODO(orizi): Use `&&` when supported.
                            format!("if lhs.{member} != rhs.{member} {{ return false; }}")
                        }).join("\n        "),
                        generics = format_generics(generic_args),
                        generics_impl = format_generics_with_trait(generic_args, |t| format!("impl {t}Destruct: Destruct<{t}>"))
            }
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn get_serde_impl(name: &str, extra_info: &ExtraInfo) -> String {
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
                                else {{ return Option::None(()); }}
                            )
                        }}
                    }}
                ",
                variants.iter().enumerate().map(|(idx, variant)| {
                    format!(
                        "{name}::{variant}(x) => {{ serde::Serde::serialize(@{idx}, ref output); serde::Serde::serialize(x, ref output); }},",
                    )
                }).join("\n            "),
                variants.iter().enumerate().map(|(idx, variant)| {
                    format!(
                        "if idx == {idx} {{ {name}::{variant}(serde::Serde::deserialize(ref serialized)?) }}",
                    )
                }).join("\n            else "),
            }
        }
        ExtraInfo::Struct(members, generic_args) => {
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
                generics = format_generics(generic_args),
                generics_impl = format_generics_with_trait(generic_args, |t| format!("impl {t}Destruct: Destruct<{t}>"))
            }
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn get_empty_impl(name: &str, derived_trait: &str, extra_info: &ExtraInfo) -> String {
    match extra_info {
        ExtraInfo::Struct(_, generic_args) => format!(
            "impl {name}{derived_trait}{generics_impl} of {derived_trait}::<{name}{generics}>;\n",
            generics = format_generics(generic_args),
            generics_impl = format_generics_with_trait(generic_args, |t| format!(
                "impl {t}{dt}: {dt}<{t}>",
                dt = derived_trait
            ))
        ),
        _ => format!("impl {name}{derived_trait} of {derived_trait}::<{name}>;\n"),
    }
}
