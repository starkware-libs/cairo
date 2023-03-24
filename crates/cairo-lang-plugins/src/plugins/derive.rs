use std::sync::Arc;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin, TrivialPluginAuxData};
use cairo_lang_syntax::node::ast::{AttributeList, MemberList};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use indoc::formatdoc;
use itertools::Itertools;
use smol_str::SmolStr;

#[derive(Debug)]
pub struct DerivePlugin;

impl MacroPlugin for DerivePlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Struct(struct_ast) => generate_derive_code_for_type(
                db,
                struct_ast.name(db),
                struct_ast.attributes(db),
                ExtraInfo::Struct(member_names(db, struct_ast.members(db))),
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
    Struct(Vec<SmolStr>),
    Extern,
}

fn member_names(db: &dyn SyntaxGroup, members: MemberList) -> Vec<SmolStr> {
    members.elements(db).into_iter().map(|member| member.name(db).text(db)).collect()
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
    for attr in attributes.elements(db) {
        if attr.attr(db).text(db) == "derive" {
            if let ast::OptionAttributeArgs::AttributeArgs(args) = attr.args(db) {
                for arg in args.arg_list(db).elements(db) {
                    if let ast::Expr::Path(expr) = arg {
                        if let [ast::PathSegment::Simple(segment)] = &expr.elements(db)[..] {
                            let name = ident.text(db);
                            let derived = segment.ident(db).text(db);
                            match derived.as_str() {
                                "Copy" | "Drop" => impls.push(get_empty_impl(&name, &derived)),
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
                                        stable_ptr: expr.stable_ptr().untyped(),
                                        message: "Unsupported trait for derive for extern types."
                                            .into(),
                                    })
                                }
                                _ => diagnostics.push(PluginDiagnostic {
                                    stable_ptr: expr.stable_ptr().untyped(),
                                    message: "Unsupported trait for derive.".into(),
                                }),
                            }
                        } else {
                            diagnostics.push(PluginDiagnostic {
                                stable_ptr: expr.stable_ptr().untyped(),
                                message: "Expected a single segment.".into(),
                            });
                        }
                    } else {
                        diagnostics.push(PluginDiagnostic {
                            stable_ptr: arg.stable_ptr().untyped(),
                            message: "Expected path.".into(),
                        });
                    }
                }
            } else {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: attr.args(db).stable_ptr().untyped(),
                    message: "Expected args.".into(),
                });
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
        ExtraInfo::Struct(members) => {
            formatdoc! {"
                    impl {name}Clone of Clone::<{name}> {{
                        fn clone(self: @{name}) -> {name} {{
                            {name} {{
                                {}
                            }}
                        }}
                    }}
                ", members.iter().map(|member| {
                format!("{member}: self.{member}.clone(),")
            }).join("\n            ")}
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
        ExtraInfo::Struct(members) => {
            formatdoc! {"
                    impl {name}Destruct of Destruct::<{name}> {{
                        fn destruct(self: {name}) nopanic {{
                            {}
                        }}
                    }}
                ", members.iter().map(|member| {
                format!("traits::Destruct::destruct(self.{member});")
            }).join("\n        ")}
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
        ExtraInfo::Struct(members) => {
            formatdoc! {"
                    impl {name}PartialEq of PartialEq::<{name}> {{
                        #[inline(always)]
                        fn eq(lhs: {name}, rhs: {name}) -> bool {{
                            {}
                            true
                        }}
                        #[inline(always)]
                        fn ne(lhs: {name}, rhs: {name}) -> bool {{
                            !(lhs == rhs)
                        }}
                    }}
                ", members.iter().map(|member| {
                // TODO(orizi): Use `&&` when supported.
                format!("if lhs.{member} != rhs.{member} {{ return false; }}")
            }).join("\n        ")}
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn get_serde_impl(name: &str, extra_info: &ExtraInfo) -> String {
    match extra_info {
        ExtraInfo::Enum(variants) => {
            formatdoc! {"
                    impl {name}Serde of serde::Serde::<{name}> {{
                        fn serialize(ref output: array::Array<felt252>, value: {name}) {{
                            match lhs {{
                                {}
                            }}
                        }}
                        fn deserialize(ref input: array::Span<felt252>) -> Option<{name}> {{
                            let idx: felt252 = serde::Serde::deserialize(ref input)?;
                            Option::Some(
                                {}
                                else {{ None }}
                            )
                        }}
                    }}
                ",
                variants.iter().enumerate().map(|(idx, variant)| {
                    format!(
                        "{name}::{variant}(x) => serde::Serde::serialize(ref output, ({idx}, x)),",
                    )
                }).join("\n            "),
                variants.iter().enumerate().map(|(idx, variant)| {
                    format!(
                        "if idx == {idx} {{ {name}::{variant}(serde::Serde::deserialize(ref input)?) }}",
                    )
                }).join("\n            else "),
            }
        }
        ExtraInfo::Struct(members) => {
            formatdoc! {"
                    impl {name}Serde of serde::Serde::<{name}> {{
                        fn serialize(ref output: array::Array<felt252>, value: {name}) {{
                            {}
                        }}
                        fn deserialize(ref input: array::Span<felt252>) -> Option<{name}> {{
                            Option::Some({name} {{
                                {}
                            }})
                        }}
                    }}
                ",
                members.iter().map(|member| format!("serde::Serde::serialize(ref output, value.{member})")).join(";\n        "),
                members.iter().map(|member| format!("{member}: serde::Serde::deserialize(ref input)?,")).join("\n            "),
            }
        }
        ExtraInfo::Extern => unreachable!(),
    }
}

fn get_empty_impl(name: &str, derived_trait: &str) -> String {
    format!("impl {name}{derived_trait} of {derived_trait}::<{name}>;\n")
}
