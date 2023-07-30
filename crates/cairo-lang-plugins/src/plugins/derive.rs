use cairo_lang_defs::plugin::{MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult};
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{
    AttributeList, MemberList, OptionWrappedGenericParamList, VariantList,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use indent::indent_by;
use indoc::formatdoc;
use itertools::{chain, Itertools};
use smol_str::SmolStr;

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct DerivePlugin;

impl MacroPlugin for DerivePlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        generate_derive_code_for_type(
            db,
            match item_ast {
                ast::Item::Struct(struct_ast) => DeriveInfo::new(
                    db,
                    struct_ast.name(db),
                    struct_ast.attributes(db),
                    struct_ast.generic_params(db),
                    TypeVariantInfo::Struct(extract_members(db, struct_ast.members(db))),
                ),
                ast::Item::Enum(enum_ast) => DeriveInfo::new(
                    db,
                    enum_ast.name(db),
                    enum_ast.attributes(db),
                    enum_ast.generic_params(db),
                    TypeVariantInfo::Enum(extract_variants(db, enum_ast.variants(db))),
                ),
                ast::Item::ExternType(extern_type_ast) => DeriveInfo::new(
                    db,
                    extern_type_ast.name(db),
                    extern_type_ast.attributes(db),
                    extern_type_ast.generic_params(db),
                    TypeVariantInfo::Extern,
                ),
                _ => return PluginResult::default(),
            },
        )
    }
}

/// Information on struct members or enum variants.
struct MemberInfo {
    name: SmolStr,
    _ty: String,
    _attributes: AttributeList,
}

/// Information on the type being derived.
enum TypeVariantInfo {
    Enum(Vec<MemberInfo>),
    Struct(Vec<MemberInfo>),
    Extern,
}

/// Information on generic params.
struct GenericParamsInfo {
    /// All the generic params name, at the original order.
    ordered: Vec<SmolStr>,
    /// The generic params name that are types.
    type_generics: Vec<SmolStr>,
    /// The generic params name that are not types.
    other_generics: Vec<String>,
}
impl GenericParamsInfo {
    /// Extracts the information on generic params.
    fn new(db: &dyn SyntaxGroup, generic_params: OptionWrappedGenericParamList) -> Self {
        let mut ordered = vec![];
        let mut type_generics = vec![];
        let mut other_generics = vec![];
        match generic_params {
            OptionWrappedGenericParamList::WrappedGenericParamList(gens) => gens
                .generic_params(db)
                .elements(db)
                .into_iter()
                .map(|param| match param {
                    ast::GenericParam::Type(t) => {
                        let name = t.name(db).text(db);
                        type_generics.push(name.clone());
                        ordered.push(name);
                    }
                    ast::GenericParam::Impl(i) => {
                        other_generics.push(i.as_syntax_node().get_text_without_trivia(db));
                        ordered.push(i.name(db).text(db));
                    }
                    ast::GenericParam::Const(c) => {
                        other_generics.push(c.as_syntax_node().get_text_without_trivia(db));
                        ordered.push(c.name(db).text(db));
                    }
                })
                .collect(),
            OptionWrappedGenericParamList::Empty(_) => vec![],
        };
        Self { ordered, type_generics, other_generics }
    }

    /// Formats the generic params for the type.
    /// `additional_demands` formats the generic type params as additional trait bounds.
    fn format_generics_with_trait(
        &self,
        additional_demands: impl Fn(&SmolStr) -> Vec<String>,
    ) -> String {
        if self.ordered.is_empty() {
            "".to_string()
        } else {
            format!(
                "<{}>",
                chain!(
                    self.type_generics.iter().map(|s| s.to_string()),
                    self.other_generics.iter().cloned(),
                    self.type_generics.iter().flat_map(additional_demands)
                )
                .join(", ")
            )
        }
    }

    /// Formats the generic params for the type.
    fn format_generics(&self) -> String {
        if self.ordered.is_empty() {
            "".to_string()
        } else {
            format!("<{}>", self.ordered.iter().join(", "))
        }
    }
}

/// Information for the type being derived.
struct DeriveInfo {
    name: SmolStr,
    attributes: AttributeList,
    generics: GenericParamsInfo,
    specific_info: TypeVariantInfo,
}
impl DeriveInfo {
    /// Extracts the information on the type being derived.
    fn new(
        db: &dyn SyntaxGroup,
        ident: ast::TerminalIdentifier,
        attributes: AttributeList,
        generic_args: OptionWrappedGenericParamList,
        specific_info: TypeVariantInfo,
    ) -> Self {
        Self {
            name: ident.text(db),
            attributes,
            generics: GenericParamsInfo::new(db, generic_args),
            specific_info,
        }
    }

    /// Formats the header of the impl.
    fn format_impl_header(&self, derived_trait: &str, dependent_traits: &[&str]) -> String {
        format!(
            "impl {name}{derived_trait}{generics_impl} of {derived_trait}::<{full_typename}>",
            name = self.name,
            generics_impl = self.generics.format_generics_with_trait(|t| dependent_traits
                .iter()
                .map(|d| format!("impl {t}{d}: {d}<{t}>"))
                .collect()),
            full_typename = self.full_typename(),
        )
    }

    /// Formats the full typename of the type, including generic args.
    fn full_typename(&self) -> String {
        format!("{name}{generics}", name = self.name, generics = self.generics.format_generics())
    }
}

/// Extracts the information on the members of the struct.
fn extract_members(db: &dyn SyntaxGroup, members: MemberList) -> Vec<MemberInfo> {
    members
        .elements(db)
        .into_iter()
        .map(|member| MemberInfo {
            name: member.name(db).text(db),
            _ty: member.type_clause(db).ty(db).as_syntax_node().get_text_without_trivia(db),
            _attributes: member.attributes(db),
        })
        .collect()
}

/// Extracts the information on the variants of the enum.
fn extract_variants(db: &dyn SyntaxGroup, variants: VariantList) -> Vec<MemberInfo> {
    variants
        .elements(db)
        .into_iter()
        .map(|variant| MemberInfo {
            name: variant.name(db).text(db),
            _ty: match variant.type_clause(db) {
                ast::OptionTypeClause::Empty(_) => "()".to_string(),
                ast::OptionTypeClause::TypeClause(t) => {
                    t.ty(db).as_syntax_node().get_text_without_trivia(db)
                }
            },
            _attributes: variant.attributes(db),
        })
        .collect()
}

/// Adds an implementation for all requested derives for the type.
fn generate_derive_code_for_type(db: &dyn SyntaxGroup, info: DeriveInfo) -> PluginResult {
    let mut diagnostics = vec![];
    let mut impls = vec![];
    for attr in info.attributes.query_attr(db, "derive") {
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

            let derived = segment.ident(db).text(db);
            match derived.as_str() {
                "Copy" | "Drop" => impls.push(get_empty_impl(&derived, &info)),
                "Clone" if !matches!(&info.specific_info, TypeVariantInfo::Extern) => {
                    impls.push(get_clone_impl(&info))
                }
                "Destruct" if !matches!(&info.specific_info, TypeVariantInfo::Extern) => {
                    impls.push(get_destruct_impl(&info))
                }
                "PanicDestruct" if !matches!(&info.specific_info, TypeVariantInfo::Extern) => {
                    impls.push(get_panic_destruct_impl(&info))
                }
                "PartialEq" if !matches!(&info.specific_info, TypeVariantInfo::Extern) => {
                    impls.push(get_partial_eq_impl(&info))
                }
                "Serde" if !matches!(&info.specific_info, TypeVariantInfo::Extern) => {
                    impls.push(get_serde_impl(&info))
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
                patches: Default::default(),
                aux_data: vec![],
            })
        },
        diagnostics,
        remove_original_item: false,
    }
}

fn get_clone_impl(info: &DeriveInfo) -> String {
    let name = &info.name;
    let full_typename = info.full_typename();
    formatdoc! {"
            {header} {{
                fn clone(self: @{full_typename}) -> {full_typename} {{
                    {body}
                }}
            }}
        ",
        header = info.format_impl_header("Clone", &["Clone", "Destruct"]),
        body = indent_by(8, match &info.specific_info {
            TypeVariantInfo::Enum(variants) => {
                formatdoc! {"
                    match self {{
                        {}
                    }}",
                    variants.iter().map(|variant|
                        format!(
                            "{name}::{variant}(x) => {name}::{variant}(Clone::clone(x)),",
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
                            "{member}: Clone::clone(self.{member}),",
                            member=member.name,
                        )
                    }).join("\n"))
                }
            }
            TypeVariantInfo::Extern => unreachable!(),
        })
    }
}

fn get_destruct_impl(info: &DeriveInfo) -> String {
    let name = &info.name;
    let full_typename = info.full_typename();
    formatdoc! {"
    {header} {{
        fn destruct(self: {full_typename}) nopanic {{
            {body}
        }}
    }}
",
    header = info.format_impl_header("Destruct", &["Destruct"]),
    body = indent_by(8, match &info.specific_info {
        TypeVariantInfo::Enum(variants) => {
            formatdoc! {"
                match self {{
                    {}
                }}",
                variants.iter().map(|variant| {
                format!(
                    "{name}::{variant}(x) => traits::Destruct::destruct(x),",
                    variant=variant.name,
                )
            }).join("\n    ")}
        }
        TypeVariantInfo::Struct(members) => {
            members.iter().map(|member| {
                    format!(
                        "traits::Destruct::destruct(self.{member});",
                        member=member.name,
                    )
                }).join("\n")
        }
        TypeVariantInfo::Extern => unreachable!(),
    })
    }
}

fn get_panic_destruct_impl(info: &DeriveInfo) -> String {
    let name = &info.name;
    let full_typename = info.full_typename();
    formatdoc! {"
    {header} {{
        fn panic_destruct(self: {full_typename}, ref panic: Panic) nopanic {{
            {body}
        }}
    }}
",
    header = info.format_impl_header("PanicDestruct", &["PanicDestruct"]),
    body = indent_by(8, match &info.specific_info {
        TypeVariantInfo::Enum(variants) => {
            formatdoc! {"
                        match self {{
                            {}
                        }}", variants.iter().map(|variant| {
                format!(
                    "{name}::{variant}(x) => traits::PanicDestruct::panic_destruct(x, ref panic),",
                    variant=variant.name,
                )
            }).join("\n    ")}
        }
        TypeVariantInfo::Struct(members) => {
            members.iter().map(|member| {
                    format!(
                        "traits::PanicDestruct::panic_destruct(self.{member}, ref panic);",
                        member=member.name,
                    )
                }).join("\n")
        }
        TypeVariantInfo::Extern => unreachable!(),
    })
    }
}

fn get_partial_eq_impl(info: &DeriveInfo) -> String {
    let name = &info.name;
    let full_typename = info.full_typename();
    formatdoc! {"
    {header} {{
        fn eq(lhs: @{full_typename}, rhs: @{full_typename}) -> bool {{
            {body}
        }}
        #[inline(always)]
        fn ne(lhs: @{full_typename}, rhs: @{full_typename}) -> bool {{
            !(lhs == rhs)
        }}
    }}
",
    header = info.format_impl_header("PartialEq", &["PartialEq"]),
    body = indent_by(8, match &info.specific_info {
        TypeVariantInfo::Enum(variants) => {
            formatdoc! {"
                        match lhs {{
                            {}
                        }}",
                variants.iter().map(|lhs_variant| {
                    indent_by(4, formatdoc! {"
                        {name}::{lhs_variant}(x) => match rhs {{
                            {}
                        }},",
                        variants.iter().map(|rhs_variant|{
                            if lhs_variant.name == rhs_variant.name {
                                format!("{name}::{}(y) => x == y,", rhs_variant.name)
                            } else {
                                format!("{name}::{}(y) => false,", rhs_variant.name)
                            }
                        }).join("\n    "),
                    lhs_variant=lhs_variant.name,
                    })
                }).join("\n    ")}
        }
        TypeVariantInfo::Struct(members) => {
            if members.is_empty() {
                "true".to_string()
            } else {
                members.iter().map(|member|format!("lhs.{member} == rhs.{member}", member=member.name)).join(" && ")
            }
        }
        TypeVariantInfo::Extern => unreachable!(),
    })
    }
}

fn get_serde_impl(info: &DeriveInfo) -> String {
    let name = &info.name;
    let full_typename = info.full_typename();
    formatdoc! {"
            {header} {{
                fn serialize(self: @{full_typename}, ref output: array::Array<felt252>) {{
                    {serialize_body}
                }}
                fn deserialize(ref serialized: array::Span<felt252>) -> Option<{full_typename}> {{
                    {deserialize_body}
                }}
            }}
        ",
        header = info.format_impl_header("Serde", &["Serde", "Destruct"]),
        serialize_body = indent_by(8, match &info.specific_info {
            TypeVariantInfo::Enum(variants) => {
                formatdoc! {"
                    match self {{
                        {}
                    }}",
                    variants.iter().enumerate().map(|(idx, variant)| {
                    format!(
                        "{name}::{variant}(x) => {{ serde::Serde::serialize(@{idx}, ref output); \
                        serde::Serde::serialize(x, ref output); }},",
                        variant=variant.name,
                    )
                }).join("\n    ")}
            }
            TypeVariantInfo::Struct(members) => {
                members.iter().map(|member| format!("serde::Serde::serialize(self.{member}, ref output)", member=member.name)).join(";\n")
            }
            TypeVariantInfo::Extern => unreachable!(),
        }),
        deserialize_body = indent_by(8, match &info.specific_info {
            TypeVariantInfo::Enum(variants) => {
                formatdoc! {"
                    let idx: felt252 = serde::Serde::deserialize(ref serialized)?;
                    Option::Some(
                        {}
                        else {{ return Option::None; }}
                    )",
                    variants.iter().enumerate().map(|(idx, variant)| {
                        format!(
                            "if idx == {idx} {{ {name}::{variant}(serde::Serde::deserialize(ref serialized)?) }}",
                            variant=variant.name,
                        )
                }).join("\n    else ")}
            }
            TypeVariantInfo::Struct(members) => {
                formatdoc! {"
                    Option::Some({name} {{
                        {}
                    }})",
                    members.iter().map(|member| format!("{member}: serde::Serde::deserialize(ref serialized)?,", member=member.name)).join("\n    "),
                }
            }
            TypeVariantInfo::Extern => unreachable!(),
        })
    }
}

fn get_empty_impl(derived_trait: &str, info: &DeriveInfo) -> String {
    format!("{};\n", info.format_impl_header(derived_trait, &[derived_trait]))
}
