use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
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
use indoc::indoc;
use itertools::chain;

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
    name: RewriteNode,
    _ty: RewriteNode,
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
    ordered: Vec<RewriteNode>,
    /// The generic params name that are types.
    type_generics: Vec<RewriteNode>,
    /// The generic params name that are not types.
    other_generics: Vec<RewriteNode>,
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
                        let rewrite = RewriteNode::new_trimmed(t.name(db).as_syntax_node());
                        ordered.push(rewrite.clone());
                        type_generics.push(rewrite);
                    }
                    ast::GenericParam::Impl(i) => {
                        ordered.push(RewriteNode::new_trimmed(i.name(db).as_syntax_node()));
                        other_generics.push(RewriteNode::new_trimmed(i.as_syntax_node()));
                    }
                    ast::GenericParam::Const(c) => {
                        ordered.push(RewriteNode::new_trimmed(c.name(db).as_syntax_node()));
                        other_generics.push(RewriteNode::new_trimmed(c.as_syntax_node()));
                    }
                })
                .collect(),
            OptionWrappedGenericParamList::Empty(_) => vec![],
        };
        Self { ordered, type_generics, other_generics }
    }

    /// Returns a node for generics and their traits.
    /// `additional_demands` formats the generic type params as additional trait bounds.
    fn generics_with_trait_part(
        &self,
        additional_demands: impl Fn(&RewriteNode) -> Vec<RewriteNode>,
    ) -> RewriteNode {
        self.generics_helper(chain!(
            self.type_generics.iter().cloned(),
            self.other_generics.iter().cloned(),
            self.type_generics.iter().flat_map(|t| additional_demands(t)),
        ))
    }

    /// Returns a node for the generics of a type.
    fn generics_part(&self) -> RewriteNode {
        self.generics_helper(self.ordered.iter().cloned())
    }

    fn generics_helper(&self, mut iter: impl Iterator<Item = RewriteNode>) -> RewriteNode {
        let Some(first) = iter.next() else {
            return RewriteNode::new_modified(vec![]);
        };
        let mut children = vec!["<".into()];
        children.push(first);
        for node in iter {
            children.push(", ".into());
            children.push(node);
        }
        children.push(">".into());
        RewriteNode::new_modified(children)
    }
}

/// Information for the type being derived.
struct DeriveInfo {
    name: RewriteNode,
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
            name: RewriteNode::new_trimmed(ident.as_syntax_node()),
            attributes,
            generics: GenericParamsInfo::new(db, generic_args),
            specific_info,
        }
    }

    fn add_impl_header(
        &self,
        builder: &mut PatchBuilder,
        derived_trait: &str,
        dependent_traits: &[&str],
    ) {
        builder.add_modified(self.impl_header(derived_trait, dependent_traits))
    }

    /// Returns a node for the impl header.
    fn impl_header(&self, derived_trait: &str, dependent_traits: &[&str]) -> RewriteNode {
        RewriteNode::interpolate_patched(
            "impl $name$$derived_trait$$generics_with_trait$ of $derived_trait$<$full_typename$>",
            [
                ("name".into(), self.name.clone()),
                ("derived_trait".into(), RewriteNode::from(derived_trait)),
                ("full_typename".into(), self.full_typename()),
                ("generics".into(), RewriteNode::from(derived_trait)),
                (
                    "generics_with_trait".into(),
                    self.generics.generics_with_trait_part(|t| {
                        dependent_traits
                            .iter()
                            .map(|d| {
                                RewriteNode::interpolate_patched(
                                    "impl $t$$d$: $d$<$t$>",
                                    [("t".into(), t.clone()), ("d".into(), (*d).into())].into(),
                                )
                            })
                            .collect()
                    }),
                ),
            ]
            .into(),
        )
    }

    fn full_typename(&self) -> RewriteNode {
        RewriteNode::new_modified(vec![self.name.clone(), self.generics.generics_part()])
    }

    /// Formats the full typename of the type, including generic args.
    fn add_full_typename(&self, builder: &mut PatchBuilder) {
        builder.add_modified(self.full_typename());
    }
}

/// Extracts the information on the members of the struct.
fn extract_members(db: &dyn SyntaxGroup, members: MemberList) -> Vec<MemberInfo> {
    members
        .elements(db)
        .into_iter()
        .map(|member| MemberInfo {
            name: RewriteNode::new_trimmed(member.name(db).as_syntax_node()),
            _ty: RewriteNode::new_trimmed(member.type_clause(db).ty(db).as_syntax_node()),
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
            name: RewriteNode::new_trimmed(variant.name(db).as_syntax_node()),
            _ty: match variant.type_clause(db) {
                ast::OptionTypeClause::Empty(_) => RewriteNode::Text("()".to_string()),
                ast::OptionTypeClause::TypeClause(t) => {
                    RewriteNode::new_trimmed(t.ty(db).as_syntax_node())
                }
            },
            _attributes: variant.attributes(db),
        })
        .collect()
}

/// Adds an implementation for all requested derives for the type.
fn generate_derive_code_for_type(db: &dyn SyntaxGroup, info: DeriveInfo) -> PluginResult {
    let mut diagnostics = vec![];
    let mut builder = PatchBuilder::new(db);

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
                "Copy" | "Drop" => add_empty_impl(&mut builder, &info, &derived),
                "Clone" if !matches!(&info.specific_info, TypeVariantInfo::Extern) => {
                    add_clone_impl(&mut builder, &info)
                }
                "Destruct" if !matches!(&info.specific_info, TypeVariantInfo::Extern) => {
                    add_destruct_impl(&mut builder, &info)
                }
                "PanicDestruct" if !matches!(&info.specific_info, TypeVariantInfo::Extern) => {
                    add_panic_destruct_impl(&mut builder, &info)
                }
                "PartialEq" if !matches!(&info.specific_info, TypeVariantInfo::Extern) => {
                    add_partial_eq_impl(&mut builder, &info)
                }
                "Serde" if !matches!(&info.specific_info, TypeVariantInfo::Extern) => {
                    add_serde_impl(&mut builder, &info)
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
        code: if builder.code.is_empty() {
            None
        } else {
            Some(PluginGeneratedFile {
                name: "impls".into(),
                content: builder.code,
                patches: builder.patches,
                aux_data: vec![],
            })
        },
        diagnostics,
        remove_original_item: false,
    }
}

fn add_clone_impl(builder: &mut PatchBuilder, info: &DeriveInfo) {
    let impl_header = info.impl_header("Clone", &["Clone", "Destruct"]);
    builder.add_modified(RewriteNode::interpolate_patched(
        indoc! {"
            $impl_header$ {
                fn clone(self: @$full_typename$) -> $full_typename$ {
                    $body$
                }
            }
        "},
        [
            ("impl_header".into(), impl_header),
            ("full_typename".into(), info.full_typename()),
            (
                "body".into(),
                match &info.specific_info {
                    TypeVariantInfo::Enum(variants) => {
                        let mut children = vec!["match self {\n".into()];
                        for variant in variants {
                            children.push(RewriteNode::interpolate_patched(
                                "            $name$::$variant_name$(x) => \
                                 $name$::$variant_name$(Clone::clone(x)),\n",
                                [
                                    ("name".into(), info.name.clone()),
                                    ("variant_name".into(), variant.name.clone()),
                                ]
                                .into(),
                            ));
                        }
                        children.push("        }".into());
                        RewriteNode::new_modified(children)
                    }
                    TypeVariantInfo::Struct(members) => {
                        let mut children = vec![info.name.clone(), " {\n".into()];
                        for member in members {
                            children.push(RewriteNode::interpolate_patched(
                                "            $member_name$: Clone::clone(self.$member_name$),\n",
                                [("member_name".into(), member.name.clone())].into(),
                            ));
                        }
                        children.push("        }".into());
                        RewriteNode::new_modified(children)
                    }
                    TypeVariantInfo::Extern => unreachable!(),
                },
            ),
        ]
        .into(),
    ))
}

fn add_destruct_impl(builder: &mut PatchBuilder, info: &DeriveInfo) {
    let impl_header = info.impl_header("Destruct", &["Destruct"]);
    builder.add_modified(RewriteNode::interpolate_patched(
        indoc! {"
            $impl_header$ {
                fn destruct(self: $full_typename$) nopanic {
            $body$
                }
            }
        "},
        [
            ("impl_header".into(), impl_header),
            ("full_typename".into(), info.full_typename()),
            ("body".into(), match &info.specific_info {
                TypeVariantInfo::Enum(variants) => {
                    let mut children = vec!["        match self {\n".into()];
                    for variant in variants {
                        children.push(RewriteNode::interpolate_patched(
                            "            $name$::$variant_name$(x) => traits::Destruct::destruct(x),\n",
                            [
                                ("name".into(), info.name.clone()),
                                ("variant_name".into(), variant.name.clone()),
                            ]
                            .into(),
                        ));
                    }
                    children.push("        }".into());
                    RewriteNode::new_modified(children)
                }
                TypeVariantInfo::Struct(members) => {
                    let mut children = vec![];
                    for member in members {
                        children.push(RewriteNode::interpolate_patched(
                            "        traits::Destruct::destruct(self.$member_name$);\n",
                            [("member_name".into(), member.name.clone())].into(),
                        ));
                    }
                    RewriteNode::new_modified(children)
                }
                TypeVariantInfo::Extern => unreachable!(),
            })
        ].into()
    ));
}

fn add_panic_destruct_impl(builder: &mut PatchBuilder, info: &DeriveInfo) {
    info.add_impl_header(builder, "PanicDestruct", &["PanicDestruct"]);
    builder.add_str(" {\n");
    builder.add_str("    fn panic_destruct(self: @");
    info.add_full_typename(builder);
    builder.add_str(", ref panic: Panic) nopanic {\n");
    match &info.specific_info {
        TypeVariantInfo::Enum(variants) => {
            builder.add_str("        match self {\n");
            for variant in variants {
                builder.add_str("            ");
                builder.add_modified(info.name.clone());
                builder.add_str("::");
                builder.add_modified(variant.name.clone());
                builder.add_str("(x) => ");
                builder.add_str("traits::PanicDestruct::panic_destruct(x, ref panic),\n");
            }
            builder.add_str("        }\n");
        }
        TypeVariantInfo::Struct(members) => {
            for member in members {
                builder.add_str("        traits::PanicDestruct::panic_destruct(self.");
                builder.add_modified(member.name.clone());
                builder.add_str(", ref panic);\n");
            }
        }
        TypeVariantInfo::Extern => unreachable!(),
    }
    builder.add_str("    }\n");
    builder.add_str("}\n");
}

fn add_partial_eq_impl(builder: &mut PatchBuilder, info: &DeriveInfo) {
    info.add_impl_header(builder, "PartialEq", &["PartialEq"]);
    builder.add_str(" {\n");
    builder.add_str("    fn eq(lhs: @");
    info.add_full_typename(builder);
    builder.add_str(", rhs: @");
    info.add_full_typename(builder);
    builder.add_str(") -> bool {\n");
    match &info.specific_info {
        TypeVariantInfo::Enum(variants) => {
            builder.add_str("        match lhs {\n");
            for lhs_variant in variants {
                builder.add_str("            ");
                builder.add_modified(info.name.clone());
                builder.add_str("::");
                builder.add_modified(lhs_variant.name.clone());
                builder.add_str("(x) => match rhs {\n");
                for rhs_variant in variants {
                    builder.add_str("                ");
                    builder.add_modified(info.name.clone());
                    builder.add_str("::");
                    builder.add_modified(rhs_variant.name.clone());
                    builder.add_str("(y) => ");
                    if lhs_variant.name == rhs_variant.name {
                        builder.add_str("x == y,\n");
                    } else {
                        builder.add_str("false,\n");
                    }
                }
                builder.add_str("            },\n");
            }
            builder.add_str("        }\n");
        }
        TypeVariantInfo::Struct(members) => {
            if members.is_empty() {
                builder.add_str("        true\n");
            } else {
                builder.add_str("        lhs.");
                builder.add_modified(members[0].name.clone());
                builder.add_str(" == rhs.");
                builder.add_modified(members[0].name.clone());
                for member in members.iter().skip(1) {
                    builder.add_str(" && lhs.");
                    builder.add_modified(member.name.clone());
                    builder.add_str(" == rhs.");
                    builder.add_modified(member.name.clone());
                }
                builder.add_str("\n");
            }
            builder.add_str("        }\n");
        }
        TypeVariantInfo::Extern => unreachable!(),
    }
    builder.add_str("    }\n");
    builder.add_str("    #[inline(always)]\n");
    builder.add_str("    fn ne(lhs: @");
    info.add_full_typename(builder);
    builder.add_str(", rhs: @");
    info.add_full_typename(builder);
    builder.add_str(") -> bool {\n");
    builder.add_str("        !(lhs == rhs)\n");
    builder.add_str("    }\n");
    builder.add_str("}\n");
}

fn add_serde_impl(builder: &mut PatchBuilder, info: &DeriveInfo) {
    info.add_impl_header(builder, "Serde", &["Serde", "Destruct"]);
    builder.add_str(" {\n");
    builder.add_str("    fn serialize(self: @");
    info.add_full_typename(builder);
    builder.add_str(", ref output: array::Array<felt252>) {\n");
    match &info.specific_info {
        TypeVariantInfo::Enum(variants) => {
            builder.add_str("        match self {\n");
            for (i, variant) in variants.iter().enumerate() {
                builder.add_str("            ");
                builder.add_modified(info.name.clone());
                builder.add_str("::");
                builder.add_modified(variant.name.clone());
                builder.add_modified(RewriteNode::Text(format!(
                    "(x) => {{ serde::Serde::serialize(@{i}, ref output); \
                     serde::Serde::serialize(x, ref output); }},\n"
                )));
            }
            builder.add_str("        }\n");
        }
        TypeVariantInfo::Struct(members) => {
            for member in members {
                builder.add_str("        serde::Serde::serialize(self.");
                builder.add_modified(member.name.clone());
                builder.add_str(", ref output);\n");
            }
        }
        TypeVariantInfo::Extern => unreachable!(),
    }
    builder.add_str("    }\n");
    builder.add_str("    fn deserialize(ref serialized: array::Span<felt252>) -> Option<");
    info.add_full_typename(builder);
    builder.add_str("> {\n");
    match &info.specific_info {
        TypeVariantInfo::Enum(variants) => {
            builder.add_str(
                "        let idx: felt252 = serde::Serde::deserialize(ref serialized)?;\n",
            );
            builder.add_str("        Option::Some(\n");
            for (i, variant) in variants.iter().enumerate() {
                if i == 0 {
                    builder.add_str("            ");
                } else {
                    builder.add_str("            else ");
                }
                builder.add_str(format!("if idx == {i} {{ ").as_str());
                builder.add_modified(info.name.clone());
                builder.add_str("::");
                builder.add_modified(variant.name.clone());
                builder.add_str("(serde::Serde::deserialize(ref serialized)?) }\n");
            }
            builder.add_str("            else { return Option::None; }\n");
            builder.add_str("        )\n");
            builder.add_str("    }\n");
        }
        TypeVariantInfo::Struct(members) => {
            builder.add_str("        Option::Some(");
            builder.add_modified(info.name.clone());
            builder.add_str(" {\n");
            for member in members {
                builder.add_str("            ");
                builder.add_modified(member.name.clone());
                builder.add_str(": serde::Serde::deserialize(ref serialized)?,\n");
            }
            builder.add_str("        })\n");
        }
        TypeVariantInfo::Extern => unreachable!(),
    }
    builder.add_str("    }\n");
    builder.add_str("}\n");
}

fn add_empty_impl(builder: &mut PatchBuilder, info: &DeriveInfo, derived_trait: &str) {
    info.add_impl_header(builder, derived_trait, &[derived_trait]);
    builder.add_str(":\n");
}
