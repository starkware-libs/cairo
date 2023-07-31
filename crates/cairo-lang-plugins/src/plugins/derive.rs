use cairo_lang_defs::patcher::{Patch, PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{
    AttributeList, MemberList, OptionWrappedGenericParamList, TerminalIdentifier, VariantList,
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
        let mut children = vec![];
        if !self.ordered.is_empty() {
            children.push("<".into());
            add_with_seperator(
                &mut children,
                chain!(
                    self.type_generics.iter().cloned(),
                    self.other_generics.iter().cloned(),
                    self.type_generics.iter().flat_map(|t| additional_demands(t)),
                ),
                ", ",
            );
            children.push(">".into());
        }
        RewriteNode::new_modified(children)
    }

    /// Returns a node for the generics of a type.
    fn generics_part(&self) -> RewriteNode {
        let mut children = vec![];
        if !self.ordered.is_empty() {
            children.push("<".into());
            add_with_seperator(&mut children, self.ordered.iter().cloned(), ", ");
            children.push(">".into());
        }
        RewriteNode::new_modified(children)
    }
}

fn add_with_seperator(
    children: &mut Vec<RewriteNode>,
    mut iter: impl Iterator<Item = RewriteNode>,
    seperator: &str,
) {
    let Some(first) = iter.next() else {
        return;
    };
    children.push(first);
    for node in iter {
        children.push(seperator.into());
        children.push(node);
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

    /// Returns a node for the impl header.
    fn impl_header(
        &self,
        derived_trait: &TerminalIdentifier,
        dependent_traits: &[RewriteNode],
    ) -> RewriteNode {
        RewriteNode::interpolate_patched(
            "impl $name$$derived_trait$$generics_with_trait$ of $derived_trait$<$full_typename$>",
            [
                ("name".into(), self.name.clone()),
                ("derived_trait".into(), RewriteNode::from_ast(derived_trait)),
                ("full_typename".into(), self.full_typename()),
                ("generics".into(), RewriteNode::from_ast(derived_trait)),
                (
                    "generics_with_trait".into(),
                    self.generics.generics_with_trait_part(|t| {
                        dependent_traits
                            .iter()
                            .map(|d| {
                                RewriteNode::interpolate_patched(
                                    "impl $t$$d$: $d$<$t$>",
                                    [("t".into(), t.clone()), ("d".into(), d.clone())].into(),
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

/// Derive result.
struct DeriveResult {
    /// The code to be inserted.
    code: Option<RewriteNode>,
    /// The diagnostics to be reported.
    diagnostics: Vec<PluginDiagnostic>,
    /// Auxiliary information.
    aux_data: Option<DynGeneratedFileAuxData>,
}
impl DeriveResult {
    fn from_diagnostic(diagnostic: PluginDiagnostic) -> Self {
        Self { code: None, diagnostics: vec![diagnostic], aux_data: None }
    }
    fn unsupported_for_extern<T: TypedSyntaxNode>(node: &T) -> Self {
        Self::from_diagnostic(PluginDiagnostic {
            stable_ptr: node.as_syntax_node().stable_ptr(),
            message: "Unsupported trait for derive for extern types.".into(),
        })
    }
    fn from_node(node: RewriteNode) -> Self {
        Self { code: Some(node), diagnostics: vec![], aux_data: None }
    }
}

/// Adds an implementation for all requested derives for the type.
fn generate_derive_code_for_type(db: &dyn SyntaxGroup, info: DeriveInfo) -> PluginResult {
    let mut diagnostics = vec![];
    let mut builder = PatchBuilder::new(db);
    let mut aux_data = None;
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
                variant: AttributeArgVariant::Unnamed { value: ast::Expr::Path(path), .. },
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

            let derived = segment.ident(db);
            let derive_result = match derived.text(db).as_str() {
                "Copy" | "Drop" => empty_impl(&info, &derived),
                "Clone" => clone_impl(&info, &derived),
                "Destruct" => destruct_impl(&info, &derived),
                "PanicDestruct" => panic_destruct_impl(&info, &derived),
                "PartialEq" => partial_eq_impl(&info, &derived),
                "Serde" => serde_impl(&info, &derived),
                _ => {
                    // TODO(spapini): How to allow downstream derives while also
                    //  alerting the user when the derive doesn't exist?
                    continue;
                }
            };
            if let Some(code) = derive_result.code {
                let start = TextOffset::default().add_width(TextWidth::from_str(&builder.code));
                builder.add_modified(code);
                let end = TextOffset::default().add_width(TextWidth::from_str(&builder.code));
                // Adding a patch to point to the derive causing the error over all the generated
                // code. Note we add it later than the node, this way it will only
                // happen after patches provided by the specific derive.
                builder.patches.patches.push(Patch {
                    span: TextSpan { start, end },
                    origin_span: derived.as_syntax_node().span(db),
                });
            }
            diagnostics.extend(derive_result.diagnostics);
            aux_data = aux_data.or(derive_result.aux_data);
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
                aux_data,
            })
        },
        diagnostics,
        remove_original_item: false,
    }
}

fn clone_impl(info: &DeriveInfo, derived: &TerminalIdentifier) -> DeriveResult {
    DeriveResult::from_node(RewriteNode::interpolate_patched(
        indoc! {"
            $header$ {
                fn clone(self: @$full_typename$) -> $full_typename$ {
                    $body$
                }
            }
        "},
        [
            (
                "header".into(),
                info.impl_header(
                    derived,
                    &[RewriteNode::from_ast(derived), RewriteNode::from("Destruct")],
                ),
            ),
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
                    TypeVariantInfo::Extern => {
                        return DeriveResult::unsupported_for_extern(derived);
                    }
                },
            ),
        ]
        .into(),
    ))
}

fn destruct_impl(info: &DeriveInfo, derived: &TerminalIdentifier) -> DeriveResult {
    DeriveResult::from_node(RewriteNode::interpolate_patched(
        indoc! {"
            $header$ {
                fn destruct(self: $full_typename$) nopanic {
                    $body$
                }
            }
        "},
        [
            ("header".into(), info.impl_header(derived, &[RewriteNode::from_ast(derived)])),
            ("full_typename".into(), info.full_typename()),
            (
                "body".into(),
                match &info.specific_info {
                    TypeVariantInfo::Enum(variants) => {
                        let mut children = vec!["match self {\n".into()];
                        for variant in variants {
                            children.push(RewriteNode::interpolate_patched(
                                "            $name$::$variant_name$(x) => \
                                 traits::Destruct::destruct(x),\n",
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
                        add_with_seperator(
                            &mut children,
                            members.iter().map(|member| {
                                RewriteNode::interpolate_patched(
                                    "traits::Destruct::destruct(self.$member_name$);",
                                    [("member_name".into(), member.name.clone())].into(),
                                )
                            }),
                            "\n        ",
                        );
                        RewriteNode::new_modified(children)
                    }
                    TypeVariantInfo::Extern => {
                        return DeriveResult::unsupported_for_extern(derived);
                    }
                },
            ),
        ]
        .into(),
    ))
}

fn panic_destruct_impl(info: &DeriveInfo, derived: &TerminalIdentifier) -> DeriveResult {
    DeriveResult::from_node(RewriteNode::interpolate_patched(
        indoc! {"
            $header$ {
                fn panic_destruct(self: $full_typename$, ref panic: Panic) nopanic {
                    $body$
                }
            }
        "},
        [
            ("header".into(), info.impl_header(derived, &[RewriteNode::from_ast(derived)])),
            ("full_typename".into(), info.full_typename()),
            (
                "body".into(),
                match &info.specific_info {
                    TypeVariantInfo::Enum(variants) => {
                        let mut children = vec!["match self {\n".into()];
                        for variant in variants {
                            children.push(RewriteNode::interpolate_patched(
                                "            $name$::$variant_name$(x) => \
                                 traits::PanicDestruct::panic_destruct(x, ref panic),\n",
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
                        add_with_seperator(
                            &mut children,
                            members.iter().map(|member| {
                                RewriteNode::interpolate_patched(
                                    "traits::PanicDestruct::panic_destruct(self.$member_name$, \
                                     ref panic);",
                                    [("member_name".into(), member.name.clone())].into(),
                                )
                            }),
                            "\n        ",
                        );
                        RewriteNode::new_modified(children)
                    }
                    TypeVariantInfo::Extern => {
                        return DeriveResult::unsupported_for_extern(derived);
                    }
                },
            ),
        ]
        .into(),
    ))
}

fn partial_eq_impl(info: &DeriveInfo, derived: &TerminalIdentifier) -> DeriveResult {
    DeriveResult::from_node(RewriteNode::interpolate_patched(
        indoc! {"
            $header$ {
                fn eq(lhs: @$full_typename$, rhs: @$full_typename$) -> bool {
                    $body$
                }
                #[inline(always)]
                fn ne(lhs: @$full_typename$, rhs: @$full_typename$) -> bool {
                    !(lhs == rhs)
                }
            }
        "},
        [
            ("header".into(), info.impl_header(derived, &[RewriteNode::from_ast(derived)])),
            ("full_typename".into(), info.full_typename()),
            (
                "body".into(),
                match &info.specific_info {
                    TypeVariantInfo::Enum(variants) => {
                        let mut children = vec!["match lhs {\n".into()];
                        for (i, lhs_variant) in variants.iter().enumerate() {
                            children.push(RewriteNode::interpolate_patched(
                                "            $name$::$variant_name$(x) => match rhs {\n",
                                [
                                    ("name".into(), info.name.clone()),
                                    ("variant_name".into(), lhs_variant.name.clone()),
                                ]
                                .into(),
                            ));
                            for (j, rhs_variant) in variants.iter().enumerate() {
                                children.push(RewriteNode::interpolate_patched(
                                    "                $name$::$variant_name$(y) => ",
                                    [
                                        ("name".into(), info.name.clone()),
                                        ("variant_name".into(), rhs_variant.name.clone()),
                                    ]
                                    .into(),
                                ));
                                children.push(if i == j { "x == y,\n" } else { "false,\n" }.into());
                            }
                            children.push("            },\n".into());
                        }
                        children.push("        }".into());
                        RewriteNode::new_modified(children)
                    }
                    TypeVariantInfo::Struct(members) => {
                        if members.is_empty() {
                            RewriteNode::from("true")
                        } else {
                            let mut children = vec![];
                            add_with_seperator(
                                &mut children,
                                members.iter().map(|member| {
                                    RewriteNode::interpolate_patched(
                                        "lhs.$member_name$ == rhs.$member_name$",
                                        [("member_name".into(), member.name.clone())].into(),
                                    )
                                }),
                                " && ",
                            );
                            RewriteNode::new_modified(children)
                        }
                    }
                    TypeVariantInfo::Extern => {
                        return DeriveResult::unsupported_for_extern(derived);
                    }
                },
            ),
        ]
        .into(),
    ))
}

fn serde_impl(info: &DeriveInfo, derived: &TerminalIdentifier) -> DeriveResult {
    DeriveResult::from_node(RewriteNode::interpolate_patched(
        indoc! {"
            $header$ {
                fn serialize(self: @$full_typename$, ref output: array::Array<felt252>) {
                    $serialize_body$
                }
                fn deserialize(ref serialized: array::Span<felt252>) -> Option<$full_typename$> {
                    $deserialize_body$
                }
            }
        "},
        [
            (
                "header".into(),
                info.impl_header(
                    derived,
                    &[RewriteNode::from_ast(derived), RewriteNode::from("Destruct")],
                ),
            ),
            ("full_typename".into(), info.full_typename()),
            (
                "serialize_body".into(),
                match &info.specific_info {
                    TypeVariantInfo::Enum(variants) => {
                        let mut children = vec!["match self {\n".into()];
                        for (i, variant) in variants.iter().enumerate() {
                            children.push(RewriteNode::interpolate_patched(
                                "            $name$::$variant_name$(x) => { \
                                 serde::Serde::serialize(@$idx$, ref output); \
                                 serde::Serde::serialize(x, ref output); },\n",
                                [
                                    ("name".into(), info.name.clone()),
                                    ("idx".into(), RewriteNode::Text(i.to_string())),
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
                        add_with_seperator(
                            &mut children,
                            members.iter().map(|member| {
                                RewriteNode::interpolate_patched(
                                    "serde::Serde::serialize(self.$member_name$, ref output)",
                                    [("member_name".into(), member.name.clone())].into(),
                                )
                            }),
                            ";\n        ",
                        );
                        RewriteNode::new_modified(children)
                    }
                    TypeVariantInfo::Extern => {
                        return DeriveResult::unsupported_for_extern(derived);
                    }
                },
            ),
            (
                "deserialize_body".into(),
                match &info.specific_info {
                    TypeVariantInfo::Enum(variants) => {
                        let mut children = vec![
                                    "let idx: felt252 = serde::Serde::deserialize(ref \
                                     serialized)?;\n        Option::Some(\n            "
                                        .into(),
                                ];
                        for (i, variant) in variants.iter().enumerate() {
                            children.push(RewriteNode::interpolate_patched(
                                "if idx == $idx$ { \
                                 $name$::$variant_name$(serde::Serde::deserialize(ref \
                                 serialized)?) }\n            else ",
                                [
                                    ("name".into(), info.name.clone()),
                                    ("idx".into(), RewriteNode::Text(i.to_string())),
                                    ("variant_name".into(), variant.name.clone()),
                                ]
                                .into(),
                            ));
                        }
                        children.push("{ return Option::None; }\n        )".into());
                        RewriteNode::new_modified(children)
                    }
                    TypeVariantInfo::Struct(members) => {
                        let mut children =
                            vec!["Option::Some(".into(), info.name.clone(), " {\n".into()];
                        for member in members {
                            children.push(RewriteNode::interpolate_patched(
                                "            $member_name$: serde::Serde::deserialize(ref \
                                 serialized)?,\n",
                                [("member_name".into(), member.name.clone())].into(),
                            ));
                        }
                        children.push("        })".into());
                        RewriteNode::new_modified(children)
                    }
                    TypeVariantInfo::Extern => {
                        return DeriveResult::unsupported_for_extern(derived);
                    }
                },
            ),
        ]
        .into(),
    ))
}

fn empty_impl(info: &DeriveInfo, derived: &TerminalIdentifier) -> DeriveResult {
    DeriveResult::from_node(RewriteNode::new_modified(vec![
        info.impl_header(derived, &[RewriteNode::from_ast(derived)]),
        ";\n".into(),
    ]))
}
