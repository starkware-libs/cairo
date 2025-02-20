use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{
    AttributeList, MemberList, OptionWrappedGenericParamList, VariantList,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GenericParamEx, IsDependentType, QueryAttrs};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use itertools::{Itertools, chain};
use smol_str::SmolStr;

mod clone;
mod debug;
mod default;
mod destruct;
mod hash;
mod panic_destruct;
mod partial_eq;
mod serde;

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct DerivePlugin;

const DERIVE_ATTR: &str = "derive";

impl MacroPlugin for DerivePlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        generate_derive_code_for_type(
            db,
            metadata,
            match item_ast {
                ast::ModuleItem::Struct(struct_ast) => {
                    let generics = GenericParamsInfo::new(db, struct_ast.generic_params(db));
                    let member_info = extract_members(
                        db,
                        struct_ast.members(db),
                        &generics.param_names.iter().map(|p| p.as_str()).collect_vec(),
                    );
                    DeriveInfo::new(
                        db,
                        struct_ast.name(db),
                        struct_ast.attributes(db),
                        generics,
                        member_info,
                        TypeVariant::Struct,
                    )
                }
                ast::ModuleItem::Enum(enum_ast) => {
                    let generics = GenericParamsInfo::new(db, enum_ast.generic_params(db));
                    let member_info = extract_variants(
                        db,
                        enum_ast.variants(db),
                        &generics.param_names.iter().map(|p| p.as_str()).collect_vec(),
                    );
                    DeriveInfo::new(
                        db,
                        enum_ast.name(db),
                        enum_ast.attributes(db),
                        generics,
                        member_info,
                        TypeVariant::Enum,
                    )
                }
                _ => {
                    let maybe_error = item_ast.find_attr(db, DERIVE_ATTR).map(|derive_attr| {
                        vec![PluginDiagnostic::error(
                            derive_attr.as_syntax_node().stable_ptr(),
                            "`derive` may only be applied to `struct`s and `enum`s".to_string(),
                        )]
                    });

                    return PluginResult {
                        diagnostics: maybe_error.unwrap_or_default(),
                        ..PluginResult::default()
                    };
                }
            },
        )
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![DERIVE_ATTR.to_string(), default::DEFAULT_ATTR.to_string()]
    }

    fn declared_derives(&self) -> Vec<String> {
        vec![
            "Copy".to_string(),
            "Drop".to_string(),
            "Clone".to_string(),
            "Debug".to_string(),
            "Default".to_string(),
            "Destruct".to_string(),
            "Hash".to_string(),
            "PanicDestruct".to_string(),
            "PartialEq".to_string(),
            "Serde".to_string(),
        ]
    }
}

/// Information on struct members or enum variants.
struct MemberInfo {
    name: SmolStr,
    ty: String,
    attributes: AttributeList,
    is_generics_dependent: bool,
}
impl MemberInfo {
    fn impl_name(&self, trt: &str) -> String {
        if self.is_generics_dependent {
            let short_name = trt.split("::").last().unwrap_or(trt);
            format!("__MEMBER_IMPL_{}_{short_name}", self.name)
        } else {
            format!("{}::<{}>", trt, self.ty)
        }
    }
    fn drop_with(&self) -> String {
        if self.is_generics_dependent {
            format!("core::internal::DropWith::<{}, {}>", self.ty, self.impl_name("Drop"))
        } else {
            format!("core::internal::Wrapper::<{}>", self.ty)
        }
    }
    fn destruct_with(&self) -> String {
        if self.is_generics_dependent {
            format!("core::internal::DestructWith::<{}, {}>", self.ty, self.impl_name("Destruct"))
        } else {
            format!("core::internal::Wrapper::<{}>", self.ty)
        }
    }
}

/// Information on the type being derived.
enum TypeVariant {
    Enum,
    Struct,
}

/// Information on generic params.
struct GenericParamsInfo {
    /// All the generic param names, at the original order.
    pub param_names: Vec<SmolStr>,
    /// The full genaric params, including keywords and definitions.
    pub full_params: Vec<String>,
}
impl GenericParamsInfo {
    /// Extracts the information on generic params.
    fn new(db: &dyn SyntaxGroup, generic_params: OptionWrappedGenericParamList) -> Self {
        let OptionWrappedGenericParamList::WrappedGenericParamList(gens) = generic_params else {
            return Self { param_names: Default::default(), full_params: Default::default() };
        };
        let params = gens.generic_params(db).elements(db);
        Self {
            param_names: params
                .iter()
                .map(|param| param.name(db).map(|n| n.text(db)).unwrap_or_else(|| "_".into()))
                .collect(),
            full_params: params
                .iter()
                .map(|param| param.as_syntax_node().get_text_without_trivia(db))
                .collect(),
        }
    }
}

/// Information for the type being derived.
pub struct DeriveInfo {
    name: SmolStr,
    attributes: AttributeList,
    generics: GenericParamsInfo,
    members_info: Vec<MemberInfo>,
    type_variant: TypeVariant,
}
impl DeriveInfo {
    /// Extracts the information on the type being derived.
    fn new(
        db: &dyn SyntaxGroup,
        ident: ast::TerminalIdentifier,
        attributes: AttributeList,
        generics: GenericParamsInfo,
        members_info: Vec<MemberInfo>,
        type_variant: TypeVariant,
    ) -> Self {
        Self { name: ident.text(db), attributes, generics, members_info, type_variant }
    }

    /// Returns a full derived impl header - given `derived_trait` - and the `dependent_traits`
    /// required for its all its members.
    fn impl_header(&self, derived_trait: &str, dependent_traits: &[&str]) -> String {
        let derived_trait_name = derived_trait.split("::").last().unwrap_or(derived_trait);
        format!(
            "impl {name}{derived_trait_name}<{generics}> of {derived_trait}::<{full_typename}>",
            name = self.name,
            generics =
                self.impl_generics(dependent_traits, |trt, ty| format!("{trt}<{ty}>")).join(", "),
            full_typename = self.full_typename(),
        )
    }

    /// Returns the expected generics parameters for a derived impl definition.
    ///
    /// `dep_req` - is the formatting of a trait and the type as a concrete trait.
    fn impl_generics(
        &self,
        dependent_traits: &[&str],
        dep_req: fn(&str, &str) -> String,
    ) -> Vec<String> {
        chain!(
            self.generics.full_params.iter().cloned(),
            self.members_info.iter().filter(|m| m.is_generics_dependent).flat_map(|m| {
                dependent_traits
                    .iter()
                    .cloned()
                    .map(move |trt| format!("impl {}: {}", m.impl_name(trt), dep_req(trt, &m.ty)))
            })
        )
        .collect()
    }

    /// Formats the full typename of the type, including generic args.
    fn full_typename(&self) -> String {
        if self.generics.param_names.is_empty() {
            self.name.to_string()
        } else {
            format!("{}<{}>", self.name, self.generics.param_names.iter().join(", "))
        }
    }
}

/// Extracts the information on the members of the struct.
fn extract_members(
    db: &dyn SyntaxGroup,
    members: MemberList,
    generics: &[&str],
) -> Vec<MemberInfo> {
    members
        .elements(db)
        .into_iter()
        .map(|member| MemberInfo {
            name: member.name(db).text(db),
            ty: member.type_clause(db).ty(db).as_syntax_node().get_text_without_trivia(db),
            attributes: member.attributes(db),
            is_generics_dependent: member.type_clause(db).ty(db).is_dependent_type(db, generics),
        })
        .collect()
}

/// Extracts the information on the variants of the enum.
fn extract_variants(
    db: &dyn SyntaxGroup,
    variants: VariantList,
    generics: &[&str],
) -> Vec<MemberInfo> {
    variants
        .elements(db)
        .into_iter()
        .map(|variant| MemberInfo {
            name: variant.name(db).text(db),
            ty: match variant.type_clause(db) {
                ast::OptionTypeClause::Empty(_) => "()".to_string(),
                ast::OptionTypeClause::TypeClause(t) => {
                    t.ty(db).as_syntax_node().get_text_without_trivia(db)
                }
            },
            attributes: variant.attributes(db),
            is_generics_dependent: match variant.type_clause(db) {
                ast::OptionTypeClause::Empty(_) => false,
                ast::OptionTypeClause::TypeClause(t) => t.ty(db).is_dependent_type(db, generics),
            },
        })
        .collect()
}

/// Adds an implementation for all requested derives for the type.
fn generate_derive_code_for_type(
    db: &dyn SyntaxGroup,
    metadata: &MacroPluginMetadata<'_>,
    info: DeriveInfo,
) -> PluginResult {
    let mut diagnostics = vec![];
    let mut builder = PatchBuilder::new(db, &info.attributes);
    for attr in info.attributes.query_attr(db, DERIVE_ATTR) {
        let attr = attr.structurize(db);

        if attr.args.is_empty() {
            diagnostics.push(PluginDiagnostic::error(
                attr.args_stable_ptr.untyped(),
                "Expected args.".into(),
            ));
            continue;
        }

        for arg in attr.args {
            let AttributeArg {
                variant: AttributeArgVariant::Unnamed(ast::Expr::Path(derived_path)),
                ..
            } = arg
            else {
                diagnostics.push(PluginDiagnostic::error(&arg.arg, "Expected path.".into()));
                continue;
            };

            let derived = derived_path.as_syntax_node().get_text_without_trivia(db);
            if let Some(code) = match derived.as_str() {
                "Copy" | "Drop" => Some(get_empty_impl(&derived, &info)),
                "Clone" => Some(clone::handle_clone(&info)),
                "Debug" => Some(debug::handle_debug(&info)),
                "Default" => default::handle_default(db, &info, &derived_path, &mut diagnostics),
                "Destruct" => Some(destruct::handle_destruct(&info)),
                "Hash" => Some(hash::handle_hash(&info)),
                "PanicDestruct" => Some(panic_destruct::handle_panic_destruct(&info)),
                "PartialEq" => Some(partial_eq::handle_partial_eq(&info)),
                "Serde" => Some(serde::handle_serde(&info)),
                _ => {
                    if !metadata.declared_derives.contains(&derived) {
                        diagnostics.push(PluginDiagnostic::error(
                            &derived_path,
                            format!("Unknown derive `{derived}` - a plugin might be missing."),
                        ));
                    }
                    None
                }
            } {
                builder.add_modified(RewriteNode::mapped_text(code, db, &derived_path));
            }
        }
    }
    let (content, code_mappings) = builder.build();
    PluginResult {
        code: (!content.is_empty()).then(|| PluginGeneratedFile {
            name: "impls".into(),
            code_mappings,
            content,
            aux_data: None,
            diagnostics_note: Default::default(),
        }),
        diagnostics,
        remove_original_item: false,
    }
}

fn get_empty_impl(derived_trait: &str, info: &DeriveInfo) -> String {
    let derive_trait = format!("core::traits::{derived_trait}");
    format!("{};\n", info.impl_header(&derive_trait, &[&derive_trait]))
}
