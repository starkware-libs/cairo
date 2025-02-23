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
use cairo_lang_syntax::node::helpers::{GenericParamEx, QueryAttrs};
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
                ast::ModuleItem::Struct(struct_ast) => DeriveInfo::new(
                    db,
                    struct_ast.name(db),
                    struct_ast.attributes(db),
                    struct_ast.generic_params(db),
                    TypeVariantInfo::Struct(extract_members(db, struct_ast.members(db))),
                ),
                ast::ModuleItem::Enum(enum_ast) => DeriveInfo::new(
                    db,
                    enum_ast.name(db),
                    enum_ast.attributes(db),
                    enum_ast.generic_params(db),
                    TypeVariantInfo::Enum(extract_variants(db, enum_ast.variants(db))),
                ),
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
    _ty: String,
    attributes: AttributeList,
}

/// Information on the type being derived.
enum TypeVariantInfo {
    Enum(Vec<MemberInfo>),
    Struct(Vec<MemberInfo>),
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
        if let OptionWrappedGenericParamList::WrappedGenericParamList(gens) = generic_params {
            for param in gens.generic_params(db).elements(db) {
                ordered.push(param.name(db).map(|n| n.text(db)).unwrap_or_else(|| "_".into()));
                if let ast::GenericParam::Type(t) = param {
                    type_generics.push(t.name(db).text(db));
                } else {
                    other_generics.push(param.as_syntax_node().get_text_without_trivia(db));
                }
            }
        }
        Self { ordered, type_generics, other_generics }
    }

    /// Formats the generic params for the type.
    /// `additional_demands` formats the generic type params as additional trait bounds.
    /// Does not print including the `<>`.
    fn format_generics_with_trait_params_only(
        &self,
        additional_demands: impl Fn(&SmolStr) -> Vec<String>,
    ) -> String {
        chain!(
            self.type_generics.iter().map(|s| s.to_string()),
            self.other_generics.iter().cloned(),
            self.type_generics.iter().flat_map(additional_demands)
        )
        .join(", ")
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
            format!("<{}>", self.format_generics_with_trait_params_only(additional_demands))
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
pub struct DeriveInfo {
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
    fn format_impl_header(
        &self,
        derived_trait_module: &str,
        derived_trait_name: &str,
        dependent_traits: &[&str],
    ) -> String {
        format!(
            "impl {name}{derived_trait_name}{generics_impl} of \
             {derived_trait_module}::{derived_trait_name}::<{full_typename}>",
            name = self.name,
            generics_impl = self.generics.format_generics_with_trait(|t| dependent_traits
                .iter()
                .map(|d| format!("+{d}<{t}>"))
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
            attributes: member.attributes(db),
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
            attributes: variant.attributes(db),
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
                "Clone" => clone::handle_clone(&info),
                "Debug" => debug::handle_debug(&info),
                "Default" => default::handle_default(db, &info, &derived_path, &mut diagnostics),
                "Destruct" => destruct::handle_destruct(&info),
                "Hash" => hash::handle_hash(&info),
                "PanicDestruct" => panic_destruct::handle_panic_destruct(&info),
                "PartialEq" => partial_eq::handle_partial_eq(&info),
                "Serde" => serde::handle_serde(&info),
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
    format!(
        "{};\n",
        info.format_impl_header(
            "core::traits",
            derived_trait,
            &[&format!("core::traits::{derived_trait}")]
        )
    )
}
