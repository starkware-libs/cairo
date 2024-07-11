use std::sync::Arc;

use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{
    AttributeList, MemberList, OptionWrappedGenericParamList, VariantList,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GenericParamEx, QueryAttrs};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::LookupIntern;
use itertools::{chain, Itertools};

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
                    struct_ast.as_syntax_node().span(db),
                ),
                ast::ModuleItem::Enum(enum_ast) => DeriveInfo::new(
                    db,
                    enum_ast.name(db),
                    enum_ast.attributes(db),
                    enum_ast.generic_params(db),
                    TypeVariantInfo::Enum(extract_variants(db, enum_ast.variants(db))),
                    enum_ast.as_syntax_node().span(db),
                ),
                ast::ModuleItem::ExternType(extern_type_ast) => DeriveInfo::new(
                    db,
                    extern_type_ast.name(db),
                    extern_type_ast.attributes(db),
                    extern_type_ast.generic_params(db),
                    TypeVariantInfo::Extern,
                    extern_type_ast.as_syntax_node().span(db),
                ),
                _ => return PluginResult::default(),
            },
        )
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![DERIVE_ATTR.to_string(), default::DEFAULT_ATTR.to_string()]
    }
}

/// Information on struct members or enum variants.
struct MemberInfo {
    name: Arc<str>,
    _ty: String,
    attributes: AttributeList,
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
    ordered: Vec<Arc<str>>,
    /// The generic params name that are types.
    type_generics: Vec<Arc<str>>,
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
                ordered.push(
                    param
                        .name(db)
                        .map(|n| n.text(db).lookup_intern(db))
                        .unwrap_or_else(|| "_".into()),
                );
                if let ast::GenericParam::Type(t) = param {
                    type_generics.push(t.name(db).text(db).lookup_intern(db));
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
        additional_demands: impl Fn(&str) -> Vec<Arc<str>>,
    ) -> String {
        chain!(
            self.type_generics.iter().cloned(),
            self.other_generics.iter().map(|s| s.as_str().into()),
            self.type_generics.iter().flat_map(|s| additional_demands(s))
        )
        .join(", ")
    }

    /// Formats the generic params for the type.
    /// `additional_demands` formats the generic type params as additional trait bounds.
    fn format_generics_with_trait(
        &self,
        additional_demands: impl Fn(&str) -> Vec<Arc<str>>,
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
    name: Arc<str>,
    attributes: AttributeList,
    generics: GenericParamsInfo,
    specific_info: TypeVariantInfo,
    span: TextSpan,
}
impl DeriveInfo {
    /// Extracts the information on the type being derived.
    fn new(
        db: &dyn SyntaxGroup,
        ident: ast::TerminalIdentifier,
        attributes: AttributeList,
        generic_args: OptionWrappedGenericParamList,
        specific_info: TypeVariantInfo,
        span: TextSpan,
    ) -> Self {
        Self {
            name: ident.text(db).lookup_intern(db),
            attributes,
            generics: GenericParamsInfo::new(db, generic_args),
            specific_info,
            span,
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
                .map(|d| format!("+{d}<{t}>").into())
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
            name: member.name(db).text(db).lookup_intern(db),
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
            name: variant.name(db).text(db).lookup_intern(db),
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

#[derive(Default)]
pub struct DeriveResult {
    impls: Vec<String>,
    diagnostics: Vec<PluginDiagnostic>,
}

/// Adds an implementation for all requested derives for the type.
fn generate_derive_code_for_type(
    db: &dyn SyntaxGroup,
    metadata: &MacroPluginMetadata<'_>,
    info: DeriveInfo,
) -> PluginResult {
    let mut result = DeriveResult::default();
    for attr in info.attributes.query_attr(db, DERIVE_ATTR) {
        let attr = attr.structurize(db);

        if attr.args.is_empty() {
            result.diagnostics.push(PluginDiagnostic::error(
                attr.args_stable_ptr.untyped(),
                "Expected args.".into(),
            ));
            continue;
        }

        for arg in attr.args {
            let AttributeArg {
                variant: AttributeArgVariant::Unnamed(ast::Expr::Path(path)), ..
            } = arg
            else {
                result.diagnostics.push(PluginDiagnostic::error(&arg.arg, "Expected path.".into()));
                continue;
            };

            let derived = path.as_syntax_node().get_text_without_trivia(db);
            let stable_ptr = path.stable_ptr().untyped();
            match derived.as_str() {
                "Copy" | "Drop" => result.impls.push(get_empty_impl(&derived, &info)),
                "Clone" => clone::handle_clone(&info, stable_ptr, &mut result),
                "Debug" => debug::handle_debug(&info, stable_ptr, &mut result),
                "Default" => default::handle_default(db, &info, stable_ptr, &mut result),
                "Destruct" => destruct::handle_destruct(&info, stable_ptr, &mut result),
                "Hash" => hash::handle_hash(&info, stable_ptr, &mut result),
                "PanicDestruct" => {
                    panic_destruct::handle_panic_destruct(&info, stable_ptr, &mut result)
                }
                "PartialEq" => partial_eq::handle_partial_eq(&info, stable_ptr, &mut result),
                "Serde" => serde::handle_serde(&info, stable_ptr, &mut result),
                _ => {
                    if !metadata.declared_derives.contains(&derived) {
                        result.diagnostics.push(PluginDiagnostic::error(
                            stable_ptr,
                            format!("Unknown derive `{derived}` - a plugin might be missing."),
                        ));
                    }
                }
            }
        }
    }
    PluginResult {
        code: if result.impls.is_empty() {
            None
        } else {
            let content = result.impls.join("");
            Some(PluginGeneratedFile {
                name: "impls".into(),
                code_mappings: vec![CodeMapping {
                    origin: CodeOrigin::Span(info.span),
                    span: TextSpan {
                        start: TextOffset::default(),
                        end: TextOffset::default().add_width(TextWidth::from_str(&content)),
                    },
                }],
                content,
                aux_data: None,
            })
        },
        diagnostics: result.diagnostics,
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

/// Returns a diagnostic for when a derive is not supported for extern types.
fn unsupported_for_extern_diagnostic(stable_ptr: SyntaxStablePtrId) -> PluginDiagnostic {
    PluginDiagnostic::error(stable_ptr, "Unsupported trait for derive for extern types.".into())
}
