use cairo_lang_defs::plugin::{MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult};
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{
    AttributeList, MemberList, OptionWrappedGenericParamList, VariantList,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use itertools::{chain, Itertools};
use smol_str::SmolStr;

mod clone;
mod default;
mod destruct;
mod hash;
mod panic_destruct;
mod partial_eq;
mod serde;

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

#[derive(Default)]
pub struct DeriveResult {
    impls: Vec<String>,
    diagnostics: Vec<PluginDiagnostic>,
}

/// Adds an implementation for all requested derives for the type.
fn generate_derive_code_for_type(db: &dyn SyntaxGroup, info: DeriveInfo) -> PluginResult {
    let mut result = DeriveResult::default();
    for attr in info.attributes.query_attr(db, "derive") {
        let attr = attr.structurize(db);

        if attr.args.is_empty() {
            result.diagnostics.push(PluginDiagnostic {
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
                result.diagnostics.push(PluginDiagnostic {
                    stable_ptr: arg.arg_stable_ptr.untyped(),
                    message: "Expected path.".into(),
                });
                continue;
            };

            let [ast::PathSegment::Simple(segment)] = &path.elements(db)[..] else {
                continue;
            };

            let derived = segment.ident(db).text(db);
            let stable_ptr = value_stable_ptr.untyped();
            match derived.as_str() {
                "Copy" | "Drop" => result.impls.push(get_empty_impl(&derived, &info)),
                "Clone" => clone::handle_clone(&info, stable_ptr, &mut result),
                "Default" => default::handle_default(db, &info, stable_ptr, &mut result),
                "Destruct" => destruct::handle_destruct(&info, stable_ptr, &mut result),
                "Hash" => hash::handle_hash(&info, stable_ptr, &mut result),
                "PanicDestruct" => {
                    panic_destruct::handle_panic_destruct(&info, stable_ptr, &mut result)
                }
                "PartialEq" => partial_eq::handle_partial_eq(&info, stable_ptr, &mut result),
                "Serde" => serde::handle_serde(&info, stable_ptr, &mut result),
                _ => {
                    // TODO(spapini): How to allow downstream derives while also
                    //  alerting the user when the derive doesn't exist?
                }
            }
        }
    }
    PluginResult {
        code: if result.impls.is_empty() {
            None
        } else {
            Some(PluginGeneratedFile {
                name: "impls".into(),
                content: result.impls.join(""),
                diagnostics_mappings: Default::default(),
                aux_data: None,
            })
        },
        diagnostics: result.diagnostics,
        remove_original_item: false,
    }
}

fn get_empty_impl(derived_trait: &str, info: &DeriveInfo) -> String {
    format!("{};\n", info.format_impl_header(derived_trait, &[derived_trait]))
}

/// Returns a diagnostic for when a derive is not supported for extern types.
fn unsupported_for_extern_diagnostic(stable_ptr: SyntaxStablePtrId) -> PluginDiagnostic {
    PluginDiagnostic {
        stable_ptr,
        message: "Unsupported trait for derive for extern types.".into(),
    }
}
