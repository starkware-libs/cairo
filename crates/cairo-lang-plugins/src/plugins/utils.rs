use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GenericParamEx, IsDependentType};
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};
use itertools::{Itertools, chain};
use smol_str::SmolStr;

/// Information on struct members or enum variants.
pub struct MemberInfo {
    pub name: SmolStr,
    pub ty: String,
    pub attributes: ast::AttributeList,
    pub is_generics_dependent: bool,
}
impl MemberInfo {
    pub fn impl_name(&self, trt: &str) -> String {
        if self.is_generics_dependent {
            let short_name = trt.split("::").last().unwrap_or(trt);
            format!("__MEMBER_IMPL_{}_{short_name}", self.name)
        } else {
            format!("{}::<{}>", trt, self.ty)
        }
    }
    pub fn drop_with(&self) -> String {
        if self.is_generics_dependent {
            format!("core::internal::DropWith::<{}, {}>", self.ty, self.impl_name("Drop"))
        } else {
            format!("core::internal::InferDrop::<{}>", self.ty)
        }
    }
    pub fn destruct_with(&self) -> String {
        if self.is_generics_dependent {
            format!("core::internal::DestructWith::<{}, {}>", self.ty, self.impl_name("Destruct"))
        } else {
            format!("core::internal::InferDestruct::<{}>", self.ty)
        }
    }
}

/// Information on the type being derived.
pub enum TypeVariant {
    Enum,
    Struct,
}

/// Information on generic params.
pub struct GenericParamsInfo {
    /// All the generic param names, at the original order.
    pub param_names: Vec<SmolStr>,
    /// The full generic params, including keywords and definitions.
    pub full_params: Vec<String>,
}
impl GenericParamsInfo {
    /// Extracts the information on generic params.
    pub fn new(db: &dyn SyntaxGroup, generic_params: ast::OptionWrappedGenericParamList) -> Self {
        let ast::OptionWrappedGenericParamList::WrappedGenericParamList(gens) = generic_params
        else {
            return Self { param_names: Default::default(), full_params: Default::default() };
        };
        let (param_names, full_params) = gens
            .generic_params(db)
            .elements(db)
            .map(|param| {
                let name = param.name(db).map(|n| n.text(db)).unwrap_or_else(|| "_".into());
                let full_param = param.as_syntax_node().get_text_without_trivia(db);
                (name, full_param)
            })
            .unzip();
        Self { param_names, full_params }
    }
}

/// Information for the type being processed by a plugin.
pub struct PluginTypeInfo {
    pub name: SmolStr,
    pub attributes: ast::AttributeList,
    pub generics: GenericParamsInfo,
    pub members_info: Vec<MemberInfo>,
    pub type_variant: TypeVariant,
}
impl PluginTypeInfo {
    /// Extracts the information on the type being derived.
    pub fn new(db: &dyn SyntaxGroup, item_ast: &ast::ModuleItem) -> Option<Self> {
        match item_ast {
            ast::ModuleItem::Struct(struct_ast) => {
                let generics = GenericParamsInfo::new(db, struct_ast.generic_params(db));
                let members_info = extract_members(
                    db,
                    struct_ast.members(db),
                    &generics.param_names.iter().map(|p| p.as_str()).collect_vec(),
                );
                Some(Self {
                    name: struct_ast.name(db).text(db),
                    attributes: struct_ast.attributes(db),
                    generics,
                    members_info,
                    type_variant: TypeVariant::Struct,
                })
            }
            ast::ModuleItem::Enum(enum_ast) => {
                let generics = GenericParamsInfo::new(db, enum_ast.generic_params(db));
                let members_info = extract_variants(
                    db,
                    enum_ast.variants(db),
                    &generics.param_names.iter().map(|p| p.as_str()).collect_vec(),
                );
                Some(Self {
                    name: enum_ast.name(db).text(db),
                    attributes: enum_ast.attributes(db),
                    generics,
                    members_info,
                    type_variant: TypeVariant::Enum,
                })
            }
            _ => None,
        }
    }

    /// Returns a full derived impl header - given `derived_trait` - and the `dependent_traits`
    /// required for its all its members.
    pub fn impl_header(&self, derived_trait: &str, dependent_traits: &[&str]) -> String {
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
    pub fn impl_generics(
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
    pub fn full_typename(&self) -> String {
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
    members: ast::MemberList,
    generics: &[&str],
) -> Vec<MemberInfo> {
    members
        .elements(db)
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
    variants: ast::VariantList,
    generics: &[&str],
) -> Vec<MemberInfo> {
    variants
        .elements(db)
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
