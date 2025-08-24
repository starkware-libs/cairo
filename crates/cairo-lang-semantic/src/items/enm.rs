use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    EnumId, LanguageElementId, LookupItemId, ModuleItemId, VariantId, VariantLongId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_filesystem::ids::StrRef;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, Upcast};

use super::attribute::SemanticQueryAttrs;
use super::generics::{GenericParamsData, semantic_generic_params};
use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter};
use crate::types::{add_type_based_diagnostics, resolve_type};
use crate::{ConcreteEnumId, SemanticDiagnostic, semantic};

#[cfg(test)]
#[path = "enm_test.rs"]
mod test;

// Declaration
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn SemanticGroup)]
pub struct EnumDeclarationData<'db> {
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    generic_params: Vec<semantic::GenericParam<'db>>,
    attributes: Vec<Attribute<'db>>,
    resolver_data: Arc<ResolverData<'db>>,
}

/// Implementation of [crate::db::SemanticGroup::priv_enum_declaration_data].
pub fn priv_enum_declaration_data<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<EnumDeclarationData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let enum_ast = db.module_enum_by_id(enum_id)?;

    // Generic params.
    let generic_params_data = db.enum_generic_params_data(enum_id)?;
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id)));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);
    let generic_params = generic_params_data.generic_params;
    let attributes = enum_ast.attributes(db).structurize(db);

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, enum_ast.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();

    let resolver_data = Arc::new(resolver.data);
    Ok(EnumDeclarationData {
        diagnostics: diagnostics.build(),
        generic_params,
        attributes,
        resolver_data,
    })
}

/// Query implementation of [crate::db::SemanticGroup::priv_enum_declaration_data].
#[salsa::tracked]
pub fn priv_enum_declaration_data_tracked<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<EnumDeclarationData<'db>> {
    priv_enum_declaration_data(db, enum_id)
}

/// Implementation of [crate::db::SemanticGroup::enum_declaration_diagnostics].
pub fn enum_declaration_diagnostics<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_enum_declaration_data(enum_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query Implementation of [crate::db::SemanticGroup::enum_declaration_diagnostics].
#[salsa::tracked]
pub fn enum_declaration_diagnostics_tracked<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    enum_declaration_diagnostics(db, enum_id)
}

/// Implementation of [crate::db::SemanticGroup::enum_generic_params].
pub fn enum_generic_params<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<Vec<semantic::GenericParam<'db>>> {
    Ok(db.enum_generic_params_data(enum_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::enum_generic_params].
#[salsa::tracked]
pub fn enum_generic_params_tracked<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<Vec<semantic::GenericParam<'db>>> {
    enum_generic_params(db, enum_id)
}

/// Implementation of [crate::db::SemanticGroup::enum_generic_params_data].
pub fn enum_generic_params_data<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_file_id = enum_id.module_file_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let enum_ast = db.module_enum_by_id(enum_id)?;

    // Generic params.
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id)));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    resolver.set_feature_config(&enum_id, &enum_ast, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &enum_ast.generic_params(db),
    );
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, enum_ast.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { generic_params, diagnostics: diagnostics.build(), resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::enum_generic_params_data].
#[salsa::tracked]
pub fn enum_generic_params_data_tracked<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    enum_generic_params_data(db, enum_id)
}

/// Implementation of [crate::db::SemanticGroup::enum_attributes].
pub fn enum_attributes<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<Vec<Attribute<'db>>> {
    Ok(db.priv_enum_declaration_data(enum_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::enum_attributes].
#[salsa::tracked]
pub fn enum_attributes_tracked<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<Vec<Attribute<'db>>> {
    enum_attributes(db, enum_id)
}

/// Implementation of [crate::db::SemanticGroup::enum_declaration_resolver_data].
pub fn enum_declaration_resolver_data<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db.priv_enum_declaration_data(enum_id)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::enum_declaration_resolver_data].
#[salsa::tracked]
pub fn enum_declaration_resolver_data_tracked<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    enum_declaration_resolver_data(db, enum_id)
}

// Definition
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn SemanticGroup)]
pub struct EnumDefinitionData<'db> {
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    variants: OrderedHashMap<StrRef<'db>, VariantId<'db>>,
    variant_semantic: OrderedHashMap<VariantId<'db>, Variant<'db>>,
    resolver_data: Arc<ResolverData<'db>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn SemanticGroup)]
pub struct Variant<'db> {
    pub enum_id: EnumId<'db>,
    pub id: VariantId<'db>,
    pub ty: semantic::TypeId<'db>,
    /// The index of the variant from within the variant list.
    pub idx: usize,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, SemanticObject, salsa::Update)]
pub struct ConcreteVariant<'db> {
    pub concrete_enum_id: ConcreteEnumId<'db>,
    pub id: VariantId<'db>,
    pub ty: semantic::TypeId<'db>,
    /// The index of the variant from within the variant list.
    #[dont_rewrite]
    pub idx: usize,
}

/// Selector pattern of a match arm of a match on numeric values.
/// Required for the dont_rewrite attribute to work.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup)]
pub struct ValueSelectorArm {
    #[dont_rewrite]
    pub value: usize,
}

/// Selector pattern of a match arm.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum MatchArmSelector<'db> {
    VariantId(ConcreteVariant<'db>),
    Value(ValueSelectorArm),
}

/// Implementation of [crate::db::SemanticGroup::priv_enum_definition_data].
pub fn priv_enum_definition_data<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<EnumDefinitionData<'db>> {
    let module_file_id = enum_id.module_file_id(db);
    let crate_id = module_file_id.0.owning_crate(db);
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let enum_ast = db.module_enum_by_id(enum_id)?;

    // Generic params.
    let generic_params_data = db.enum_generic_params_data(enum_id)?;
    let inference_id =
        InferenceId::LookupItemDefinition(LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id)));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);

    // Variants.
    let mut variants = OrderedHashMap::default();
    let mut variant_semantic = OrderedHashMap::default();
    for (variant_idx, variant) in enum_ast.variants(db).elements(db).enumerate() {
        let feature_restore =
            resolver.extend_feature_config_from_item(db, crate_id, &mut diagnostics, &variant);
        let id = VariantLongId(module_file_id, variant.stable_ptr(db)).intern(db);
        let ty = match variant.type_clause(db) {
            ast::OptionTypeClause::Empty(_) => unit_ty(db),
            ast::OptionTypeClause::TypeClause(type_clause) => {
                resolve_type(db, &mut diagnostics, &mut resolver, &type_clause.ty(db))
            }
        };
        let variant_name: StrRef<'_> = variant.name(db).text(db).into();
        if let Some(_other_variant) = variants.insert(variant_name, id) {
            diagnostics
                .report(variant.stable_ptr(db), EnumVariantRedefinition { enum_id, variant_name });
        }
        variant_semantic.insert(id, Variant { enum_id, id, ty, idx: variant_idx });
        resolver.restore_feature_config(feature_restore);
    }

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, enum_ast.stable_ptr(db).untyped());

    for (_, variant) in variant_semantic.iter_mut() {
        variant.ty = inference.rewrite(variant.ty).no_err();
    }

    let resolver_data = Arc::new(resolver.data);
    Ok(EnumDefinitionData {
        diagnostics: diagnostics.build(),
        variants,
        variant_semantic,
        resolver_data,
    })
}

/// Query implementation of [crate::db::SemanticGroup::priv_enum_definition_data].
#[salsa::tracked]
pub fn priv_enum_definition_data_tracked<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<EnumDefinitionData<'db>> {
    priv_enum_definition_data(db, enum_id)
}

/// Implementation of [crate::db::SemanticGroup::enum_definition_diagnostics].
pub fn enum_definition_diagnostics<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    let Ok(data) = db.priv_enum_definition_data(enum_id) else {
        return Default::default();
    };

    let crate_id = data.resolver_data.module_file_id.0.owning_crate(db);

    // If the enum is a phantom type, no need to check if its variants are fully valid types, as
    // they won't be used.
    if db
        .declared_phantom_type_attributes(crate_id)
        .iter()
        .any(|attr| enum_id.has_attr(db, attr).unwrap_or_default())
    {
        return data.diagnostics;
    }
    let mut diagnostics = SemanticDiagnostics::from(data.diagnostics);
    for (_, variant) in data.variant_semantic.iter() {
        let stable_ptr = variant.id.stable_ptr(db);
        add_type_based_diagnostics(db, &mut diagnostics, variant.ty, stable_ptr);
        if variant.ty.is_phantom(db) {
            diagnostics.report(stable_ptr, NonPhantomTypeContainingPhantomType);
        }
    }
    diagnostics.build()
}

/// Query implementation of [crate::db::SemanticGroup::enum_definition_diagnostics].
#[salsa::tracked]
pub fn enum_definition_diagnostics_tracked<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    enum_definition_diagnostics(db, enum_id)
}

/// Implementation of [crate::db::SemanticGroup::enum_definition_resolver_data].
pub fn enum_definition_resolver_data<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db.priv_enum_definition_data(enum_id)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::enum_definition_resolver_data].
#[salsa::tracked]
pub fn enum_definition_resolver_data_tracked<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    enum_definition_resolver_data(db, enum_id)
}

/// Implementation of [crate::db::SemanticGroup::enum_variants].
pub fn enum_variants<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<OrderedHashMap<StrRef<'db>, VariantId<'db>>> {
    Ok(db.priv_enum_definition_data(enum_id)?.variants)
}

/// Query implementation of [crate::db::SemanticGroup::enum_variants].
#[salsa::tracked]
pub fn enum_variants_tracked<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
) -> Maybe<OrderedHashMap<StrRef<'db>, VariantId<'db>>> {
    enum_variants(db, enum_id)
}

/// Implementation of [crate::db::SemanticGroup::variant_semantic].
pub fn variant_semantic<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
    variant_id: VariantId<'db>,
) -> Maybe<Variant<'db>> {
    let data = db.priv_enum_definition_data(enum_id)?;
    Ok(data.variant_semantic[&variant_id].clone())
}

/// Query implementation of [crate::db::SemanticGroup::variant_semantic].
#[salsa::tracked]
pub fn variant_semantic_tracked<'db>(
    db: &'db dyn SemanticGroup,
    enum_id: EnumId<'db>,
    variant_id: VariantId<'db>,
) -> Maybe<Variant<'db>> {
    variant_semantic(db, enum_id, variant_id)
}

// TODO(spapini): Consider making these queries.
pub trait SemanticEnumEx: for<'a> Upcast<'a, dyn SemanticGroup> {
    /// Retrieves the [ConcreteVariant] for a [ConcreteEnumId] and a [Variant].
    fn concrete_enum_variant<'db>(
        &'db self,
        concrete_enum_id: ConcreteEnumId<'db>,
        variant: &Variant<'db>,
    ) -> Maybe<ConcreteVariant<'db>> {
        // TODO(spapini): Uphold the invariant that constructed ConcreteEnumId instances
        //   always have the correct number of generic arguments.
        let db = self.upcast();
        let generic_params = db.enum_generic_params(concrete_enum_id.enum_id(db))?;
        let generic_args = concrete_enum_id.long(db).generic_args.clone();
        GenericSubstitution::new(&generic_params, &generic_args).substitute(
            db,
            ConcreteVariant { concrete_enum_id, id: variant.id, ty: variant.ty, idx: variant.idx },
        )
    }

    /// Retrieves all the [ConcreteVariant]s for a [ConcreteEnumId].
    fn concrete_enum_variants<'db>(
        &'db self,
        concrete_enum_id: ConcreteEnumId<'db>,
    ) -> Maybe<Vec<ConcreteVariant<'db>>> {
        // TODO(spapini): Uphold the invariant that constructed ConcreteEnumId instances
        //   always have the correct number of generic arguments.
        let db = self.upcast();
        let enum_id = concrete_enum_id.enum_id(db);
        db.enum_variants(enum_id)?
            .values()
            .map(|variant_id| {
                db.concrete_enum_variant(
                    concrete_enum_id,
                    &db.variant_semantic(enum_id, *variant_id)?,
                )
            })
            .collect()
    }
}

impl<T: for<'db> Upcast<'db, dyn SemanticGroup> + ?Sized> SemanticEnumEx for T {}
