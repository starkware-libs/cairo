use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    EnumId, LanguageElementId, LookupItemId, ModuleItemId, VariantId, VariantLongId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, MaybeAsRef, skip_diagnostic};
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_proc_macros::{DebugWithDb, HeapSize, SemanticObject};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use salsa::Database;

use super::attribute::SemanticQueryAttrs;
use super::generics::{GenericParamsData, semantic_generic_params};
use crate::corelib::unit_ty;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter};
use crate::types::{add_type_based_diagnostics, resolve_type};
use crate::{ConcreteEnumId, GenericParam, SemanticDiagnostic, semantic};

#[cfg(test)]
#[path = "enm_test.rs"]
mod test;

// Declaration
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
struct EnumDeclarationData<'db> {
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    attributes: Vec<Attribute<'db>>,
    resolver_data: Arc<ResolverData<'db>>,
}

/// Returns the declaration data of an enum.
#[salsa::tracked(returns(ref))]
fn enum_declaration_data<'db>(
    db: &'db dyn Database,
    enum_id: EnumId<'db>,
) -> Maybe<EnumDeclarationData<'db>> {
    let mut diagnostics = SemanticDiagnostics::new(enum_id.parent_module(db));
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let enum_ast = db.module_enum_by_id(enum_id)?;

    // Generic params.
    let generic_params_data = enum_generic_params_data(db, enum_id).maybe_as_ref()?;
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id)));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics.clone());
    let attributes = enum_ast.attributes(db).structurize(db);

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, enum_ast.stable_ptr(db).untyped());

    let resolver_data = Arc::new(resolver.data);
    Ok(EnumDeclarationData { diagnostics: diagnostics.build(), attributes, resolver_data })
}

/// Returns the generic parameters data of an enum.
#[salsa::tracked(returns(ref))]
fn enum_generic_params_data<'db>(
    db: &'db dyn Database,
    enum_id: EnumId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_id = enum_id.parent_module(db);
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    let enum_ast = db.module_enum_by_id(enum_id)?;

    // Generic params.
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id)));
    let mut resolver = Resolver::new(db, module_id, inference_id);
    resolver.set_feature_config(&enum_id, &enum_ast, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_id,
        &enum_ast.generic_params(db),
    );
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, enum_ast.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { generic_params, diagnostics: diagnostics.build(), resolver_data })
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
struct EnumDefinitionData<'db> {
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    variants: OrderedHashMap<SmolStrId<'db>, VariantId<'db>>,
    variant_semantic: OrderedHashMap<VariantId<'db>, Variant<'db>>,
    resolver_data: Arc<ResolverData<'db>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
pub struct Variant<'db> {
    pub enum_id: EnumId<'db>,
    pub id: VariantId<'db>,
    pub ty: semantic::TypeId<'db>,
    /// The index of the variant from within the variant list.
    pub idx: usize,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, SemanticObject, HeapSize, salsa::Update)]
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
#[debug_db(dyn Database)]
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

/// Returns the definition data of an enum.
#[salsa::tracked(returns(ref))]
fn enum_definition_data<'db>(
    db: &'db dyn Database,
    enum_id: EnumId<'db>,
) -> Maybe<EnumDefinitionData<'db>> {
    let module_id = enum_id.parent_module(db);
    let crate_id = module_id.owning_crate(db);
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let enum_ast = db.module_enum_by_id(enum_id)?;

    // Generic params.
    let generic_params_data = enum_generic_params_data(db, enum_id).maybe_as_ref()?;
    let inference_id =
        InferenceId::LookupItemDefinition(LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id)));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics.clone());

    // Variants.
    let mut variants = OrderedHashMap::default();
    let mut variant_semantic = OrderedHashMap::default();
    for variant in enum_ast.variants(db).elements(db) {
        let feature_restore =
            resolver.extend_feature_config_from_item(db, crate_id, &mut diagnostics, &variant);
        let id = VariantLongId(module_id, variant.stable_ptr(db)).intern(db);
        let ty = match variant.type_clause(db) {
            ast::OptionTypeClause::Empty(_) => unit_ty(db),
            ast::OptionTypeClause::TypeClause(type_clause) => {
                resolve_type(db, &mut diagnostics, &mut resolver, &type_clause.ty(db))
            }
        };
        let variant_name = variant.name(db).text(db);
        match variants.entry(variant_name) {
            Entry::Vacant(e) => {
                e.insert(id);
                let idx = variant_semantic.len();
                variant_semantic.insert(id, Variant { enum_id, id, ty, idx });
            }
            Entry::Occupied(_) => {
                diagnostics.report(
                    variant.stable_ptr(db),
                    EnumVariantRedefinition { enum_id, variant_name },
                );
            }
        }
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

/// Query implementation of [EnumSemantic::enum_definition_diagnostics].
#[salsa::tracked]
fn enum_definition_diagnostics<'db>(
    db: &'db dyn Database,
    enum_id: EnumId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    let Ok(data) = enum_definition_data(db, enum_id) else {
        return Default::default();
    };

    let crate_id = data.resolver_data.module_id.owning_crate(db);

    // If the enum is a phantom type, no need to check if its variants are fully valid types, as
    // they won't be used.
    if db
        .declared_phantom_type_attributes(crate_id)
        .iter()
        .any(|attr| enum_id.has_attr(db, attr.long(db)).unwrap_or_default())
    {
        return data.diagnostics.clone();
    }
    let mut diagnostics =
        SemanticDiagnostics::from_diagnostics(enum_id.parent_module(db), data.diagnostics.clone());
    for (_, variant) in data.variant_semantic.iter() {
        let stable_ptr = variant.id.stable_ptr(db);
        add_type_based_diagnostics(db, &mut diagnostics, variant.ty, stable_ptr);
        if variant.ty.is_phantom(db) {
            diagnostics.report(stable_ptr, NonPhantomTypeContainingPhantomType);
        }
    }
    diagnostics.build()
}

// TODO(spapini): Consider making these queries.
pub trait SemanticEnumEx: Database {
    /// Retrieves the [ConcreteVariant] for a [ConcreteEnumId] and a [Variant].
    fn concrete_enum_variant<'db>(
        &'db self,
        concrete_enum_id: ConcreteEnumId<'db>,
        variant: &Variant<'db>,
    ) -> Maybe<ConcreteVariant<'db>> {
        // TODO(spapini): Uphold the invariant that constructed ConcreteEnumId instances
        //   always have the correct number of generic arguments.
        let db = self.as_dyn_database();
        let generic_params = db.enum_generic_params(concrete_enum_id.enum_id(db))?;
        let generic_args = &concrete_enum_id.long(db).generic_args;
        GenericSubstitution::new(generic_params, generic_args).substitute(
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
        let db = self.as_dyn_database();
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

impl<T: Database + ?Sized> SemanticEnumEx for T {}

/// Trait for enum-related semantic queries.
pub trait EnumSemantic<'db>: Database {
    /// Returns the diagnostics of an enum declaration.
    fn enum_declaration_diagnostics(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        let db = self.as_dyn_database();
        enum_declaration_data(db, enum_id)
            .as_ref()
            .map(|data| data.diagnostics.clone())
            .unwrap_or_default()
    }
    /// Returns the generic parameters of an enum.
    fn enum_generic_params(&'db self, enum_id: EnumId<'db>) -> Maybe<&'db [GenericParam<'db>]> {
        let db = self.as_dyn_database();
        Ok(&enum_generic_params_data(db, enum_id).maybe_as_ref()?.generic_params)
    }
    /// Returns the attributes attached to an enum.
    fn enum_attributes(&'db self, enum_id: EnumId<'db>) -> Maybe<&'db [Attribute<'db>]> {
        let db = self.as_dyn_database();
        Ok(&enum_declaration_data(db, enum_id).maybe_as_ref()?.attributes)
    }
    /// Returns the resolution resolved_items of an enum declaration.
    fn enum_declaration_resolver_data(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        let db = self.as_dyn_database();
        Ok(enum_declaration_data(db, enum_id).maybe_as_ref()?.resolver_data.clone())
    }
    /// Returns the definition diagnostics of an enum definition.
    fn enum_definition_diagnostics(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        enum_definition_diagnostics(self.as_dyn_database(), enum_id)
    }
    /// Returns the members of an enum.
    fn enum_variants(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<&'db OrderedHashMap<SmolStrId<'db>, VariantId<'db>>> {
        let db = self.as_dyn_database();
        Ok(&enum_definition_data(db, enum_id).maybe_as_ref()?.variants)
    }
    /// Returns the semantic model of a variant.
    fn variant_semantic(
        &'db self,
        enum_id: EnumId<'db>,
        variant_id: VariantId<'db>,
    ) -> Maybe<semantic::Variant<'db>> {
        let db = self.as_dyn_database();
        enum_definition_data(db, enum_id)
            .maybe_as_ref()?
            .variant_semantic
            .get(&variant_id)
            .cloned()
            .ok_or_else(skip_diagnostic)
    }
    /// Returns the resolution resolved_items of an enum definition.
    fn enum_definition_resolver_data(
        &'db self,
        enum_id: EnumId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        let db = self.as_dyn_database();
        Ok(enum_definition_data(db, enum_id).maybe_as_ref()?.resolver_data.clone())
    }
}
impl<'db, T: Database + ?Sized> EnumSemantic<'db> for T {}
