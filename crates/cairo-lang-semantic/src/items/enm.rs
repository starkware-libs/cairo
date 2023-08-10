use std::sync::Arc;

use cairo_lang_defs::ids::{
    EnumId, LanguageElementId, LookupItemId, ModuleItemId, VariantId, VariantLongId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::Upcast;
use itertools::enumerate;
use smol_str::SmolStr;

use super::generics::{semantic_generic_params, GenericParamsData};
use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::InferenceId;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use crate::types::resolve_type;
use crate::{semantic, ConcreteEnumId, SemanticDiagnostic};

#[cfg(test)]
#[path = "enm_test.rs"]
mod test;

// Declaration
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct EnumDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<semantic::GenericParam>,
    attributes: Vec<Attribute>,
    resolver_data: Arc<ResolverData>,
}

/// Query implementation of [crate::db::SemanticGroup::priv_enum_declaration_data].
pub fn priv_enum_declaration_data(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Maybe<EnumDeclarationData> {
    let module_file_id = enum_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let module_enums = db.module_enums(module_file_id.0)?;
    let enum_ast = module_enums.get(&enum_id).to_maybe()?;
    let syntax_db = db.upcast();

    // Generic params.
    let generic_params_data = db.enum_generic_params_data(enum_id)?;
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id)));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);
    let generic_params = generic_params_data.generic_params;
    let attributes = enum_ast.attributes(syntax_db).structurize(syntax_db);

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = resolver.inference().finalize() {
        inference_err
            .report(&mut diagnostics, stable_ptr.unwrap_or(enum_ast.stable_ptr().untyped()));
    }
    let generic_params = resolver.inference().rewrite(generic_params).no_err();

    let resolver_data = Arc::new(resolver.data);
    Ok(EnumDeclarationData {
        diagnostics: diagnostics.build(),
        generic_params,
        attributes,
        resolver_data,
    })
}

/// Query implementation of [crate::db::SemanticGroup::enum_declaration_diagnostics].
pub fn enum_declaration_diagnostics(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_enum_declaration_data(enum_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::enum_generic_params].
pub fn enum_generic_params(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Maybe<Vec<semantic::GenericParam>> {
    Ok(db.enum_generic_params_data(enum_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::enum_generic_params_data].
pub fn enum_generic_params_data(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Maybe<GenericParamsData> {
    let module_file_id = enum_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let module_enums = db.module_enums(module_file_id.0)?;
    let enum_ast = module_enums.get(&enum_id).to_maybe()?;

    // Generic params.
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id)));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &enum_ast.generic_params(db.upcast()),
    )?;
    resolver.inference().finalize().map(|(_, inference_err)| {
        inference_err.report(&mut diagnostics, enum_ast.stable_ptr().untyped())
    });
    let generic_params = resolver.inference().rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { generic_params, diagnostics: diagnostics.build(), resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::enum_attributes].
pub fn enum_attributes(db: &dyn SemanticGroup, enum_id: EnumId) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_enum_declaration_data(enum_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::enum_declaration_resolver_data].
pub fn enum_declaration_resolver_data(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_enum_declaration_data(enum_id)?.resolver_data)
}

// Definition
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct EnumDefinitionData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    variants: OrderedHashMap<SmolStr, VariantId>,
    variant_semantic: OrderedHashMap<VariantId, Variant>,
    resolver_data: Arc<ResolverData>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct Variant {
    pub enum_id: EnumId,
    pub id: VariantId,
    pub ty: semantic::TypeId,
    /// The index of the variant from within the variant list.
    pub idx: usize,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteVariant {
    pub concrete_enum_id: ConcreteEnumId,
    pub id: VariantId,
    pub ty: semantic::TypeId,
    /// The index of the variant from within the variant list.
    #[dont_rewrite]
    pub idx: usize,
}

/// Query implementation of [crate::db::SemanticGroup::priv_enum_definition_data].
pub fn priv_enum_definition_data(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Maybe<EnumDefinitionData> {
    let module_file_id = enum_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let module_enums = db.module_enums(module_file_id.0)?;
    let enum_ast = module_enums.get(&enum_id).to_maybe()?;
    let syntax_db = db.upcast();

    // Generic params.
    let generic_params_data = db.enum_generic_params_data(enum_id)?;
    let inference_id =
        InferenceId::LookupItemDefinition(LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id)));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);

    // Variants.
    let mut variants = OrderedHashMap::default();
    let mut variant_semantic = OrderedHashMap::default();
    for (variant_idx, variant) in enumerate(enum_ast.variants(syntax_db).elements(syntax_db)) {
        let id = db.intern_variant(VariantLongId(module_file_id, variant.stable_ptr()));
        let ty = match variant.type_clause(syntax_db) {
            ast::OptionTypeClause::Empty(_) => unit_ty(db),
            ast::OptionTypeClause::TypeClause(type_clause) => {
                resolve_type(db, &mut diagnostics, &mut resolver, &type_clause.ty(db.upcast()))
            }
        };
        let variant_name = variant.name(syntax_db).text(syntax_db);
        if let Some(_other_variant) = variants.insert(variant_name.clone(), id) {
            diagnostics.report(&variant, EnumVariantRedefinition { enum_id, variant_name });
        }
        variant_semantic.insert(id, Variant { enum_id, id, ty, idx: variant_idx });
    }

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = resolver.inference().finalize() {
        inference_err
            .report(&mut diagnostics, stable_ptr.unwrap_or(enum_ast.stable_ptr().untyped()));
    }
    for (_, variant) in variant_semantic.iter_mut() {
        variant.ty = resolver.inference().rewrite(variant.ty).no_err();
    }

    let resolver_data = Arc::new(resolver.data);
    Ok(EnumDefinitionData {
        diagnostics: diagnostics.build(),
        variants,
        variant_semantic,
        resolver_data,
    })
}

/// Query implementation of [crate::db::SemanticGroup::enum_definition_diagnostics].
pub fn enum_definition_diagnostics(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_enum_definition_data(enum_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::enum_definition_resolver_data].
pub fn enum_definition_resolver_data(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_enum_definition_data(enum_id)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::enum_variants].
pub fn enum_variants(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Maybe<OrderedHashMap<SmolStr, VariantId>> {
    Ok(db.priv_enum_definition_data(enum_id)?.variants)
}

/// Query implementation of [crate::db::SemanticGroup::variant_semantic].
pub fn variant_semantic(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
    variant_id: VariantId,
) -> Maybe<Variant> {
    let data = db.priv_enum_definition_data(enum_id)?;
    data.variant_semantic.get(&variant_id).cloned().to_maybe()
}

// TODO(spapini): Consider making these queries.
pub trait SemanticEnumEx<'a>: Upcast<dyn SemanticGroup + 'a> {
    /// Retrieves the [ConcreteVariant] for a [ConcreteEnumId] and a [Variant].
    fn concrete_enum_variant(
        &self,
        concrete_enum_id: ConcreteEnumId,
        variant: &Variant,
    ) -> Maybe<ConcreteVariant> {
        // TODO(spapini): Uphold the invariant that constructed ConcreteEnumId instances
        //   always have the correct number of generic arguments.
        let db = self.upcast();
        let generic_params = db.enum_generic_params(concrete_enum_id.enum_id(db))?;
        let generic_args = db.lookup_intern_concrete_enum(concrete_enum_id).generic_args;
        let substitution = GenericSubstitution::new(&generic_params, &generic_args);
        SubstitutionRewriter { db, substitution: &substitution }.rewrite(ConcreteVariant {
            concrete_enum_id,
            id: variant.id,
            ty: variant.ty,
            idx: variant.idx,
        })
    }

    /// Retrieves all the [ConcreteVariant]s for a [ConcreteEnumId].
    fn concrete_enum_variants(
        &self,
        concrete_enum_id: ConcreteEnumId,
    ) -> Maybe<Vec<ConcreteVariant>> {
        // TODO(spapini): Uphold the invariant that constructed ConcreteEnumId instances
        //   always have the correct number of generic arguments.
        let db = self.upcast();
        let enum_id = concrete_enum_id.enum_id(db);
        db.enum_variants(enum_id)?
            .into_iter()
            .map(|(_, variant_id)| {
                db.concrete_enum_variant(
                    concrete_enum_id,
                    &db.variant_semantic(enum_id, variant_id)?,
                )
            })
            .collect()
    }
}

impl<'a, T: Upcast<dyn SemanticGroup + 'a> + ?Sized> SemanticEnumEx<'a> for T {}
