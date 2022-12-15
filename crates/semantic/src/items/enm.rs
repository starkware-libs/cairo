use std::sync::Arc;

use db_utils::Upcast;
use defs::ids::{EnumId, GenericParamId, LanguageElementId, VariantId, VariantLongId};
use diagnostics::{Diagnostics, Maybe, ToMaybe};
use diagnostics_proc_macros::DebugWithDb;
use itertools::enumerate;
use smol_str::SmolStr;
use syntax::node::{Terminal, TypedSyntaxNode};
use utils::ordered_hash_map::OrderedHashMap;

use super::generics::semantic_generic_params;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve_path::{ResolvedLookback, Resolver};
use crate::types::{resolve_type, substitute_generics};
use crate::{semantic, ConcreteEnumId, SemanticDiagnostic};

#[cfg(test)]
#[path = "enm_test.rs"]
mod test;

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct EnumData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<GenericParamId>,
    variants: OrderedHashMap<SmolStr, VariantId>,
    variant_semantic: OrderedHashMap<VariantId, Variant>,
    resolved_lookback: Arc<ResolvedLookback>,
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

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteVariant {
    pub concrete_enum_id: ConcreteEnumId,
    pub id: VariantId,
    pub ty: semantic::TypeId,
    /// The index of the variant from within the variant list.
    pub idx: usize,
}

/// Query implementation of [crate::db::SemanticGroup::enum_semantic_diagnostics].
pub fn enum_semantic_diagnostics(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_enum_semantic_data(enum_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::enum_generic_params].
pub fn enum_generic_params(db: &dyn SemanticGroup, enum_id: EnumId) -> Maybe<Vec<GenericParamId>> {
    Ok(db.priv_enum_semantic_data(enum_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::enum_variants].
pub fn enum_variants(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Maybe<OrderedHashMap<SmolStr, VariantId>> {
    Ok(db.priv_enum_semantic_data(enum_id)?.variants)
}

/// Query implementation of [crate::db::SemanticGroup::variant_semantic].
pub fn variant_semantic(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
    variant_id: VariantId,
) -> Maybe<Variant> {
    let data = db.priv_enum_semantic_data(enum_id)?;
    data.variant_semantic.get(&variant_id).cloned().to_maybe()
}

/// Query implementation of [crate::db::SemanticGroup::enum_resolved_lookback].
pub fn enum_resolved_lookback(
    db: &dyn SemanticGroup,
    enum_id: EnumId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db.priv_enum_semantic_data(enum_id)?.resolved_lookback)
}

/// Query implementation of [crate::db::SemanticGroup::priv_enum_semantic_data].
pub fn priv_enum_semantic_data(db: &dyn SemanticGroup, enum_id: EnumId) -> Maybe<EnumData> {
    // TODO(spapini): When asts are rooted on items, don't query module_data directly. Use a
    // selector.
    let module_file_id = enum_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let module_data = db.module_data(module_file_id.0)?;
    let enum_ast = module_data.enums.get(&enum_id).to_maybe()?;
    let syntax_db = db.upcast();

    // Generic params.
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_file_id,
        &enum_ast.generic_params(db.upcast()),
    );
    let mut resolver = Resolver::new(db, module_file_id, &generic_params);

    // Variants.
    let mut variants = OrderedHashMap::default();
    let mut variant_semantic = OrderedHashMap::default();
    for (variant_idx, variant) in enumerate(enum_ast.variants(syntax_db).elements(syntax_db)) {
        let id = db.intern_variant(VariantLongId(module_file_id, variant.stable_ptr()));
        let ty = resolve_type(
            db,
            &mut diagnostics,
            &mut resolver,
            &variant.type_clause(syntax_db).ty(syntax_db),
        );
        let variant_name = variant.name(syntax_db).text(syntax_db);
        if let Some(_other_variant) = variants.insert(variant_name.clone(), id) {
            diagnostics.report(&variant, EnumVariantRedefinition { enum_id, variant_name });
        }
        variant_semantic.insert(id, Variant { enum_id, id, ty, idx: variant_idx });
    }

    let resolved_lookback = Arc::new(resolver.lookback);

    Ok(EnumData {
        diagnostics: diagnostics.build(),
        generic_params,
        variants,
        variant_semantic,
        resolved_lookback,
    })
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
        let substitution = &generic_params.into_iter().zip(generic_args.into_iter()).collect();

        let ty = substitute_generics(db, substitution, variant.ty);
        Ok(ConcreteVariant { concrete_enum_id, id: variant.id, ty, idx: variant.idx })
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
