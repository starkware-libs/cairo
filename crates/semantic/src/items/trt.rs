use db_utils::define_short_id;
use defs::ids::{GenericParamId, ImplId, LanguageElementId, ModuleId, TraitId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;
use utils::{extract_matches, try_extract_matches, OptionHelper};

use super::attribute::{ast_attributes_to_semantic, Attribute};
use super::enm::SemanticEnumEx;
use super::generics::semantic_generic_params;
use super::strct::SemanticStructEx;
use crate::corelib::{copy_trait, drop_trait};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve_path::{ResolvedConcreteItem, ResolvedGenericItem, Resolver};
use crate::{GenericArgumentId, SemanticDiagnostic, TypeId, TypeLongId};

#[cfg(test)]
#[path = "trt_test.rs"]
mod test;

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteTraitLongId {
    pub trait_id: TraitId,
    pub generic_args: Vec<GenericArgumentId>,
}
define_short_id!(ConcreteTraitId, ConcreteTraitLongId, SemanticGroup, lookup_intern_concrete_trait);

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TraitData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<GenericParamId>,
    attributes: Vec<Attribute>,
}

/// Query implementation of [crate::db::SemanticGroup::trait_semantic_diagnostics].
pub fn trait_semantic_diagnostics(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_trait_semantic_data(trait_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::trait_generic_params].
pub fn trait_generic_params(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Option<Vec<GenericParamId>> {
    Some(db.priv_trait_semantic_data(trait_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::trait_attributes].
pub fn trait_attributes(db: &dyn SemanticGroup, trait_id: TraitId) -> Option<Vec<Attribute>> {
    Some(db.priv_trait_semantic_data(trait_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::priv_trait_semantic_data].
pub fn priv_trait_semantic_data(db: &dyn SemanticGroup, trait_id: TraitId) -> Option<TraitData> {
    // TODO(spapini): When asts are rooted on items, don't query module_data directly. Use a
    // selector.

    let syntax_db = db.upcast();
    let module_id = trait_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    let module_data = db.module_data(module_id)?;
    let trait_ast = module_data.traits.get(&trait_id)?;

    // Generic params.
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_id,
        &trait_ast.generic_params(syntax_db),
    );

    let attributes = ast_attributes_to_semantic(syntax_db, trait_ast.attributes(syntax_db));
    Some(TraitData { attributes, diagnostics: diagnostics.build(), generic_params })
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteImplLongId {
    pub impl_id: ImplId,
    pub generic_args: Vec<GenericArgumentId>,
}
define_short_id!(ConcreteImplId, ConcreteImplLongId, SemanticGroup, lookup_intern_concrete_impl);

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<GenericParamId>,
    /// The concrete trait this impl implements, or None if cannot be resolved.
    concrete_trait: Option<ConcreteTraitId>,
}

/// Query implementation of [crate::db::SemanticGroup::impl_semantic_declaration_diagnostics].
pub fn impl_semantic_declaration_diagnostics(
    db: &dyn SemanticGroup,
    impl_id: ImplId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_declaration_data(impl_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_generic_params].
pub fn impl_generic_params(db: &dyn SemanticGroup, impl_id: ImplId) -> Option<Vec<GenericParamId>> {
    Some(db.priv_impl_declaration_data(impl_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_declaration_data].
pub fn priv_impl_declaration_data(
    db: &dyn SemanticGroup,
    impl_id: ImplId,
) -> Option<ImplDeclarationData> {
    // TODO(spapini): When asts are rooted on items, don't query module_data directly. Use a
    // selector.
    let module_id = impl_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    let module_data = db.module_data(module_id)?;
    let impl_ast = module_data.impls.get(&impl_id)?;
    let syntax_db = db.upcast();

    // Generic params.
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_id,
        &impl_ast.generic_params(syntax_db),
    );
    let mut resolver = Resolver::new(db, module_id, &generic_params);

    let trait_path_syntax = impl_ast.trait_path(syntax_db);
    let concrete_trait = resolver
        .resolve_concrete_path(&mut diagnostics, &trait_path_syntax)
        .and_then(|option_concrete_path| {
            try_extract_matches!(option_concrete_path, ResolvedConcreteItem::Trait)
                .on_none(|| diagnostics.report(&trait_path_syntax, NotATrait))
        });

    Some(ImplDeclarationData { diagnostics: diagnostics.build(), generic_params, concrete_trait })
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplDefinitionData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
}

/// Query implementation of [crate::db::SemanticGroup::impl_semantic_definition_diagnostics].
pub fn impl_semantic_definition_diagnostics(
    db: &dyn SemanticGroup,
    impl_id: ImplId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_definition_data(impl_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_definition_data].
pub fn priv_impl_definition_data(
    db: &dyn SemanticGroup,
    impl_id: ImplId,
) -> Option<ImplDefinitionData> {
    let module_id = impl_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);

    let declaration_data = db.priv_impl_declaration_data(impl_id)?;
    let concrete_trait = declaration_data.concrete_trait?;

    let module_data = db.module_data(module_id)?;
    let impl_ast = module_data.impls.get(&impl_id)?;

    let lookup_context = ImplLookupContext {
        module_id,
        extra_modules: vec![],
        generic_params: declaration_data.generic_params,
    };
    check_special_impls(
        db,
        &mut diagnostics,
        lookup_context,
        concrete_trait,
        impl_ast.stable_ptr().untyped(),
    );

    Some(ImplDefinitionData { diagnostics: diagnostics.build() })
}

/// Handle special cases such as Copy and Drop checking.
fn check_special_impls(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    lookup_context: ImplLookupContext,
    concrete_trait: ConcreteTraitId,
    stable_ptr: SyntaxStablePtrId,
) -> Option<()> {
    let ConcreteTraitLongId { trait_id, generic_args } =
        db.lookup_intern_concrete_trait(concrete_trait);
    let copy = copy_trait(db);
    let drop = drop_trait(db);

    if trait_id == copy {
        let tys = get_inner_types(db, extract_matches!(generic_args[0], GenericArgumentId::Type))?;
        if !tys
            .into_iter()
            .filter_map(|ty| db.type_info(lookup_context.clone(), ty))
            .all(|info| info.duplicatable)
        {
            diagnostics.report_by_ptr(stable_ptr, InvalidCopyTraitImpl);
            return None;
        }
    }
    if trait_id == drop {
        let tys = get_inner_types(db, extract_matches!(generic_args[0], GenericArgumentId::Type))?;
        if !tys
            .into_iter()
            .filter_map(|ty| db.type_info(lookup_context.clone(), ty))
            .all(|info| info.droppable)
        {
            diagnostics.report_by_ptr(stable_ptr, InvalidDropTraitImpl);
            return None;
        }
    }

    Some(())
}

/// Retrieves all the inner types (members of a struct / tuple or variants of an enum).
/// These are the types that are required to implement some trait,
/// in order for the original type to be able to implement this trait.
fn get_inner_types(db: &dyn SemanticGroup, ty: TypeId) -> Option<Vec<TypeId>> {
    Some(match db.lookup_intern_type(ty) {
        TypeLongId::Concrete(concrete_type_id) => {
            // Look for Copy and Drop trait in the defining module.
            match concrete_type_id {
                crate::ConcreteTypeId::Struct(concrete_struct_id) => db
                    .concrete_struct_members(concrete_struct_id)?
                    .values()
                    .map(|member| member.ty)
                    .collect(),
                crate::ConcreteTypeId::Enum(concrete_enum_id) => db
                    .concrete_enum_variants(concrete_enum_id)?
                    .into_iter()
                    .map(|variant| variant.ty)
                    .collect(),
                crate::ConcreteTypeId::Extern(_) => vec![],
            }
        }
        TypeLongId::Tuple(tys) => tys,
        TypeLongId::GenericParameter(_) | TypeLongId::Never | TypeLongId::Missing => {
            return None;
        }
    })
}

/// Query implementation of [crate::db::SemanticGroup::find_impls_at_module].
pub fn find_impls_at_module(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    concrete_trait_id: ConcreteTraitId,
) -> Option<Vec<ConcreteImplId>> {
    let mut res = Vec::new();
    let impls = db.module_data(module_id)?.impls;
    // TODO(spapini): Index better.
    for impl_id in impls.keys().copied() {
        let Some(imp_data)= db.priv_impl_declaration_data(impl_id) else {continue};
        if !imp_data.generic_params.is_empty() {
            // TODO(spapini): Infer generics and substitute.
            continue;
        }

        if imp_data.concrete_trait == Some(concrete_trait_id) {
            let concrete_impl_id =
                db.intern_concrete_impl(ConcreteImplLongId { impl_id, generic_args: vec![] });
            res.push(concrete_impl_id);
        }
    }
    Some(res)
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ImplLookupContext {
    pub module_id: ModuleId,
    pub extra_modules: Vec<ModuleId>,
    pub generic_params: Vec<GenericParamId>,
}

/// Finds all the implementations for a concrete trait, in a specific lookup context.
pub fn find_impls_at_context(
    db: &dyn SemanticGroup,
    lookup_context: &ImplLookupContext,
    concrete_trait_id: ConcreteTraitId,
) -> Option<Vec<ConcreteImplId>> {
    let mut res = Vec::new();
    // TODO(spapini): Lookup in generic_params once impl generic params are supported.
    res.extend(find_impls_at_module(db, lookup_context.module_id, concrete_trait_id)?);
    for module_id in &lookup_context.extra_modules {
        if let Some(imps) = find_impls_at_module(db, *module_id, concrete_trait_id) {
            res.extend(imps);
        }
    }
    for submodule in db.module_submodules(lookup_context.module_id)? {
        res.extend(find_impls_at_module(db, submodule, concrete_trait_id)?);
    }
    for use_id in db.module_data(lookup_context.module_id)?.uses.keys() {
        if let Some(ResolvedGenericItem::Module(submodule)) = db.use_resolved_item(*use_id) {
            res.extend(find_impls_at_module(db, submodule, concrete_trait_id)?);
        }
    }
    Some(res)
}
