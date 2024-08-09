use std::collections::BTreeSet;
use std::sync::Arc;
use std::{mem, panic, vec};

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    FunctionTitleId, GenericKind, GenericParamId, ImplAliasId, ImplConstantDefId,
    ImplConstantDefLongId, ImplDefId, ImplFunctionId, ImplFunctionLongId, ImplImplDefId,
    ImplImplDefLongId, ImplItemId, ImplTypeDefId, ImplTypeDefLongId, LanguageElementId,
    LookupItemId, ModuleId, ModuleItemId, NamedLanguageElementId, NamedLanguageElementLongId,
    TopLevelLanguageElementId, TraitConstantId, TraitFunctionId, TraitId, TraitImplId, TraitTypeId,
};
use cairo_lang_diagnostics::{
    skip_diagnostic, DiagnosticAdded, Diagnostics, DiagnosticsBuilder, Maybe, ToMaybe, ToOption,
};
use cairo_lang_filesystem::ids::UnstableSalsaId;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::ast::OptionTypeClause;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{
    define_short_id, extract_matches, try_extract_matches, Intern, LookupIntern,
};
use itertools::{izip, Itertools};
use smol_str::SmolStr;
use syntax::attribute::structured::{Attribute, AttributeListStructurize};
use syntax::node::ast::{self, GenericArg, ImplItem, MaybeImplBody, OptionReturnTypeClause};
use syntax::node::db::SyntaxGroup;
use syntax::node::helpers::OptionWrappedGenericParamListHelper;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode};

use super::constant::{
    constant_semantic_data_cycle_helper, constant_semantic_data_helper, ConstValue, ConstValueId,
    ConstantData, ImplConstantId,
};
use super::enm::SemanticEnumEx;
use super::function_with_body::{get_inline_config, FunctionBody, FunctionBodyData};
use super::functions::{
    forbid_inline_always_with_impl_generic_param, FunctionDeclarationData, GenericFunctionId,
    ImplGenericFunctionId, InlineConfiguration,
};
use super::generics::{
    generic_params_to_args, semantic_generic_params, GenericArgumentHead, GenericParamsData,
};
use super::impl_alias::{
    impl_alias_generic_params_data_helper, impl_alias_semantic_data_cycle_helper,
    impl_alias_semantic_data_helper, ImplAliasData,
};
use super::structure::SemanticStructEx;
use super::trt::{
    ConcreteTraitConstantId, ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId,
};
use super::type_aliases::{
    type_alias_generic_params_data_helper, type_alias_semantic_data_cycle_helper,
    type_alias_semantic_data_helper, TypeAliasData,
};
use super::{resolve_trait_path, TraitOrImplContext};
use crate::corelib::{
    copy_trait, core_submodule, deref_trait, drop_trait, get_core_trait, CoreTraitContext,
};
use crate::db::{get_resolver_data_options, SemanticGroup};
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::{compute_root_expr, ComputationContext, ContextFunction, Environment};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::conform::InferenceConform;
use crate::expr::inference::infers::InferenceEmbeddings;
use crate::expr::inference::solver::SolutionSet;
use crate::expr::inference::{ImplVarId, Inference, InferenceError, InferenceId};
use crate::items::function_with_body::get_implicit_precedence;
use crate::items::functions::ImplicitPrecedence;
use crate::items::us::SemanticUseEx;
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use crate::types::{add_type_based_diagnostics, get_impl_at_context, resolve_type, ImplTypeId};
use crate::{
    semantic, semantic_object_for_id, Arenas, ConcreteFunction, ConcreteTraitId,
    ConcreteTraitLongId, FunctionId, FunctionLongId, GenericArgumentId, GenericParam, Mutability,
    SemanticDiagnostic, TypeId, TypeLongId,
};

#[cfg(test)]
#[path = "imp_test.rs"]
mod test;

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteImplLongId {
    pub impl_def_id: ImplDefId,
    pub generic_args: Vec<GenericArgumentId>,
}
define_short_id!(
    ConcreteImplId,
    ConcreteImplLongId,
    SemanticGroup,
    lookup_intern_concrete_impl,
    intern_concrete_impl
);
semantic_object_for_id!(
    ConcreteImplId,
    lookup_intern_concrete_impl,
    intern_concrete_impl,
    ConcreteImplLongId
);
impl DebugWithDb<dyn SemanticGroup> for ConcreteImplLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{}", self.impl_def_id.full_path(db.upcast()))?;
        if !self.generic_args.is_empty() {
            write!(f, "::<")?;
            for (i, arg) in self.generic_args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg.format(db))?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}
impl ConcreteImplId {
    pub fn impl_def_id(&self, db: &dyn SemanticGroup) -> ImplDefId {
        self.lookup_intern(db).impl_def_id
    }
    pub fn get_impl_function(
        &self,
        db: &dyn SemanticGroup,
        function: TraitFunctionId,
    ) -> Maybe<Option<ImplFunctionId>> {
        db.impl_function_by_trait_function(self.impl_def_id(db), function)
    }
    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        self.impl_def_id(db).name(db.upcast())
    }
    pub fn substitution(&self, db: &dyn SemanticGroup) -> Maybe<GenericSubstitution> {
        Ok(GenericSubstitution::from_impl(ImplLongId::Concrete(*self).intern(db)).concat(
            GenericSubstitution::new(
                &db.impl_def_generic_params(self.impl_def_id(db))?,
                &self.lookup_intern(db).generic_args,
            ),
        ))
    }
    /// Returns true if the `impl` does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        self.lookup_intern(db)
            .generic_args
            .iter()
            .all(|generic_argument_id| generic_argument_id.is_fully_concrete(db))
    }
    /// Returns true if the `impl` does not depend on impl or type variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        self.lookup_intern(db)
            .generic_args
            .iter()
            .all(|generic_argument_id| generic_argument_id.is_var_free(db))
    }
}

/// Represents a "callee" impl that can be referred to in the code.
/// Traits should be resolved to this.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum ImplLongId {
    Concrete(ConcreteImplId),
    GenericParameter(GenericParamId),
    ImplVar(ImplVarId),
    ImplImpl(ImplImplId),
    TraitImpl(TraitImplId),
    GeneratedImpl(GeneratedImplId),
}
impl ImplLongId {
    /// Returns the [ImplHead] of an impl if available.
    pub fn head(&self, db: &dyn SemanticGroup) -> Option<ImplHead> {
        Some(match self {
            ImplLongId::Concrete(concrete) => ImplHead::Concrete(concrete.impl_def_id(db)),
            ImplLongId::GenericParameter(_)
            | ImplLongId::ImplVar(_)
            | ImplLongId::ImplImpl(_)
            | ImplLongId::TraitImpl(_)
            | ImplLongId::GeneratedImpl(_) => {
                return None;
            }
        })
    }
    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        match self {
            ImplLongId::Concrete(concrete_impl) => concrete_impl.name(db),
            ImplLongId::GenericParameter(generic_param_impl) => {
                generic_param_impl.name(db.upcast()).unwrap_or_else(|| "_".into())
            }
            ImplLongId::ImplVar(var) => {
                format!("ImplVar({})", var.concrete_trait_id(db).full_path(db)).into()
            }
            ImplLongId::ImplImpl(impl_impl) => format!(
                "{}::{}",
                impl_impl.impl_id().name(db.upcast()),
                db.impl_impl_concrete_trait(*impl_impl)
                    .map(|trait_impl| trait_impl.full_path(db))
                    .unwrap_or_else(|_| "_".into())
            )
            .into(),
            ImplLongId::TraitImpl(trait_impl) => trait_impl.name(db.upcast()),
            ImplLongId::GeneratedImpl(generated_impl) => {
                format!("{:?}", generated_impl.debug(db.elongate())).into()
            }
        }
    }
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        match self {
            ImplLongId::Concrete(concrete_impl) => {
                format!("{:?}", concrete_impl.debug(db.elongate()))
            }
            ImplLongId::GenericParameter(generic_param_impl) => {
                generic_param_impl.format(db.upcast())
            }
            ImplLongId::ImplVar(var) => format!("{var:?}"),
            ImplLongId::ImplImpl(impl_impl) => format!("{:?}", impl_impl.debug(db.elongate())),
            ImplLongId::TraitImpl(trait_impl) => format!("{:?}", trait_impl.debug(db.elongate())),
            ImplLongId::GeneratedImpl(generated_impl) => {
                format!("{:?}", generated_impl.debug(db.elongate()))
            }
        }
    }

    /// Returns true if the `impl` does not depend on impl or type variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        match self {
            ImplLongId::Concrete(concrete_impl_id) => concrete_impl_id.is_var_free(db),
            ImplLongId::GenericParameter(_) | ImplLongId::TraitImpl(_) => true,
            ImplLongId::ImplVar(_) => false,
            ImplLongId::ImplImpl(impl_impl) => impl_impl.impl_id().is_var_free(db),
            ImplLongId::GeneratedImpl(generated_impl) => {
                generated_impl.concrete_trait.is_var_free(db)
            }
        }
    }

    /// Returns true if the `impl` does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        match self {
            ImplLongId::Concrete(concrete_impl_id) => concrete_impl_id.is_fully_concrete(db),
            ImplLongId::GenericParameter(_) => false,
            ImplLongId::ImplVar(_) => false,
            ImplLongId::ImplImpl(_) | ImplLongId::TraitImpl(_) => false,
            ImplLongId::GeneratedImpl(generated_impl) => {
                generated_impl.concrete_trait.is_fully_concrete(db)
            }
        }
    }
}
impl DebugWithDb<dyn SemanticGroup> for ImplLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        match self {
            ImplLongId::Concrete(concrete_impl_id) => write!(f, "{:?}", concrete_impl_id.debug(db)),
            ImplLongId::GenericParameter(param) => write!(f, "{:?}", param.debug(db)),
            ImplLongId::ImplVar(var) => write!(f, "?{}", var.lookup_intern(db).id.0),
            ImplLongId::ImplImpl(impl_impl) => write!(f, "{:?}", impl_impl.debug(db)),
            ImplLongId::TraitImpl(trait_impl) => write!(f, "{:?}", trait_impl.debug(db)),
            ImplLongId::GeneratedImpl(generated_impl) => {
                write!(f, "Generated {}", generated_impl.concrete_trait.full_path(db.upcast()))
            }
        }
    }
}

define_short_id!(ImplId, ImplLongId, SemanticGroup, lookup_intern_impl, intern_impl);
semantic_object_for_id!(ImplId, lookup_intern_impl, intern_impl, ImplLongId);
impl ImplId {
    pub fn concrete_trait(&self, db: &dyn SemanticGroup) -> Maybe<ConcreteTraitId> {
        db.impl_concrete_trait(*self)
    }

    /// Returns true if the `impl` does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        db.priv_impl_is_fully_concrete(*self)
    }

    /// Returns true if the `impl` does not depend on impl or type variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        db.priv_impl_is_var_free(*self)
    }

    /// Returns the [ImplHead] of an impl if available.
    pub fn head(&self, db: &dyn SemanticGroup) -> Option<ImplHead> {
        self.lookup_intern(db).head(db)
    }

    /// Returns the name of the impl.
    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        self.lookup_intern(db).name(db)
    }

    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        self.lookup_intern(db).format(db)
    }
}

/// An impl that is generated by the compiler for a specific trait.
/// There can be only one such impl per concrete trait as otherwise there would be a
/// MultipleImplsFound ambiguity.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct GeneratedImplId {
    pub concrete_trait: ConcreteTraitId,
}

impl DebugWithDb<dyn SemanticGroup> for GeneratedImplId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "Generated ")?;
        self.concrete_trait.fmt(f, db)
    }
}

/// An impl item of kind impl.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ImplImplId {
    /// The impl the item impl is in.
    impl_id: ImplId,
    /// The trait impl this impl impl "implements".
    trait_impl_id: TraitImplId,
}

impl ImplImplId {
    /// Creates a new impl impl id. For an impl impl of a concrete impl, asserts that the
    /// trait impl belongs to the same trait that the impl implements (panics if not).
    pub fn new(impl_id: ImplId, trait_impl_id: TraitImplId, db: &dyn SemanticGroup) -> Self {
        if let crate::items::imp::ImplLongId::Concrete(concrete_impl) = impl_id.lookup_intern(db) {
            let impl_def_id = concrete_impl.impl_def_id(db);
            assert_eq!(Ok(trait_impl_id.trait_id(db.upcast())), db.impl_def_trait(impl_def_id));
        }

        ImplImplId { impl_id, trait_impl_id }
    }
    pub fn impl_id(&self) -> ImplId {
        self.impl_id
    }
    pub fn trait_impl_id(&self) -> TraitImplId {
        self.trait_impl_id
    }

    pub fn format(&self, db: &dyn SemanticGroup) -> SmolStr {
        format!("{}::{}", self.impl_id.name(db.upcast()), self.trait_impl_id.name(db.upcast()))
            .into()
    }
}
impl DebugWithDb<dyn SemanticGroup> for ImplImplId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{}", self.format(db))
    }
}

impl UnstableSalsaId for ImplId {
    fn get_internal_id(&self) -> &salsa::InternId {
        &self.0
    }
}

/// Head of an impl. A non-param non-variable impl has a head, which represents the kind of the root
/// node in its tree representation. This is used for caching queries for fast lookups when the impl
/// is not completely inferred yet.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ImplHead {
    Concrete(ImplDefId),
}

// === Impl Declaration ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<semantic::GenericParam>,
    /// The concrete trait this impl implements, or Err if cannot be resolved.
    concrete_trait: Maybe<ConcreteTraitId>,
    attributes: Vec<Attribute>,
    resolver_data: Arc<ResolverData>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::impl_semantic_declaration_diagnostics].
pub fn impl_semantic_declaration_diagnostics(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_declaration_data(impl_def_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_generic_params_data].
pub fn impl_def_generic_params_data(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<GenericParamsData> {
    let module_file_id = impl_def_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();

    let impl_ast = db.module_impl_by_id(impl_def_id)?.to_maybe()?;
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(ModuleItemId::Impl(impl_def_id)));

    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    resolver.set_feature_config(&impl_def_id, &impl_ast, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &impl_ast.generic_params(db.upcast()),
    );
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, impl_ast.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { generic_params, diagnostics: diagnostics.build(), resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_generic_params].
pub fn impl_def_generic_params(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Vec<semantic::GenericParam>> {
    Ok(db.impl_def_generic_params_data(impl_def_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_resolver_data].
pub fn impl_def_resolver_data(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_impl_declaration_data(impl_def_id)?.resolver_data)
}

/// Trivial cycle handler for [crate::db::SemanticGroup::impl_def_resolver_data].
pub fn impl_def_resolver_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_def_id: &ImplDefId,
) -> Maybe<Arc<ResolverData>> {
    // Forwarding (not as a query) cycle handling to `priv_impl_declaration_data` cycle handler.
    impl_def_resolver_data(db, *impl_def_id)
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_concrete_trait].
pub fn impl_def_concrete_trait(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<ConcreteTraitId> {
    db.priv_impl_declaration_data(impl_def_id)?.concrete_trait
}

/// Trivial cycle handler for [crate::db::SemanticGroup::impl_def_concrete_trait].
pub fn impl_def_concrete_trait_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_def_id: &ImplDefId,
) -> Maybe<ConcreteTraitId> {
    // Forwarding (not as a query) cycle handling to `priv_impl_declaration_data` cycle handler.
    impl_def_concrete_trait(db, *impl_def_id)
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_substitution].
pub fn impl_def_substitution(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Arc<GenericSubstitution>> {
    let params = db.impl_def_generic_params(impl_def_id)?;
    let generic_args = generic_params_to_args(&params, db);
    Ok(Arc::new(ConcreteImplLongId { impl_def_id, generic_args }.intern(db).substitution(db)?))
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_attributes].
pub fn impl_def_attributes(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_impl_declaration_data(impl_def_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_trait].
pub fn impl_def_trait(db: &dyn SemanticGroup, impl_def_id: ImplDefId) -> Maybe<TraitId> {
    let module_file_id = impl_def_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();

    let impl_ast = db.module_impl_by_id(impl_def_id)?.to_maybe()?;
    let inference_id = InferenceId::ImplDefTrait(impl_def_id);

    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    resolver.set_feature_config(&impl_def_id, &impl_ast, &mut diagnostics);

    let trait_path_syntax = impl_ast.trait_path(db.upcast());

    resolve_trait_path(&mut diagnostics, &mut resolver, &trait_path_syntax)
}

/// Query implementation of [crate::db::SemanticGroup::impl_concrete_trait].
pub fn impl_concrete_trait(db: &dyn SemanticGroup, impl_id: ImplId) -> Maybe<ConcreteTraitId> {
    match impl_id.lookup_intern(db) {
        ImplLongId::Concrete(concrete_impl_id) => {
            let long_impl = concrete_impl_id.lookup_intern(db);
            let substitution = GenericSubstitution::new(
                &db.impl_def_generic_params(long_impl.impl_def_id)?,
                &long_impl.generic_args,
            );

            let impl_concrete_trait_id = db.impl_def_concrete_trait(long_impl.impl_def_id)?;
            SubstitutionRewriter { db, substitution: &substitution }.rewrite(impl_concrete_trait_id)
        }
        ImplLongId::GenericParameter(param) => {
            let param_impl =
                extract_matches!(db.generic_param_semantic(param)?, GenericParam::Impl);
            param_impl.concrete_trait
        }
        ImplLongId::ImplVar(var) => Ok(var.lookup_intern(db).concrete_trait_id),
        ImplLongId::ImplImpl(impl_impl) => impl_impl_concrete_trait(db, impl_impl),
        ImplLongId::TraitImpl(trait_impl) => db.trait_impl_concrete_trait(trait_impl),
        ImplLongId::GeneratedImpl(generated_impl) => Ok(generated_impl.concrete_trait),
    }
}

// --- Computation ---

/// Cycle handling for [crate::db::SemanticGroup::priv_impl_declaration_data].
pub fn priv_impl_declaration_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_def_id: &ImplDefId,
) -> Maybe<ImplDeclarationData> {
    priv_impl_declaration_data_inner(db, *impl_def_id, false)
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_declaration_data].
pub fn priv_impl_declaration_data(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<ImplDeclarationData> {
    priv_impl_declaration_data_inner(db, impl_def_id, true)
}

/// Shared code for the query and cycle handling.
/// The cycle handling logic needs to pass resolve_trait=false to prevent the cycle.
pub fn priv_impl_declaration_data_inner(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    resolve_trait: bool,
) -> Maybe<ImplDeclarationData> {
    let mut diagnostics = SemanticDiagnostics::default();

    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let syntax_db = db.upcast();
    let impl_ast = db.module_impl_by_id(impl_def_id)?.to_maybe()?;
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::Impl(impl_def_id),
    ));

    // Generic params.
    let generic_params_data = db.impl_def_generic_params_data(impl_def_id)?;
    let generic_params = generic_params_data.generic_params;
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    resolver.set_feature_config(&impl_def_id, &impl_ast, &mut diagnostics);
    diagnostics.extend(generic_params_data.diagnostics);
    let trait_path_syntax = impl_ast.trait_path(syntax_db);

    let concrete_trait = if resolve_trait {
        match resolver.resolve_concrete_path(
            &mut diagnostics,
            &trait_path_syntax,
            NotFoundItemType::Trait,
        ) {
            Ok(resolved_item) => try_extract_matches!(resolved_item, ResolvedConcreteItem::Trait)
                .ok_or_else(|| diagnostics.report(&trait_path_syntax, NotATrait)),
            Err(err) => Err(err),
        }
    } else {
        Err(diagnostics.report(&trait_path_syntax, ImplRequirementCycle))
    };

    // Check for reimplementation of TypeEqual Trait.
    if let Ok(concrete_trait) = concrete_trait {
        if concrete_trait.trait_id(db)
            == get_core_trait(db, CoreTraitContext::MetaProgramming, "TypeEqual".into())
            && impl_def_id.module_file_id(db.upcast()).0 != core_submodule(db, "metaprogramming")
        {
            diagnostics.report(&trait_path_syntax, TypeEqualTraitReImplementation);
        }
    }

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, impl_ast.stable_ptr().untyped());

    let concrete_trait = inference.rewrite(concrete_trait).no_err();
    let generic_params = inference.rewrite(generic_params).no_err();

    let attributes = impl_ast.attributes(syntax_db).structurize(syntax_db);
    let mut resolver_data = resolver.data;
    resolver_data.trait_or_impl_ctx = TraitOrImplContext::Impl(impl_def_id);
    Ok(ImplDeclarationData {
        diagnostics: diagnostics.build(),
        generic_params,
        concrete_trait,
        attributes,
        resolver_data: Arc::new(resolver_data),
    })
}

// === Impl Definition ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplDefinitionData {
    /// The diagnostics here are "flat" - that is, only the diagnostics found on the impl level
    /// itself, and don't include the diagnostics of its items. The reason it's this way is that
    /// computing the items' diagnostics require a query about their impl, forming a cycle of
    /// queries. Adding the items' diagnostics only after the whole computation breaks this cycle.
    diagnostics: Diagnostics<SemanticDiagnostic>,

    // AST maps.
    function_asts: OrderedHashMap<ImplFunctionId, ast::FunctionWithBody>,
    item_type_asts: Arc<OrderedHashMap<ImplTypeDefId, ast::ItemTypeAlias>>,
    item_constant_asts: Arc<OrderedHashMap<ImplConstantDefId, ast::ItemConstant>>,
    item_impl_asts: Arc<OrderedHashMap<ImplImplDefId, ast::ItemImplAlias>>,

    /// Mapping of item names to their IDs. All the IDs should appear in one of the AST maps above.
    item_id_by_name: Arc<OrderedHashMap<SmolStr, ImplItemId>>,

    /// Mapping of missing impl names item names to the trait id.
    implicit_impls_id_by_name: Arc<OrderedHashMap<SmolStr, TraitImplId>>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::impl_semantic_definition_diagnostics].
pub fn impl_semantic_definition_diagnostics(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Diagnostics<SemanticDiagnostic> {
    let mut diagnostics = DiagnosticsBuilder::default();

    let Ok(data) = db.priv_impl_definition_data(impl_def_id) else {
        return Diagnostics::default();
    };

    // The diagnostics from `priv_impl_definition_data` are only the diagnostics from the impl
    // level. They should be enriched with the items' diagnostics.
    diagnostics.extend(data.diagnostics);
    for impl_function_id in data.function_asts.keys() {
        diagnostics.extend(db.impl_function_declaration_diagnostics(*impl_function_id));
        diagnostics.extend(db.impl_function_body_diagnostics(*impl_function_id));
    }
    for impl_item_type_id in data.item_type_asts.keys() {
        diagnostics.extend(db.impl_type_def_semantic_diagnostics(*impl_item_type_id));
        if let Ok(ty) = db.impl_type_def_resolved_type(*impl_item_type_id) {
            add_type_based_diagnostics(
                db,
                &mut diagnostics,
                ty,
                impl_item_type_id.stable_ptr(db.upcast()),
            );
        }
    }
    for impl_item_constant_id in data.item_constant_asts.keys() {
        diagnostics.extend(db.impl_constant_def_semantic_diagnostics(*impl_item_constant_id));
    }
    for impl_item_impl_id in data.item_impl_asts.keys() {
        diagnostics.extend(db.impl_impl_def_semantic_diagnostics(*impl_item_impl_id));
    }
    for implicit_impl_id in data.implicit_impls_id_by_name.values() {
        diagnostics
            .extend(db.implicit_impl_impl_semantic_diagnostics(impl_def_id, *implicit_impl_id));
    }
    // Diagnostics for special traits.
    if diagnostics.error_count == 0 {
        let trait_id = db
            .priv_impl_declaration_data(impl_def_id)
            .unwrap()
            .concrete_trait
            .unwrap()
            .trait_id(db);
        if trait_id == deref_trait(db) {
            handle_deref_impl(db, impl_def_id, &mut diagnostics);
        }
    }
    diagnostics.build()
}

/// Checks that there are no cycles of Deref impls.
fn handle_deref_impl(
    db: &dyn SemanticGroup,
    mut impl_def_id: ImplDefId,
    diagnostics: &mut DiagnosticsBuilder<SemanticDiagnostic>,
) {
    let mut visited_impls: OrderedHashSet<ImplDefId> = OrderedHashSet::default();
    let deref_trait_id = deref_trait(db);
    loop {
        let Ok(impl_id) = get_impl_based_on_single_impl_type(db, impl_def_id, |ty| {
            ConcreteTraitLongId {
                trait_id: deref_trait_id,
                generic_args: vec![GenericArgumentId::Type(ty)],
            }
            .intern(db)
        }) else {
            // Inference errors are handled when the impl is in actual use. In here we only check
            // for cycles.
            return;
        };
        impl_def_id = match impl_id.lookup_intern(db) {
            ImplLongId::Concrete(concrete_impl_id) => concrete_impl_id.impl_def_id(db),
            _ => return,
        };

        if !visited_impls.insert(impl_def_id) {
            let deref_chain = visited_impls
                .iter()
                .map(|visited_impl| {
                    format!(
                        "{:?}",
                        db.impl_def_concrete_trait(*visited_impl).unwrap().debug(db.elongate())
                    )
                })
                .join(" -> ");
            diagnostics.report(
                impl_def_id.stable_ptr(db.upcast()),
                SemanticDiagnosticKind::DerefCycle { deref_chain },
            );
            return;
        }
    }
}

/// Assuming that an impl has a single impl type, extracts the type, and then infers another impl
/// based on it. If the inference fails, returns the inference error and the impl type definition
/// for diagnostics.
fn get_impl_based_on_single_impl_type(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    concrete_trait_id: impl FnOnce(TypeId) -> ConcreteTraitId,
) -> Result<ImplId, (InferenceError, ImplTypeDefId)> {
    let data = db.priv_impl_definition_data(impl_def_id).unwrap();
    let mut types_iter = data.item_type_asts.iter();
    let (impl_item_type_id, _) = types_iter.next().unwrap();
    if types_iter.next().is_some() {
        panic!(
            "get_impl_based_on_single_impl_type called with an impl that has more than one type"
        );
    }
    let ty = db.impl_type_def_resolved_type(*impl_item_type_id).unwrap();
    let module_file_id = impl_def_id.module_file_id(db.upcast());
    let generic_params = db.impl_def_generic_params(impl_def_id).unwrap();
    let generic_params_ids =
        generic_params.iter().map(|generic_param| generic_param.id()).collect();
    let lookup_context = ImplLookupContext::new(module_file_id.0, generic_params_ids);
    get_impl_at_context(db, lookup_context, concrete_trait_id(ty), None)
        .map_err(|err| (err, *impl_item_type_id))
}

/// Query implementation of [crate::db::SemanticGroup::impl_functions].
pub fn impl_functions(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<OrderedHashMap<SmolStr, ImplFunctionId>> {
    Ok(db
        .priv_impl_definition_data(impl_def_id)?
        .function_asts
        .keys()
        .map(|function_id| {
            let function_long_id = function_id.lookup_intern(db);
            (function_long_id.name(db.upcast()), *function_id)
        })
        .collect())
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_by_trait_function].
pub fn impl_function_by_trait_function(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    trait_function_id: TraitFunctionId,
) -> Maybe<Option<ImplFunctionId>> {
    let defs_db = db.upcast();
    let name = trait_function_id.name(defs_db);
    for impl_function_id in db.priv_impl_definition_data(impl_def_id)?.function_asts.keys() {
        if impl_function_id.lookup_intern(db).name(defs_db) == name {
            return Ok(Some(*impl_function_id));
        }
    }
    Ok(None)
}

/// Query implementation of [crate::db::SemanticGroup::impl_item_by_name].
pub fn impl_item_by_name(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    name: SmolStr,
) -> Maybe<Option<ImplItemId>> {
    Ok(db.priv_impl_definition_data(impl_def_id)?.item_id_by_name.get(&name).cloned())
}

/// Query implementation of [crate::db::SemanticGroup::impl_implicit_impl_by_name].
pub fn impl_implicit_impl_by_name(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    name: SmolStr,
) -> Maybe<Option<TraitImplId>> {
    Ok(db.priv_impl_definition_data(impl_def_id)?.implicit_impls_id_by_name.get(&name).cloned())
}

/// Query implementation of [SemanticGroup::impl_all_used_items].
pub fn impl_all_used_items(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Arc<OrderedHashSet<LookupItemId>>> {
    let mut all_used_items = db.impl_def_resolver_data(impl_def_id)?.used_items.clone();
    let data = db.priv_impl_definition_data(impl_def_id)?;
    for item in data.item_id_by_name.values() {
        for resolver_data in get_resolver_data_options(LookupItemId::ImplItem(*item), db) {
            all_used_items.extend(resolver_data.used_items.iter().cloned());
        }
    }
    Ok(all_used_items.into())
}

/// Query implementation of [crate::db::SemanticGroup::impl_types].
pub fn impl_types(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Arc<OrderedHashMap<ImplTypeDefId, ast::ItemTypeAlias>>> {
    Ok(db.priv_impl_definition_data(impl_def_id)?.item_type_asts)
}

/// Query implementation of [crate::db::SemanticGroup::impl_type_ids].
pub fn impl_type_ids(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Arc<[ImplTypeDefId]>> {
    Ok(db.impl_types(impl_def_id)?.keys().copied().collect_vec().into())
}

/// Query implementation of [crate::db::SemanticGroup::impl_type_by_id].
pub fn impl_type_by_id(
    db: &dyn SemanticGroup,
    impl_type_id: ImplTypeDefId,
) -> Maybe<Option<ast::ItemTypeAlias>> {
    let impl_types = db.impl_types(impl_type_id.impl_def_id(db.upcast()))?;
    Ok(impl_types.get(&impl_type_id).cloned())
}

/// Query implementation of [crate::db::SemanticGroup::impl_type_by_trait_type].
pub fn impl_type_by_trait_type(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    trait_type_id: TraitTypeId,
) -> Maybe<ImplTypeDefId> {
    if trait_type_id.trait_id(db.upcast()) != db.impl_def_trait(impl_def_id)? {
        unreachable!(
            "impl_type_by_trait_type called with a trait type that does not belong to the impl's \
             trait"
        )
    }

    let defs_db = db.upcast();
    let name = trait_type_id.name(defs_db);
    // If the trait type's name is not found, then a missing item diagnostic is reported.
    db.impl_item_by_name(impl_def_id, name).and_then(|maybe_item_id| match maybe_item_id {
        Some(item_id) => Ok(extract_matches!(item_id, ImplItemId::Type)),
        None => Err(skip_diagnostic()),
    })
}

/// Query implementation of [crate::db::SemanticGroup::impl_constants].
pub fn impl_constants(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Arc<OrderedHashMap<ImplConstantDefId, ast::ItemConstant>>> {
    Ok(db.priv_impl_definition_data(impl_def_id)?.item_constant_asts)
}

/// Query implementation of [crate::db::SemanticGroup::impl_constant_by_trait_constant].
pub fn impl_constant_by_trait_constant(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    trait_constant_id: TraitConstantId,
) -> Maybe<ImplConstantDefId> {
    if trait_constant_id.trait_id(db.upcast()) != db.impl_def_trait(impl_def_id)? {
        unreachable!(
            "impl_constant_by_trait_constant called with a trait constant that does not belong to \
             the impl's trait"
        )
    }

    let defs_db = db.upcast();
    let name = trait_constant_id.name(defs_db);
    // If the trait constant's name is not found, then a missing item diagnostic is reported.
    db.impl_item_by_name(impl_def_id, name).and_then(|maybe_item_id| match maybe_item_id {
        Some(item_id) => Ok(extract_matches!(item_id, ImplItemId::Constant)),
        None => Err(skip_diagnostic()),
    })
}

/// Query implementation of [crate::db::SemanticGroup::impl_impls].
pub fn impl_impls(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Arc<OrderedHashMap<ImplImplDefId, ast::ItemImplAlias>>> {
    Ok(db.priv_impl_definition_data(impl_def_id)?.item_impl_asts)
}

/// Query implementation of [crate::db::SemanticGroup::impl_impl_ids].
pub fn impl_impl_ids(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Arc<[ImplImplDefId]>> {
    Ok(db.impl_impls(impl_def_id)?.keys().copied().collect_vec().into())
}

/// Query implementation of [crate::db::SemanticGroup::impl_impl_by_id].
pub fn impl_impl_by_id(
    db: &dyn SemanticGroup,
    impl_impl_id: ImplImplDefId,
) -> Maybe<Option<ast::ItemImplAlias>> {
    let impl_impls = db.impl_impls(impl_impl_id.impl_def_id(db.upcast()))?;
    Ok(impl_impls.get(&impl_impl_id).cloned())
}

/// Query implementation of [crate::db::SemanticGroup::impl_impl_by_trait_impl].
pub fn impl_impl_by_trait_impl(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    trait_impl_id: TraitImplId,
) -> Maybe<ImplImplDefId> {
    if trait_impl_id.trait_id(db.upcast()) != db.impl_def_trait(impl_def_id)? {
        unreachable!(
            "impl_impl_by_trait_impl called with a trait impl that does not belong to the impl's \
             trait"
        )
    }

    let defs_db = db.upcast();
    let name = trait_impl_id.name(defs_db);
    // If the trait impl's name is not found, then a missing item diagnostic is reported.
    db.impl_item_by_name(impl_def_id, name).and_then(|maybe_item_id| match maybe_item_id {
        Some(item_id) => Ok(extract_matches!(item_id, ImplItemId::Impl)),
        None => Err(skip_diagnostic()),
    })
}

/// Query implementation of [crate::db::SemanticGroup::is_implicit_impl_impl].
pub fn is_implicit_impl_impl(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    trait_impl_id: TraitImplId,
) -> Maybe<bool> {
    if trait_impl_id.trait_id(db.upcast()) != db.impl_def_trait(impl_def_id)? {
        unreachable!(
            "impl_impl_by_trait_impl called with a trait impl that does not belong to the impl's \
             trait"
        )
    }

    let defs_db = db.upcast();
    let name = trait_impl_id.name(defs_db);
    // If the trait impl's name is not found, then a missing item diagnostic is reported.
    Ok(db.impl_implicit_impl_by_name(impl_def_id, name)?.is_some())
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_impl_definition_data].
pub fn priv_impl_definition_data(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<ImplDefinitionData> {
    let syntax_db = db.upcast();

    let module_file_id = impl_def_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();

    let generic_params = db.impl_def_generic_params(impl_def_id)?;
    let concrete_trait = db.priv_impl_declaration_data(impl_def_id)?.concrete_trait?;

    let impl_ast = db.module_impl_by_id(impl_def_id)?.to_maybe()?;

    let generic_params_ids =
        generic_params.iter().map(|generic_param| generic_param.id()).collect();
    let lookup_context = ImplLookupContext::new(module_file_id.0, generic_params_ids);
    check_special_impls(
        db,
        &mut diagnostics,
        lookup_context,
        concrete_trait,
        impl_ast.stable_ptr().untyped(),
    )
    // Ignore the result.
    .ok();

    let mut function_asts = OrderedHashMap::default();
    let mut item_type_asts = OrderedHashMap::default();
    let mut item_constant_asts = OrderedHashMap::default();
    let mut item_impl_asts = OrderedHashMap::default();
    let mut item_id_by_name = OrderedHashMap::default();

    if let MaybeImplBody::Some(body) = impl_ast.body(syntax_db) {
        for item in body.items(syntax_db).elements(syntax_db) {
            match item {
                ImplItem::Module(module) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    module.module_kw(syntax_db),
                ),

                ImplItem::Use(use_item) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    use_item.use_kw(syntax_db),
                ),
                ImplItem::ExternFunction(extern_func) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    extern_func.extern_kw(syntax_db),
                ),
                ImplItem::ExternType(extern_type) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    extern_type.extern_kw(syntax_db),
                ),
                ImplItem::Trait(trt) => {
                    report_invalid_impl_item(syntax_db, &mut diagnostics, trt.trait_kw(syntax_db))
                }
                ImplItem::Struct(structure) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    structure.struct_kw(syntax_db),
                ),
                ImplItem::Enum(enm) => {
                    report_invalid_impl_item(syntax_db, &mut diagnostics, enm.enum_kw(syntax_db))
                }
                ImplItem::Function(func) => {
                    let impl_function_id =
                        ImplFunctionLongId(module_file_id, func.stable_ptr()).intern(db);
                    let name_node = func.declaration(syntax_db).name(syntax_db);
                    let name = name_node.text(syntax_db);
                    if item_id_by_name
                        .insert(name.clone(), ImplItemId::Function(impl_function_id))
                        .is_some()
                    {
                        diagnostics.report(&name_node, NameDefinedMultipleTimes(name));
                    }
                    function_asts.insert(impl_function_id, func);
                }
                ImplItem::Type(ty) => {
                    let impl_type_id =
                        ImplTypeDefLongId(module_file_id, ty.stable_ptr()).intern(db);
                    let name_node = ty.name(syntax_db);
                    let name = name_node.text(syntax_db);
                    if item_id_by_name
                        .insert(name.clone(), ImplItemId::Type(impl_type_id))
                        .is_some()
                    {
                        diagnostics.report(&name_node, NameDefinedMultipleTimes(name));
                    }
                    item_type_asts.insert(impl_type_id, ty);
                }
                ImplItem::Constant(constant) => {
                    let impl_constant_id =
                        ImplConstantDefLongId(module_file_id, constant.stable_ptr()).intern(db);
                    let name_node = constant.name(syntax_db);
                    let name = name_node.text(syntax_db);
                    if item_id_by_name
                        .insert(name.clone(), ImplItemId::Constant(impl_constant_id))
                        .is_some()
                    {
                        diagnostics.report(
                            &name_node,
                            SemanticDiagnosticKind::NameDefinedMultipleTimes(name),
                        );
                    }
                    item_constant_asts.insert(impl_constant_id, constant);
                }
                ImplItem::Impl(imp) => {
                    let impl_constant_id =
                        ImplImplDefLongId(module_file_id, imp.stable_ptr()).intern(db);
                    let name_node = imp.name(syntax_db);
                    let name = name_node.text(syntax_db);
                    if item_id_by_name
                        .insert(name.clone(), ImplItemId::Impl(impl_constant_id))
                        .is_some()
                    {
                        diagnostics.report(
                            &name_node,
                            SemanticDiagnosticKind::NameDefinedMultipleTimes(name),
                        );
                    }
                    item_impl_asts.insert(impl_constant_id, imp);
                }
                // Report nothing, a parser diagnostic is reported.
                ImplItem::Missing(_) => {}
            }
        }
    }

    let mut implicit_impls_id_by_name = OrderedHashMap::default();

    let trait_id = concrete_trait.lookup_intern(db).trait_id;
    for trait_impl_id in db.trait_impls(trait_id)? {
        if item_id_by_name.contains_key(&trait_impl_id.0) {
            continue;
        }
        implicit_impls_id_by_name.insert(trait_impl_id.0, trait_impl_id.1);
    }

    // It is later verified that all items in this impl match items from `concrete_trait`.
    // To ensure exact match (up to trait functions with default implementation), it is sufficient
    // to verify here that all items in `concrete_trait` appear in this impl.
    let impl_item_names: OrderedHashSet<SmolStr> = item_id_by_name.keys().cloned().collect();

    let trait_required_item_names = db.trait_required_item_names(trait_id)?;
    let missing_items_in_impl =
        trait_required_item_names.difference(&impl_item_names).cloned().collect::<Vec<_>>();
    if !missing_items_in_impl.is_empty() {
        diagnostics.report(
            // TODO(yuval): change this to point to impl declaration (need to add ImplDeclaration
            // in cairo_spec).
            // TODO(TomerStarkware): make sure we do not report missing if the trait item is
            // unsupported in impl.
            &impl_ast.name(syntax_db),
            SemanticDiagnosticKind::MissingItemsInImpl(missing_items_in_impl),
        );
    }

    Ok(ImplDefinitionData {
        diagnostics: diagnostics.build(),
        function_asts,
        item_type_asts: item_type_asts.into(),
        item_id_by_name: item_id_by_name.into(),
        item_constant_asts: item_constant_asts.into(),
        item_impl_asts: item_impl_asts.into(),
        implicit_impls_id_by_name: implicit_impls_id_by_name.into(),
    })
}

/// An helper function to report diagnostics of items in an impl (used in
/// priv_impl_definition_data).
fn report_invalid_impl_item<Terminal: syntax::node::Terminal>(
    syntax_db: &dyn SyntaxGroup,
    diagnostics: &mut SemanticDiagnostics,
    kw_terminal: Terminal,
) {
    diagnostics.report(
        kw_terminal.as_syntax_node().stable_ptr(),
        InvalidImplItem(kw_terminal.text(syntax_db)),
    );
}

/// Handle special cases such as Copy and Drop checking.
fn check_special_impls(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    lookup_context: ImplLookupContext,
    concrete_trait: ConcreteTraitId,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<()> {
    let ConcreteTraitLongId { trait_id, generic_args } = concrete_trait.lookup_intern(db);
    let copy = copy_trait(db);
    let drop = drop_trait(db);

    if trait_id == copy {
        let tys = get_inner_types(db, extract_matches!(generic_args[0], GenericArgumentId::Type))?;
        if let Some(inference_error) = tys
            .into_iter()
            .filter_map(|ty| db.type_info(lookup_context.clone(), ty).to_option())
            .flat_map(|info| info.copyable.err())
            .next()
        {
            return Err(diagnostics.report(stable_ptr, InvalidCopyTraitImpl(inference_error)));
        }
    }
    if trait_id == drop {
        let tys = get_inner_types(db, extract_matches!(generic_args[0], GenericArgumentId::Type))?;
        if let Some(inference_error) = tys
            .into_iter()
            .filter_map(|ty| db.type_info(lookup_context.clone(), ty).to_option())
            .flat_map(|info| info.droppable.err())
            .next()
        {
            return Err(diagnostics.report(stable_ptr, InvalidDropTraitImpl(inference_error)));
        }
    }

    Ok(())
}

/// Retrieves all the inner types (members of a struct / tuple or variants of an enum).
///
/// These are the types that are required to implement some trait,
/// in order for the original type to be able to implement this trait.
///
/// For example, a struct containing a type T can implement Drop only if T implements Drop.
fn get_inner_types(db: &dyn SemanticGroup, ty: TypeId) -> Maybe<Vec<TypeId>> {
    Ok(match ty.lookup_intern(db) {
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
        TypeLongId::Snapshot(_) | TypeLongId::Closure(_) => vec![],
        TypeLongId::GenericParameter(_) => {
            return Err(skip_diagnostic());
        }
        TypeLongId::Var(_) | TypeLongId::TraitType(_) | TypeLongId::ImplType(_) => {
            panic!("Types should be fully resolved at this point.")
        }
        TypeLongId::Coupon(_) => vec![],
        TypeLongId::FixedSizeArray { type_id, .. } => vec![type_id],
        TypeLongId::Missing(diag_added) => {
            return Err(diag_added);
        }
    })
}

// === Trait Filter ===

/// A filter for trait lookup that is not based on current inference state. This is
/// used for caching queries.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TraitFilter {
    pub trait_id: TraitId,
    /// The filter on the generic arguments.
    pub generics_filter: GenericsHeadFilter,
}

/// A lookup filter on generic arguments that is not based on current inference state.
/// This is used for caching queries.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericsHeadFilter {
    /// No filter is applied. When nothing is known about the generics, this will lead to a
    /// wider search.
    NoFilter,
    /// Generics exists and the first generic parameter has a filter.
    /// This is usually enough to considerably reduce the number of searched items.
    FirstGenericFilter(GenericArgumentHead),
    /// Generics must not exist.
    NoGenerics,
}

/// Query implementation of [crate::db::SemanticGroup::module_impl_ids_for_trait_filter].
pub fn module_impl_ids_for_trait_filter(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    trait_filter: TraitFilter,
) -> Maybe<Vec<UninferredImpl>> {
    let mut uninferred_impls = Vec::new();
    if let Ok(impl_ids) = db.module_impls_ids(module_id) {
        uninferred_impls.extend(impl_ids.iter().copied().map(UninferredImpl::Def));
    }
    if let Ok(impl_aliases_ids) = db.module_impl_aliases_ids(module_id) {
        uninferred_impls.extend(impl_aliases_ids.iter().copied().map(UninferredImpl::ImplAlias));
    }
    if let Ok(uses_ids) = db.module_uses_ids(module_id) {
        for use_id in uses_ids.iter().copied() {
            match db.use_resolved_item(use_id) {
                Ok(ResolvedGenericItem::Impl(impl_def_id)) => {
                    uninferred_impls.push(UninferredImpl::Def(impl_def_id));
                }
                Ok(ResolvedGenericItem::GenericImplAlias(impl_alias_id)) => {
                    uninferred_impls.push(UninferredImpl::ImplAlias(impl_alias_id));
                }
                _ => {}
            }
        }
    }
    let mut res = Vec::new();
    for uninferred_impl in uninferred_impls {
        let Ok(trait_id) = uninferred_impl.trait_id(db) else { continue };
        if trait_id != trait_filter.trait_id {
            continue;
        }
        let Ok(concrete_trait_id) = uninferred_impl.concrete_trait(db) else {
            continue;
        };
        if let Ok(true) = concrete_trait_fits_trait_filter(db, concrete_trait_id, &trait_filter) {
            res.push(uninferred_impl);
        }
    }

    Ok(res)
}

/// Cycle handling for [crate::db::SemanticGroup::module_impl_ids_for_trait_filter].
pub fn module_impl_ids_for_trait_filter_cycle(
    _db: &dyn SemanticGroup,
    _cycle: &[String],
    _module_id: &ModuleId,
    _trait_filter: &TraitFilter,
) -> Maybe<Vec<UninferredImpl>> {
    // The diagnostics will be reported from the calling function, specifically from
    // `priv_impl_declaration_data_inner`.
    Err(skip_diagnostic())
}

/// Query implementation of [crate::db::SemanticGroup::impl_impl_ids_for_trait_filter].
pub fn impl_impl_ids_for_trait_filter(
    db: &dyn SemanticGroup,
    impl_id: ImplId,
    trait_filter: TraitFilter,
) -> Maybe<Vec<UninferredImpl>> {
    let mut uninferred_impls = Vec::new();
    for (_, trait_impl_id) in db.trait_impls(impl_id.concrete_trait(db)?.trait_id(db))?.iter() {
        uninferred_impls.push(UninferredImpl::ImplImpl(ImplImplId::new(
            impl_id,
            *trait_impl_id,
            db,
        )));
    }
    let mut res = Vec::new();
    for uninferred_impl in uninferred_impls {
        let Ok(trait_id) = uninferred_impl.trait_id(db) else { continue };
        if trait_id != trait_filter.trait_id {
            continue;
        }
        let Ok(concrete_trait_id) = uninferred_impl.concrete_trait(db) else {
            continue;
        };
        if let Ok(true) = concrete_trait_fits_trait_filter(db, concrete_trait_id, &trait_filter) {
            res.push(uninferred_impl);
        }
    }

    Ok(res)
}
/// Cycle handling for [crate::db::SemanticGroup::impl_impl_ids_for_trait_filter].
pub fn impl_impl_ids_for_trait_filter_cycle(
    _db: &dyn SemanticGroup,
    _cycle: &[String],
    _imp: &ImplId,
    _trait_filter: &TraitFilter,
) -> Maybe<Vec<UninferredImpl>> {
    // The diagnostics will be reported from the calling function, specifically from
    // `priv_impl_declaration_data_inner`.
    Err(skip_diagnostic())
}

/// Checks whether an [ImplDefId] passes a [TraitFilter].
fn concrete_trait_fits_trait_filter(
    db: &dyn SemanticGroup,
    concrete_trait_id: ConcreteTraitId,
    trait_filter: &TraitFilter,
) -> Maybe<bool> {
    if trait_filter.trait_id != concrete_trait_id.trait_id(db) {
        return Ok(false);
    }
    let generic_args = concrete_trait_id.generic_args(db);
    let first_generic = generic_args.first();
    Ok(match &trait_filter.generics_filter {
        GenericsHeadFilter::NoFilter => true,
        GenericsHeadFilter::FirstGenericFilter(constraint_head) => {
            let Some(first_generic) = first_generic else {
                return Ok(false);
            };
            let Some(first_generic_head) = first_generic.head(db) else {
                return Ok(true);
            };
            &first_generic_head == constraint_head
        }
        GenericsHeadFilter::NoGenerics => first_generic.is_none(),
    })
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ImplOrModuleById {
    Impl(ImplId),
    Module(ModuleId),
}
impl From<ImplId> for ImplOrModuleById {
    fn from(impl_id: ImplId) -> Self {
        ImplOrModuleById::Impl(impl_id)
    }
}
impl From<ModuleId> for ImplOrModuleById {
    fn from(module_id: ModuleId) -> Self {
        ImplOrModuleById::Module(module_id)
    }
}

impl Ord for ImplOrModuleById {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (ImplOrModuleById::Impl(imp), ImplOrModuleById::Impl(other_impl)) => {
                imp.get_internal_id().cmp(other_impl.get_internal_id())
            }
            (ImplOrModuleById::Module(module), ImplOrModuleById::Module(other_module)) => {
                match (module, other_module) {
                    (ModuleId::CrateRoot(crate_id), ModuleId::CrateRoot(other_crate_id)) => {
                        crate_id.get_internal_id().cmp(other_crate_id.get_internal_id())
                    }
                    (ModuleId::CrateRoot(_), ModuleId::Submodule(_)) => std::cmp::Ordering::Less,
                    (ModuleId::Submodule(_), ModuleId::CrateRoot(_)) => std::cmp::Ordering::Greater,
                    (ModuleId::Submodule(module_id), ModuleId::Submodule(other_module_id)) => {
                        module_id.get_internal_id().cmp(other_module_id.get_internal_id())
                    }
                }
            }
            (ImplOrModuleById::Impl(_), ImplOrModuleById::Module(_)) => std::cmp::Ordering::Less,
            (ImplOrModuleById::Module(_), ImplOrModuleById::Impl(_)) => std::cmp::Ordering::Greater,
        }
    }
}
impl PartialOrd for ImplOrModuleById {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplLookupContext {
    pub modules_and_impls: BTreeSet<ImplOrModuleById>,
    pub generic_params: Vec<GenericParamId>,
}
impl ImplLookupContext {
    pub fn new(module_id: ModuleId, generic_params: Vec<GenericParamId>) -> ImplLookupContext {
        Self { modules_and_impls: [ImplOrModuleById::Module(module_id)].into(), generic_params }
    }
    pub fn insert_lookup_scope(&mut self, item: ImplOrModuleById) {
        match item {
            ImplOrModuleById::Impl(impl_id) => {
                self.modules_and_impls.insert(ImplOrModuleById::Impl(impl_id));
            }
            ImplOrModuleById::Module(module_id) => {
                self.modules_and_impls.insert(ImplOrModuleById::Module(module_id));
            }
        }
    }
    pub fn insert_module(&mut self, module_id: ModuleId) -> bool {
        self.modules_and_impls.insert(ImplOrModuleById::Module(module_id))
    }

    pub fn insert_impl(&mut self, impl_id: ImplId) -> bool {
        self.modules_and_impls.insert(ImplOrModuleById::Impl(impl_id))
    }
}

/// An candidate impl for later inference.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum UninferredImpl {
    Def(ImplDefId),
    ImplAlias(ImplAliasId),
    GenericParam(GenericParamId),
    ImplImpl(ImplImplId),
    GeneratedImpl(GeneratedImplId),
}
impl UninferredImpl {
    pub fn concrete_trait(&self, db: &dyn SemanticGroup) -> Maybe<ConcreteTraitId> {
        match self {
            UninferredImpl::Def(impl_def_id) => db.impl_def_concrete_trait(*impl_def_id),
            UninferredImpl::ImplAlias(impl_alias_id) => {
                let impl_id = db.impl_alias_resolved_impl(*impl_alias_id)?;
                impl_id.concrete_trait(db)
            }
            UninferredImpl::GenericParam(param) => {
                let param =
                    extract_matches!(db.generic_param_semantic(*param)?, GenericParam::Impl);
                param.concrete_trait
            }
            UninferredImpl::ImplImpl(impl_impl_id) => db.impl_impl_concrete_trait(*impl_impl_id),
            UninferredImpl::GeneratedImpl(generated_impl) => Ok(generated_impl.concrete_trait),
        }
    }

    fn trait_id(&self, db: &dyn SemanticGroup) -> Maybe<TraitId> {
        match self {
            UninferredImpl::Def(impl_def_id) => db.impl_def_trait(*impl_def_id),
            UninferredImpl::ImplAlias(impl_alias_id) => {
                let impl_def_id = db.impl_alias_impl_def(*impl_alias_id)?;
                db.impl_def_trait(impl_def_id)
            }
            UninferredImpl::GenericParam(param) => {
                let param =
                    extract_matches!(db.generic_param_semantic(*param)?, GenericParam::Impl);
                param.concrete_trait.map(|concrete_trait| concrete_trait.trait_id(db))
            }
            UninferredImpl::ImplImpl(impl_impl_id) => db
                .impl_impl_concrete_trait(*impl_impl_id)
                .map(|concrete_trait| concrete_trait.trait_id(db)),
            UninferredImpl::GeneratedImpl(generated_impl) => {
                Ok(generated_impl.concrete_trait.trait_id(db))
            }
        }
    }

    pub fn lookup_scope(&self, db: &dyn SemanticGroup) -> ImplOrModuleById {
        let defs_db = db.upcast();
        match self {
            UninferredImpl::Def(impl_def_id) => impl_def_id.module_file_id(defs_db).0.into(),
            UninferredImpl::ImplAlias(impl_alias_id) => {
                impl_alias_id.module_file_id(defs_db).0.into()
            }
            UninferredImpl::GenericParam(param) => param.module_file_id(defs_db).0.into(),
            UninferredImpl::ImplImpl(impl_impl_id) => impl_impl_id.impl_id.into(),
            UninferredImpl::GeneratedImpl(generated_impl) => {
                generated_impl.concrete_trait.trait_id(db).module_file_id(defs_db).0.into()
            }
        }
    }
}
impl DebugWithDb<dyn SemanticGroup> for UninferredImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn SemanticGroup) -> std::fmt::Result {
        match self {
            UninferredImpl::Def(impl_def) => write!(f, "{:?}", impl_def.full_path(db.upcast())),
            UninferredImpl::ImplAlias(impl_alias) => {
                write!(f, "{:?}", impl_alias.full_path(db.upcast()))
            }
            UninferredImpl::GenericParam(param) => {
                write!(f, "generic param {}", param.name(db.upcast()).unwrap_or_else(|| "_".into()))
            }
            UninferredImpl::ImplImpl(impl_impl) => impl_impl.fmt(f, db.elongate()),
            UninferredImpl::GeneratedImpl(generated_impl) => {
                write!(f, "Generated {}", generated_impl.concrete_trait.full_path(db.upcast()))
            }
        }
    }
}

/// Finds all the implementations of a concrete trait, in a specific lookup context.
pub fn find_candidates_at_context(
    db: &dyn SemanticGroup,
    lookup_context: &ImplLookupContext,
    filter: TraitFilter,
) -> Maybe<OrderedHashSet<UninferredImpl>> {
    let mut res = OrderedHashSet::default();
    for generic_param_id in &lookup_context.generic_params {
        if !matches!(generic_param_id.kind(db.upcast()), GenericKind::Impl) {
            continue;
        };
        let Ok(trait_id) = db.generic_impl_param_trait(*generic_param_id) else {
            continue;
        };
        if filter.trait_id != trait_id {
            continue;
        }
        let Ok(generic_param_semantic) = db.generic_param_semantic(*generic_param_id) else {
            continue;
        };
        let param = extract_matches!(generic_param_semantic, GenericParam::Impl);
        let Ok(imp_concrete_trait_id) = param.concrete_trait else { continue };
        let Ok(trait_fits_filter) =
            concrete_trait_fits_trait_filter(db, imp_concrete_trait_id, &filter)
        else {
            continue;
        };
        if !trait_fits_filter {
            continue;
        }
        res.insert(UninferredImpl::GenericParam(*generic_param_id));
    }
    for module_or_impl_id in &lookup_context.modules_and_impls {
        let Ok(imps) = (match module_or_impl_id {
            ImplOrModuleById::Module(module_id) => {
                db.module_impl_ids_for_trait_filter(*module_id, filter.clone())
            }
            ImplOrModuleById::Impl(impl_id) => {
                db.impl_impl_ids_for_trait_filter(*impl_id, filter.clone())
            }
        }) else {
            continue;
        };
        for imp in imps {
            res.insert(imp);
        }
    }
    Ok(res)
}

/// Checks if an impl of a trait function with a given self_ty exists.
/// This function does not change the state of the inference context.
///
/// `inference_errors` are aggregated here but are not reported here as diagnostics.
/// The caller has to make sure the diagnostics are reported appropriately.
pub fn can_infer_impl_by_self(
    ctx: &ComputationContext<'_>,
    inference_errors: &mut Vec<(TraitFunctionId, InferenceError)>,
    trait_function_id: TraitFunctionId,
    self_ty: TypeId,
    stable_ptr: SyntaxStablePtrId,
) -> bool {
    let mut temp_inference_data = ctx.resolver.data.inference_data.temporary_clone();
    let mut temp_inference = temp_inference_data.inference(ctx.db);
    let lookup_context = ctx.resolver.impl_lookup_context();

    let Some((concrete_trait_id, _)) = temp_inference.infer_concrete_trait_by_self(
        trait_function_id,
        self_ty,
        &lookup_context,
        Some(stable_ptr),
        |err| inference_errors.push((trait_function_id, err)),
    ) else {
        return false;
    };
    // Find impls for it.
    if let Err(err_set) = temp_inference.solve() {
        // Error is propagated and will be reported later.
        if let Some(err) = temp_inference.consume_error_without_reporting(err_set) {
            inference_errors.push((trait_function_id, err));
        }
    }
    match temp_inference.trait_solution_set(concrete_trait_id, lookup_context.clone()) {
        Ok(SolutionSet::Unique(_) | SolutionSet::Ambiguous(_)) => true,
        Ok(SolutionSet::None) => {
            inference_errors
                .push((trait_function_id, InferenceError::NoImplsFound(concrete_trait_id)));
            false
        }
        Err(err_set) => {
            // Error is propagated and will be reported later.
            if let Some(err) = temp_inference.consume_error_without_reporting(err_set) {
                inference_errors.push((trait_function_id, err));
            }
            false
        }
    }
}

/// Returns an impl of a given trait function with a given self_ty, as well as the number of
/// snapshots needed to be added to it.
pub fn infer_impl_by_self(
    ctx: &mut ComputationContext<'_>,
    trait_function_id: TraitFunctionId,
    self_ty: TypeId,
    stable_ptr: SyntaxStablePtrId,
    generic_args_syntax: Option<Vec<GenericArg>>,
) -> Option<(FunctionId, usize)> {
    let lookup_context = ctx.resolver.impl_lookup_context();
    let (concrete_trait_id, n_snapshots) = ctx.resolver.inference().infer_concrete_trait_by_self(
        trait_function_id,
        self_ty,
        &lookup_context,
        Some(stable_ptr),
        |_| {},
    )?;

    let concrete_trait_function_id =
        ConcreteTraitGenericFunctionLongId::new(ctx.db, concrete_trait_id, trait_function_id)
            .intern(ctx.db);
    let trait_func_generic_params =
        ctx.db.concrete_trait_function_generic_params(concrete_trait_function_id).unwrap();
    let generic_args = ctx
        .resolver
        .resolve_generic_args(
            ctx.diagnostics,
            &trait_func_generic_params,
            &generic_args_syntax.unwrap_or_default(),
            stable_ptr,
        )
        .unwrap();

    let impl_lookup_context = ctx.resolver.impl_lookup_context();
    let inference = &mut ctx.resolver.inference();
    let generic_function = inference.infer_trait_generic_function(
        concrete_trait_function_id,
        &impl_lookup_context,
        Some(stable_ptr),
    );

    Some((
        FunctionLongId { function: ConcreteFunction { generic_function, generic_args } }
            .intern(ctx.db),
        n_snapshots,
    ))
}

/// Returns all the trait functions that fit the given function name, can be called on the given
/// `self_ty`, and have at least one implementation in context.
///
/// `inference_errors` are aggregated here but are not reported here as diagnostics.
/// The caller has to make sure the diagnostics are reported appropriately.
pub fn filter_candidate_traits(
    ctx: &mut ComputationContext<'_>,
    inference_errors: &mut Vec<(TraitFunctionId, InferenceError)>,
    self_ty: TypeId,
    candidate_traits: &[TraitId],
    function_name: SmolStr,
    stable_ptr: SyntaxStablePtrId,
) -> Vec<TraitFunctionId> {
    let mut candidates = Vec::new();
    for trait_id in candidate_traits.iter().copied() {
        let Ok(trait_functions) = ctx.db.trait_functions(trait_id) else {
            continue;
        };
        for (name, trait_function) in trait_functions {
            if name == function_name
                && can_infer_impl_by_self(
                    ctx,
                    inference_errors,
                    trait_function,
                    self_ty,
                    stable_ptr,
                )
            {
                candidates.push(trait_function);
            }
        }
    }
    candidates
}

// === Impl Item Type definition ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplItemTypeData {
    type_alias_data: TypeAliasData,
    trait_type_id: Maybe<TraitTypeId>,
    /// The diagnostics of the module type alias, including the ones for the type alias itself.
    diagnostics: Diagnostics<SemanticDiagnostic>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::impl_type_def_semantic_diagnostics].
pub fn impl_type_def_semantic_diagnostics(
    db: &dyn SemanticGroup,
    impl_type_def_id: ImplTypeDefId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_type_semantic_data(impl_type_def_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_type_def_resolved_type].
pub fn impl_type_def_resolved_type(
    db: &dyn SemanticGroup,
    impl_type_def_id: ImplTypeDefId,
) -> Maybe<TypeId> {
    db.priv_impl_type_semantic_data(impl_type_def_id)?.type_alias_data.resolved_type
}

/// Cycle handling for [crate::db::SemanticGroup::impl_type_def_resolved_type].
pub fn impl_type_def_resolved_type_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_type_def_id: &ImplTypeDefId,
) -> Maybe<TypeId> {
    // Forwarding cycle handling to `priv_impl_type_semantic_data` handler.
    impl_type_def_resolved_type(db, *impl_type_def_id)
}

/// Query implementation of [crate::db::SemanticGroup::impl_type_def_generic_params].
pub fn impl_type_def_generic_params(
    db: &dyn SemanticGroup,
    impl_type_def_id: ImplTypeDefId,
) -> Maybe<Vec<GenericParam>> {
    Ok(db.priv_impl_type_def_generic_params_data(impl_type_def_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::impl_type_def_attributes].
pub fn impl_type_def_attributes(
    db: &dyn SemanticGroup,
    impl_type_def_id: ImplTypeDefId,
) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_impl_type_semantic_data(impl_type_def_id)?.type_alias_data.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::impl_type_def_resolver_data].
pub fn impl_type_def_resolver_data(
    db: &dyn SemanticGroup,
    impl_type_def_id: ImplTypeDefId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_impl_type_semantic_data(impl_type_def_id)?.type_alias_data.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::impl_type_def_trait_type].
pub fn impl_type_def_trait_type(
    db: &dyn SemanticGroup,
    impl_type_def_id: ImplTypeDefId,
) -> Maybe<TraitTypeId> {
    db.priv_impl_type_semantic_data(impl_type_def_id)?.trait_type_id
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_impl_type_semantic_data].
pub fn priv_impl_type_semantic_data(
    db: &dyn SemanticGroup,
    impl_type_def_id: ImplTypeDefId,
) -> Maybe<ImplItemTypeData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_type_defs = db.impl_types(impl_type_def_id.impl_def_id(db.upcast()))?;
    let impl_type_def_ast = impl_type_defs.get(&impl_type_def_id).to_maybe()?;
    let generic_params_data = db.priv_impl_type_def_generic_params_data(impl_type_def_id)?;
    let lookup_item_id = LookupItemId::ImplItem(ImplItemId::Type(impl_type_def_id));

    let trait_type_id =
        validate_impl_item_type(db, &mut diagnostics, impl_type_def_id, impl_type_def_ast);

    // TODO(yuval): resolve type aliases later, like in module type aliases, to avoid cycles in
    // non-cyclic chains.
    Ok(ImplItemTypeData {
        type_alias_data: type_alias_semantic_data_helper(
            db,
            &mut diagnostics,
            impl_type_def_ast,
            lookup_item_id,
            generic_params_data,
        )?,
        trait_type_id,
        diagnostics: diagnostics.build(),
    })
}

/// Cycle handling for [crate::db::SemanticGroup::priv_impl_type_semantic_data].
pub fn priv_impl_type_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_type_def_id: &ImplTypeDefId,
) -> Maybe<ImplItemTypeData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_type_defs = db.impl_types(impl_type_def_id.impl_def_id(db.upcast()))?;
    let impl_type_def_ast = impl_type_defs.get(impl_type_def_id).to_maybe()?;
    let generic_params_data = db.priv_impl_type_def_generic_params_data(*impl_type_def_id)?;
    let lookup_item_id = LookupItemId::ImplItem(ImplItemId::Type(*impl_type_def_id));

    let trait_type_id =
        validate_impl_item_type(db, &mut diagnostics, *impl_type_def_id, impl_type_def_ast);

    Ok(ImplItemTypeData {
        type_alias_data: type_alias_semantic_data_cycle_helper(
            db,
            &mut diagnostics,
            impl_type_def_ast,
            lookup_item_id,
            generic_params_data,
        )?,
        trait_type_id,
        diagnostics: diagnostics.build(),
    })
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_type_def_generic_params_data].
pub fn priv_impl_type_def_generic_params_data(
    db: &dyn SemanticGroup,
    impl_type_def_id: ImplTypeDefId,
) -> Maybe<GenericParamsData> {
    let defs_db = db.upcast();
    let module_file_id = impl_type_def_id.module_file_id(defs_db);
    let impl_type_def_ast = db.impl_type_by_id(impl_type_def_id)?.to_maybe()?;
    let lookup_item_id = LookupItemId::ImplItem(ImplItemId::Type(impl_type_def_id));

    let impl_resolver_data = db.impl_def_resolver_data(impl_type_def_id.impl_def_id(defs_db))?;
    type_alias_generic_params_data_helper(
        db,
        module_file_id,
        &impl_type_def_ast,
        lookup_item_id,
        Some(impl_resolver_data),
    )
}

/// Validates the impl item type, and returns the matching trait type id.
fn validate_impl_item_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    impl_type_def_id: ImplTypeDefId,
    impl_type_ast: &ast::ItemTypeAlias,
) -> Maybe<TraitTypeId> {
    let defs_db = db.upcast();
    let syntax_db = db.upcast();
    let impl_def_id = impl_type_def_id.impl_def_id(defs_db);
    let concrete_trait_id = db.impl_def_concrete_trait(impl_def_id)?;
    let trait_id = concrete_trait_id.trait_id(db);
    let type_name = impl_type_def_id.name(defs_db);
    let trait_type_id = db.trait_type_by_name(trait_id, type_name.clone())?.ok_or_else(|| {
        diagnostics.report(
            impl_type_ast,
            ImplItemNotInTrait {
                impl_def_id,
                impl_item_name: type_name,
                trait_id,
                item_kind: "type".into(),
            },
        )
    })?;

    // TODO(yuval): add validations for generic parameters, then remove this.
    // Generic parameters are not yet supported, make sure there are none.
    let generic_params_node = impl_type_ast.generic_params(syntax_db);
    if !generic_params_node.is_empty(syntax_db) {
        diagnostics.report(
            &generic_params_node,
            GenericsNotSupportedInItem { scope: "Impl".into(), item_kind: "type".into() },
        );
    }

    Ok(trait_type_id)
}

// === Impl Type ===

/// Query implementation of [crate::db::SemanticGroup::impl_type_concrete_implized].
pub fn impl_type_concrete_implized(
    db: &dyn SemanticGroup,
    impl_type_id: ImplTypeId,
) -> Maybe<TypeId> {
    let concrete_impl = match impl_type_id.impl_id().lookup_intern(db) {
        ImplLongId::Concrete(concrete_impl) => concrete_impl,
        ImplLongId::ImplImpl(imp_impl_id) => {
            let ImplLongId::Concrete(concrete_impl) =
                db.impl_impl_concrete_implized(imp_impl_id)?.lookup_intern(db)
            else {
                return Ok(TypeLongId::ImplType(impl_type_id).intern(db));
            };
            concrete_impl
        }
        ImplLongId::GenericParameter(_)
        | ImplLongId::TraitImpl(_)
        | ImplLongId::ImplVar(_)
        | ImplLongId::GeneratedImpl(_) => {
            return Ok(TypeLongId::ImplType(impl_type_id).intern(db));
        }
    };

    let impl_def_id = concrete_impl.impl_def_id(db);
    let ty = db.trait_type_implized_by_context(impl_type_id.ty(), impl_def_id);
    let Ok(ty) = ty else {
        return ty;
    };

    let substitution = &concrete_impl.substitution(db)?;
    SubstitutionRewriter { db, substitution }.rewrite(ty)
}

/// Cycle handling for [crate::db::SemanticGroup::impl_type_concrete_implized].
pub fn impl_type_concrete_implized_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_type_id: &ImplTypeId,
) -> Maybe<TypeId> {
    // Forwarding cycle handling to `priv_impl_type_semantic_data` handler.
    impl_type_concrete_implized(db, *impl_type_id)
}

// === Impl Item Constant definition ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplItemConstantData {
    constant_data: ConstantData,
    trait_constant_id: Maybe<TraitConstantId>,
    /// The diagnostics of the impl constant, including the ones for the constant itself.
    diagnostics: Diagnostics<SemanticDiagnostic>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::impl_constant_def_semantic_diagnostics].
pub fn impl_constant_def_semantic_diagnostics(
    db: &dyn SemanticGroup,
    impl_constant_def_id: ImplConstantDefId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_constant_semantic_data(impl_constant_def_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_constant_def_value].
pub fn impl_constant_def_value(
    db: &dyn SemanticGroup,
    impl_constant_def_id: ImplConstantDefId,
) -> Maybe<ConstValueId> {
    Ok(db
        .priv_impl_constant_semantic_data(impl_constant_def_id)?
        .constant_data
        .const_value
        .intern(db))
}

/// Cycle handling for [crate::db::SemanticGroup::impl_constant_def_value].
pub fn impl_constant_def_value_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_constant_def_id: &ImplConstantDefId,
) -> Maybe<ConstValueId> {
    // Forwarding cycle handling to `priv_impl_constant_semantic_data` handler.
    impl_constant_def_value(db, *impl_constant_def_id)
}

/// Query implementation of [crate::db::SemanticGroup::impl_constant_def_resolver_data].
pub fn impl_constant_def_resolver_data(
    db: &dyn SemanticGroup,
    impl_constant_def_id: ImplConstantDefId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_impl_constant_semantic_data(impl_constant_def_id)?.constant_data.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::impl_constant_def_trait_constant].
pub fn impl_constant_def_trait_constant(
    db: &dyn SemanticGroup,
    impl_constant_def_id: ImplConstantDefId,
) -> Maybe<TraitConstantId> {
    db.priv_impl_constant_semantic_data(impl_constant_def_id)?.trait_constant_id
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_impl_constant_semantic_data].
pub fn priv_impl_constant_semantic_data(
    db: &dyn SemanticGroup,
    impl_constant_def_id: ImplConstantDefId,
) -> Maybe<ImplItemConstantData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_def_id = impl_constant_def_id.impl_def_id(db.upcast());
    let impl_constant_defs = db.impl_constants(impl_def_id)?;
    let impl_constant_def_ast = impl_constant_defs.get(&impl_constant_def_id).to_maybe()?;
    let lookup_item_id = LookupItemId::ImplItem(ImplItemId::Constant(impl_constant_def_id));

    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ImplItem(
        ImplItemId::Constant(impl_constant_def_id),
    ));
    let resolver_data = db.impl_def_resolver_data(impl_def_id)?;
    let mut resolver =
        Resolver::with_data(db, resolver_data.clone_with_inference_id(db, inference_id));

    let trait_constant_id = validate_impl_item_constant(
        db,
        &mut diagnostics,
        impl_constant_def_id,
        impl_constant_def_ast,
        &mut resolver,
    );
    let mut constant_data = constant_semantic_data_helper(
        db,
        impl_constant_def_ast,
        lookup_item_id,
        Some(Arc::new(resolver.data)),
        &impl_def_id,
    )?;
    diagnostics.extend(mem::take(&mut constant_data.diagnostics));
    Ok(ImplItemConstantData { constant_data, trait_constant_id, diagnostics: diagnostics.build() })
}

/// Cycle handling for [crate::db::SemanticGroup::priv_impl_constant_semantic_data].
pub fn priv_impl_constant_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_constant_def_id: &ImplConstantDefId,
) -> Maybe<ImplItemConstantData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_def_id = impl_constant_def_id.impl_def_id(db.upcast());
    let impl_constant_defs = db.impl_constants(impl_def_id)?;
    let impl_constant_def_ast = impl_constant_defs.get(impl_constant_def_id).to_maybe()?;
    let lookup_item_id = LookupItemId::ImplItem(ImplItemId::Constant(*impl_constant_def_id));

    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ImplItem(
        ImplItemId::Constant(*impl_constant_def_id),
    ));
    let resolver_data = db.impl_def_resolver_data(impl_def_id)?;
    let mut resolver =
        Resolver::with_data(db, resolver_data.clone_with_inference_id(db, inference_id));

    let trait_constant_id = validate_impl_item_constant(
        db,
        &mut diagnostics,
        *impl_constant_def_id,
        impl_constant_def_ast,
        &mut resolver,
    );
    let mut constant_data = constant_semantic_data_cycle_helper(
        db,
        impl_constant_def_ast,
        lookup_item_id,
        Some(Arc::new(resolver.data)),
        &impl_def_id,
    )?;
    diagnostics.extend(mem::take(&mut constant_data.diagnostics));
    Ok(ImplItemConstantData { constant_data, trait_constant_id, diagnostics: diagnostics.build() })
}

/// Validates the impl item constant, and returns the matching trait constant id.
fn validate_impl_item_constant(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    impl_constant_def_id: ImplConstantDefId,
    impl_constant_ast: &ast::ItemConstant,
    resolver: &mut Resolver<'_>,
) -> Maybe<TraitConstantId> {
    let syntax_db = db.upcast();
    let defs_db = db.upcast();
    let impl_def_id = impl_constant_def_id.impl_def_id(defs_db);
    let concrete_trait_id = db.impl_def_concrete_trait(impl_def_id)?;
    let trait_id = concrete_trait_id.trait_id(db);
    let constant_name = impl_constant_def_id.name(defs_db);

    let trait_constant_id =
        db.trait_constant_by_name(trait_id, constant_name.clone())?.ok_or_else(|| {
            diagnostics.report(
                impl_constant_ast,
                ImplItemNotInTrait {
                    impl_def_id,
                    impl_item_name: constant_name,
                    trait_id,
                    item_kind: "const".into(),
                },
            )
        })?;
    let concrete_trait_constant =
        ConcreteTraitConstantId::new(db, concrete_trait_id, trait_constant_id);
    let concrete_trait_constant_ty = db.concrete_trait_constant_type(concrete_trait_constant)?;

    let impl_constant_type_clause_ast = impl_constant_ast.type_clause(syntax_db);

    let constant_ty =
        resolve_type(db, diagnostics, resolver, &impl_constant_type_clause_ast.ty(syntax_db));

    let inference = &mut resolver.inference();

    let expected_ty = inference.rewrite(concrete_trait_constant_ty).no_err();
    let actual_ty = inference.rewrite(constant_ty).no_err();
    if expected_ty != actual_ty {
        diagnostics.report(&impl_constant_type_clause_ast, WrongType { expected_ty, actual_ty });
    }
    Ok(trait_constant_id)
}

// === Impl Constant ===

/// Query implementation of [crate::db::SemanticGroup::impl_constant_implized_by_context].
pub fn impl_constant_implized_by_context(
    db: &dyn SemanticGroup,
    impl_constant_id: ImplConstantId,
    impl_def_id: ImplDefId,
) -> Maybe<ConstValueId> {
    let impl_constant_def_id =
        db.impl_constant_by_trait_constant(impl_def_id, impl_constant_id.trait_constant_id())?;

    db.impl_constant_def_value(impl_constant_def_id)
}

/// Cycle handling for [crate::db::SemanticGroup::impl_constant_implized_by_context].
pub fn impl_constant_implized_by_context_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_constant_id: &ImplConstantId,
    impl_def_id: &ImplDefId,
) -> Maybe<ConstValueId> {
    // Forwarding cycle handling to `priv_impl_constant_semantic_data` handler.
    impl_constant_implized_by_context(db, *impl_constant_id, *impl_def_id)
}

/// Query implementation of [crate::db::SemanticGroup::impl_constant_concrete_implized_value].
pub fn impl_constant_concrete_implized_value(
    db: &dyn SemanticGroup,
    impl_constant_id: ImplConstantId,
) -> Maybe<ConstValueId> {
    if let ImplLongId::Concrete(concrete_impl) = impl_constant_id.impl_id().lookup_intern(db) {
        let impl_def_id = concrete_impl.impl_def_id(db);
        let constant = db.impl_constant_implized_by_context(impl_constant_id, impl_def_id)?;
        let substitution: &GenericSubstitution = &concrete_impl.substitution(db)?;
        return SubstitutionRewriter { db, substitution }.rewrite(constant);
    }
    let substitution = &GenericSubstitution::from_impl(impl_constant_id.impl_id());

    Ok(ConstValue::ImplConstant(
        SubstitutionRewriter { db, substitution }.rewrite(impl_constant_id)?,
    )
    .intern(db))
}

/// Cycle handling for [crate::db::SemanticGroup::impl_constant_concrete_implized_value].
pub fn impl_constant_concrete_implized_value_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_constant_id: &ImplConstantId,
) -> Maybe<ConstValueId> {
    // Forwarding cycle handling to `priv_impl_const_semantic_data` handler.
    impl_constant_concrete_implized_value(db, *impl_constant_id)
}

/// Query implementation of [crate::db::SemanticGroup::impl_constant_concrete_implized_type].
pub fn impl_constant_concrete_implized_type(
    db: &dyn SemanticGroup,
    impl_constant_id: ImplConstantId,
) -> Maybe<TypeId> {
    let concrete_trait_id = match impl_constant_id.impl_id().lookup_intern(db) {
        ImplLongId::Concrete(concrete_impl) => {
            let impl_def_id = concrete_impl.impl_def_id(db);
            let ty = db.impl_constant_implized_by_context(impl_constant_id, impl_def_id)?.ty(db)?;
            let substitution = &concrete_impl.substitution(db)?;
            return SubstitutionRewriter { db, substitution }.rewrite(ty);
        }
        ImplLongId::GenericParameter(param) => {
            let param_impl =
                extract_matches!(db.generic_param_semantic(param)?, GenericParam::Impl);
            param_impl.concrete_trait?
        }
        ImplLongId::ImplVar(var) => var.lookup_intern(db).concrete_trait_id,
        ImplLongId::ImplImpl(impl_impl) => db.impl_impl_concrete_trait(impl_impl)?,
        ImplLongId::TraitImpl(trait_impl) => db.trait_impl_concrete_trait(trait_impl)?,
        ImplLongId::GeneratedImpl(generated_impl) => generated_impl.concrete_trait,
    };

    let ty = db.concrete_trait_constant_type(ConcreteTraitConstantId::new(
        db,
        concrete_trait_id,
        impl_constant_id.trait_constant_id(),
    ))?;
    let substitution = &GenericSubstitution::from_impl(impl_constant_id.impl_id());
    SubstitutionRewriter { db, substitution }.rewrite(ty)
}

/// Cycle handling for [crate::db::SemanticGroup::impl_constant_concrete_implized_type].
pub fn impl_constant_concrete_implized_type_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_constant_id: &ImplConstantId,
) -> Maybe<TypeId> {
    // Forwarding cycle handling to `priv_impl_const_semantic_data` handler.
    impl_constant_concrete_implized_type(db, *impl_constant_id)
}

// === Impl Item Impl definition ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplItemImplData {
    impl_data: ImplAliasData,
    trait_impl_id: Maybe<TraitImplId>,
    /// The diagnostics of the impl impl, including the ones for the impl itself.
    diagnostics: Diagnostics<SemanticDiagnostic>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::impl_impl_def_semantic_diagnostics].
pub fn impl_impl_def_semantic_diagnostics(
    db: &dyn SemanticGroup,
    impl_impl_def_id: ImplImplDefId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_impl_semantic_data(impl_impl_def_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_impl_def_resolver_data].
pub fn impl_impl_def_resolver_data(
    db: &dyn SemanticGroup,
    impl_impl_def_id: ImplImplDefId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_impl_impl_semantic_data(impl_impl_def_id)?.impl_data.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::impl_impl_def_trait_impl].
pub fn impl_impl_def_trait_impl(
    db: &dyn SemanticGroup,
    impl_impl_def_id: ImplImplDefId,
) -> Maybe<TraitImplId> {
    db.priv_impl_impl_semantic_data(impl_impl_def_id)?.trait_impl_id
}

/// Query implementation of [crate::db::SemanticGroup::impl_impl_def_impl].
pub fn impl_impl_def_impl(
    db: &dyn SemanticGroup,
    impl_impl_def_id: ImplImplDefId,
) -> Maybe<ImplId> {
    db.priv_impl_impl_semantic_data(impl_impl_def_id)?.impl_data.resolved_impl
}
/// Cycle handling for [crate::db::SemanticGroup::impl_impl_def_impl].
pub fn impl_impl_def_impl_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_impl_def_id: &ImplImplDefId,
) -> Maybe<ImplId> {
    db.priv_impl_impl_semantic_data(*impl_impl_def_id)?.impl_data.resolved_impl
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_impl_impl_semantic_data].
pub fn priv_impl_impl_semantic_data(
    db: &dyn SemanticGroup,
    impl_impl_def_id: ImplImplDefId,
) -> Maybe<ImplItemImplData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_def_id = impl_impl_def_id.impl_def_id(db.upcast());
    let impl_impl_defs = db.impl_impls(impl_def_id)?;
    let impl_impl_def_ast = impl_impl_defs.get(&impl_impl_def_id).to_maybe()?;
    let generic_params_data = db.priv_impl_impl_def_generic_params_data(impl_impl_def_id)?;
    let lookup_item_id = LookupItemId::ImplItem(ImplItemId::Impl(impl_impl_def_id));

    let inference_id = InferenceId::LookupItemGenerics(lookup_item_id);
    let resolver_data = db.impl_def_resolver_data(impl_def_id)?;
    let mut resolver =
        Resolver::with_data(db, resolver_data.clone_with_inference_id(db, inference_id));

    let trait_impl_id =
        validate_impl_item_impl(db, &mut diagnostics, impl_impl_def_id, impl_impl_def_ast);
    let mut impl_data = impl_alias_semantic_data_helper(
        db,
        impl_impl_def_ast,
        lookup_item_id,
        generic_params_data,
    )?;
    diagnostics.extend(mem::take(&mut impl_data.diagnostics));

    let trait_impl_concrete_trait = trait_impl_id
        .and_then(|id| db.trait_impl_concrete_trait(id))
        .and_then(|concrete_trait_id| {
            let impl_def_substitution = db.impl_def_substitution(impl_def_id)?;
            SubstitutionRewriter { db, substitution: impl_def_substitution.as_ref() }
                .rewrite(concrete_trait_id)
        });
    let resolved_impl_concrete_trait =
        impl_data.resolved_impl.and_then(|imp| imp.concrete_trait(db));
    // used an IIFE to allow the use of the `?` operator.
    let _ = (|| -> Result<(), DiagnosticAdded> {
        if resolver
            .inference()
            .conform_traits(resolved_impl_concrete_trait?, trait_impl_concrete_trait?)
            .is_err()
        {
            diagnostics.report(
                impl_impl_def_ast,
                TraitMismatch {
                    expected_trt: trait_impl_concrete_trait?,
                    actual_trt: resolved_impl_concrete_trait?,
                },
            );
        }
        Ok(())
    })();

    Ok(ImplItemImplData { impl_data, trait_impl_id, diagnostics: diagnostics.build() })
}

/// Cycle handling for [crate::db::SemanticGroup::priv_impl_impl_semantic_data].
pub fn priv_impl_impl_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_impl_def_id: &ImplImplDefId,
) -> Maybe<ImplItemImplData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_def_id = impl_impl_def_id.impl_def_id(db.upcast());
    let impl_impl_defs = db.impl_impls(impl_def_id)?;
    let impl_impl_def_ast = impl_impl_defs.get(impl_impl_def_id).to_maybe()?;
    let generic_params_data = db.priv_impl_impl_def_generic_params_data(*impl_impl_def_id)?;
    let lookup_item_id = LookupItemId::ImplItem(ImplItemId::Impl(*impl_impl_def_id));

    let inference_id = InferenceId::LookupItemGenerics(lookup_item_id);
    let resolver_data = db.impl_def_resolver_data(impl_def_id)?;
    let mut resolver =
        Resolver::with_data(db, resolver_data.clone_with_inference_id(db, inference_id));

    let trait_impl_id =
        validate_impl_item_impl(db, &mut diagnostics, *impl_impl_def_id, impl_impl_def_ast);
    let mut impl_data = impl_alias_semantic_data_cycle_helper(
        db,
        impl_impl_def_ast,
        lookup_item_id,
        generic_params_data,
    )?;

    diagnostics.extend(mem::take(&mut impl_data.diagnostics));

    let trait_impl_concrete_trait = trait_impl_id.and_then(|id| db.trait_impl_concrete_trait(id));
    let resolved_impl_concrete_trait =
        impl_data.resolved_impl.and_then(|imp| imp.concrete_trait(db));
    // used an IIFE to allow the use of the `?` operator.
    let _ = (|| -> Result<(), DiagnosticAdded> {
        if resolver
            .inference()
            .conform_traits(resolved_impl_concrete_trait?, trait_impl_concrete_trait?)
            .is_err()
        {
            diagnostics.report(
                &impl_impl_def_ast.clone(),
                TraitMismatch {
                    expected_trt: trait_impl_concrete_trait?,
                    actual_trt: resolved_impl_concrete_trait?,
                },
            );
        }
        Ok(())
    })();

    Ok(ImplItemImplData { impl_data, trait_impl_id, diagnostics: diagnostics.build() })
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_impl_def_generic_params_data].
pub fn priv_impl_impl_def_generic_params_data(
    db: &dyn SemanticGroup,
    impl_impl_def_id: ImplImplDefId,
) -> Maybe<GenericParamsData> {
    let defs_db = db.upcast();
    let module_file_id = impl_impl_def_id.module_file_id(defs_db);
    let impl_impl_def_ast = db.impl_impl_by_id(impl_impl_def_id)?.to_maybe()?;
    let lookup_item_id = LookupItemId::ImplItem(ImplItemId::Impl(impl_impl_def_id));

    let impl_resolver_data = db.impl_def_resolver_data(impl_impl_def_id.impl_def_id(defs_db))?;
    impl_alias_generic_params_data_helper(
        db,
        module_file_id,
        &impl_impl_def_ast,
        lookup_item_id,
        Some(impl_resolver_data),
    )
}

/// Validates the impl item impl, and returns the matching trait impl id.
fn validate_impl_item_impl(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    impl_impl_def_id: ImplImplDefId,
    impl_impl_ast: &ast::ItemImplAlias,
) -> Maybe<TraitImplId> {
    let syntax_db = db.upcast();
    let defs_db = db.upcast();
    let impl_def_id = impl_impl_def_id.impl_def_id(defs_db);
    let concrete_trait_id = db.impl_def_concrete_trait(impl_def_id)?;
    let trait_id = concrete_trait_id.trait_id(db);
    let impl_name = impl_impl_def_id.name(defs_db);
    let trait_impl_id = db.trait_impl_by_name(trait_id, impl_name.clone())?.ok_or_else(|| {
        diagnostics.report(
            impl_impl_ast,
            ImplItemNotInTrait {
                impl_def_id,
                impl_item_name: impl_name,
                trait_id,
                item_kind: "impl".into(),
            },
        )
    })?;

    // TODO(TomerStarkware): add validations for generic parameters, then remove this.
    // Generic parameters are not yet supported, make sure there are none.
    let generic_params_node = impl_impl_ast.generic_params(syntax_db);
    if !generic_params_node.is_empty(syntax_db) {
        diagnostics.report(
            &generic_params_node,
            GenericsNotSupportedInItem { scope: "Impl".into(), item_kind: "impl".into() },
        );
    }
    Ok(trait_impl_id)
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplicitImplImplData {
    resolved_impl: Maybe<ImplId>,
    trait_impl_id: TraitImplId,
    diagnostics: Diagnostics<SemanticDiagnostic>,
}

/// Query implementation of [crate::db::SemanticGroup::implicit_impl_impl_semantic_diagnostics].
pub fn implicit_impl_impl_semantic_diagnostics(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    trait_impl_id: TraitImplId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_implicit_impl_impl_semantic_data(impl_def_id, trait_impl_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}
/// Query implementation of [crate::db::SemanticGroup::implicit_impl_impl_impl].
pub fn implicit_impl_impl_impl(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    trait_impl_id: TraitImplId,
) -> Maybe<ImplId> {
    db.priv_implicit_impl_impl_semantic_data(impl_def_id, trait_impl_id)?.resolved_impl
}
/// Query implementation of [crate::db::SemanticGroup::implicit_impl_impl_impl].
pub fn implicit_impl_impl_impl_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_def_id: &ImplDefId,
    trait_impl_id: &TraitImplId,
) -> Maybe<ImplId> {
    db.priv_implicit_impl_impl_semantic_data(*impl_def_id, *trait_impl_id)?.resolved_impl
}

/// Query implementation of [crate::db::SemanticGroup::priv_implicit_impl_impl_semantic_data].
pub fn priv_implicit_impl_impl_semantic_data(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
    trait_impl_id: TraitImplId,
) -> Maybe<ImplicitImplImplData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::Impl(impl_def_id));

    let inference_id = InferenceId::LookupItemGenerics(lookup_item_id);
    let resolver_data = db.impl_def_resolver_data(impl_def_id)?;

    let mut resolver =
        Resolver::with_data(db, resolver_data.clone_with_inference_id(db, inference_id));
    // We cannot use `Self` as it will always find the implicit impl.
    resolver.trait_or_impl_ctx = TraitOrImplContext::None;
    let trait_impl_concrete_trait =
        db.trait_impl_concrete_trait(trait_impl_id).and_then(|concrete_trait_id| {
            let impl_def_substitution = db.impl_def_substitution(impl_def_id)?;
            SubstitutionRewriter { db, substitution: impl_def_substitution.as_ref() }
                .rewrite(concrete_trait_id)
        });
    let impl_lookup_context = resolver.impl_lookup_context();
    let resolved_impl = trait_impl_concrete_trait.map(|concrete_trait_id| {
        let imp = resolver.inference().new_impl_var(concrete_trait_id, None, impl_lookup_context);
        if let Err((err_set, _)) = resolver.inference().finalize_without_reporting() {
            diagnostics.report(
                impl_def_id.stable_ptr(db.upcast()).untyped(),
                ImplicitImplNotInferred { trait_impl_id, concrete_trait_id },
            );
            resolver.inference().report_on_pending_error(
                err_set,
                &mut diagnostics,
                impl_def_id.stable_ptr(db.upcast()).untyped(),
            );
        };
        imp
    });

    Ok(ImplicitImplImplData { resolved_impl, trait_impl_id, diagnostics: diagnostics.build() })
}
/// Cycle handling for [crate::db::SemanticGroup::priv_implicit_impl_impl_semantic_data].
pub fn priv_implicit_impl_impl_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_def_id: &ImplDefId,
    trait_impl_id: &TraitImplId,
) -> Maybe<ImplicitImplImplData> {
    // TODO(TomerStarkware): Add test case for implicit impl cycle if possible.
    let mut diagnostics = SemanticDiagnostics::default();
    let err =
        Err(diagnostics.report(impl_def_id.stable_ptr(db.upcast()).untyped(), ImplAliasCycle));
    Ok(ImplicitImplImplData {
        resolved_impl: err,
        trait_impl_id: *trait_impl_id,
        diagnostics: diagnostics.build(),
    })
}

// === Impl Impl ===

/// Query implementation of [crate::db::SemanticGroup::impl_impl_implized_by_context].
pub fn impl_impl_implized_by_context(
    db: &dyn SemanticGroup,
    impl_impl_id: ImplImplId,
    impl_def_id: ImplDefId,
) -> Maybe<ImplId> {
    if db.is_implicit_impl_impl(impl_def_id, impl_impl_id.trait_impl_id())? {
        return db.implicit_impl_impl_impl(impl_def_id, impl_impl_id.trait_impl_id());
    }

    let impl_impl_def_id = db.impl_impl_by_trait_impl(impl_def_id, impl_impl_id.trait_impl_id())?;

    db.impl_impl_def_impl(impl_impl_def_id)
}

/// Cycle handling for [crate::db::SemanticGroup::impl_impl_implized_by_context].
pub fn impl_impl_implized_by_context_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_impl_id: &ImplImplId,
    impl_def_id: &ImplDefId,
) -> Maybe<ImplId> {
    // Forwarding cycle handling to `priv_impl_impl_semantic_data` handler.
    impl_impl_implized_by_context(db, *impl_impl_id, *impl_def_id)
}

/// Query implementation of [crate::db::SemanticGroup::impl_impl_concrete_implized].
pub fn impl_impl_concrete_implized(
    db: &dyn SemanticGroup,
    impl_impl_id: ImplImplId,
) -> Maybe<ImplId> {
    if let ImplLongId::Concrete(concrete_impl) = impl_impl_id.impl_id().lookup_intern(db) {
        let impl_def_id = concrete_impl.impl_def_id(db);
        let imp = db.impl_impl_implized_by_context(impl_impl_id, impl_def_id)?;
        let substitution: &GenericSubstitution = &concrete_impl.substitution(db)?;
        return SubstitutionRewriter { db, substitution }.rewrite(imp);
    }
    let substitution = &GenericSubstitution::from_impl(impl_impl_id.impl_id());

    Ok(ImplLongId::ImplImpl(SubstitutionRewriter { db, substitution }.rewrite(impl_impl_id)?)
        .intern(db))
}
/// Query implementation of [crate::db::SemanticGroup::impl_impl_concrete_trait].
pub fn impl_impl_concrete_trait(
    db: &dyn SemanticGroup,
    impl_impl_id: ImplImplId,
) -> Maybe<ConcreteTraitId> {
    let substitution = GenericSubstitution::from_impl(impl_impl_id.impl_id());
    let impl_concrete_trait_id = db.trait_impl_concrete_trait(impl_impl_id.trait_impl_id())?;
    SubstitutionRewriter { db, substitution: &substitution }.rewrite(impl_concrete_trait_id)
}

/// Cycle handling for [crate::db::SemanticGroup::impl_impl_concrete_implized].
pub fn impl_impl_concrete_implized_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    impl_impl_id: &ImplImplId,
) -> Maybe<ImplId> {
    // Forwarding cycle handling to `priv_impl_const_semantic_data` handler.
    impl_impl_concrete_implized(db, *impl_impl_id)
}

// === Impl Function Declaration ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplFunctionDeclarationData {
    pub function_declaration_data: FunctionDeclarationData,
    trait_function_id: Maybe<TraitFunctionId>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::impl_function_declaration_diagnostics].
pub fn impl_function_declaration_diagnostics(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_function_declaration_data(impl_function_id)
        .map(|data| data.function_declaration_data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_signature].
pub fn impl_function_signature(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<semantic::Signature> {
    Ok(db
        .priv_impl_function_declaration_data(impl_function_id)?
        .function_declaration_data
        .signature)
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_generic_params].
pub fn impl_function_generic_params(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<Vec<semantic::GenericParam>> {
    Ok(db.priv_impl_function_generic_params_data(impl_function_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_function_generic_params_data].
pub fn priv_impl_function_generic_params_data(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<GenericParamsData> {
    let module_file_id = impl_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_def_id = impl_function_id.impl_def_id(db.upcast());
    let data = db.priv_impl_definition_data(impl_def_id)?;
    let function_syntax = &data.function_asts[&impl_function_id];
    let syntax_db = db.upcast();
    let declaration = function_syntax.declaration(syntax_db);
    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::ImplItem(
        ImplItemId::Function(impl_function_id),
    ));
    let resolver_data = db.impl_def_resolver_data(impl_def_id)?;
    let mut resolver =
        Resolver::with_data(db, resolver_data.clone_with_inference_id(db, inference_id));
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &declaration.generic_params(syntax_db),
    );
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, function_syntax.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { generic_params, diagnostics: diagnostics.build(), resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_attributes].
pub fn impl_function_attributes(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<Vec<Attribute>> {
    Ok(db
        .priv_impl_function_declaration_data(impl_function_id)?
        .function_declaration_data
        .attributes)
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_resolver_data].
pub fn impl_function_resolver_data(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db
        .priv_impl_function_declaration_data(impl_function_id)?
        .function_declaration_data
        .resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_declaration_inline_config].
pub fn impl_function_declaration_inline_config(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<InlineConfiguration> {
    Ok(db
        .priv_impl_function_declaration_data(impl_function_id)?
        .function_declaration_data
        .inline_config)
}

/// Query implementation of [SemanticGroup::impl_function_declaration_implicit_precedence].
pub fn impl_function_declaration_implicit_precedence(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<ImplicitPrecedence> {
    Ok(db
        .priv_impl_function_declaration_data(impl_function_id)?
        .function_declaration_data
        .implicit_precedence)
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_declaration_implicits].
pub fn impl_function_declaration_implicits(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<Vec<TypeId>> {
    Ok(db
        .priv_impl_function_declaration_data(impl_function_id)?
        .function_declaration_data
        .signature
        .implicits)
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_trait_function].
pub fn impl_function_trait_function(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<TraitFunctionId> {
    db.priv_impl_function_declaration_data(impl_function_id)?.trait_function_id
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_impl_function_declaration_data].
pub fn priv_impl_function_declaration_data(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<ImplFunctionDeclarationData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_def_id = impl_function_id.impl_def_id(db.upcast());
    let data = db.priv_impl_definition_data(impl_def_id)?;
    let function_syntax = &data.function_asts[&impl_function_id];
    let syntax_db = db.upcast();
    let declaration = function_syntax.declaration(syntax_db);

    let generic_params_data = db.priv_impl_function_generic_params_data(impl_function_id)?;
    let generic_params = generic_params_data.generic_params;
    let lookup_item_id = LookupItemId::ImplItem(ImplItemId::Function(impl_function_id));
    let inference_id = InferenceId::LookupItemGenerics(lookup_item_id);
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);
    resolver.set_feature_config(&impl_function_id, function_syntax, &mut diagnostics);

    let signature_syntax = declaration.signature(syntax_db);

    let mut environment = Environment::empty();
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        FunctionTitleId::Impl(impl_function_id),
        &mut environment,
    );

    let attributes = function_syntax.attributes(syntax_db).structurize(syntax_db);
    let (implicit_precedence, _) =
        get_implicit_precedence(&mut diagnostics, &mut resolver, &attributes);

    let inference = &mut resolver.inference();
    // Check fully resolved.
    inference.finalize(&mut diagnostics, function_syntax.stable_ptr().untyped());
    let trait_function_id = validate_impl_function_signature(
        db,
        &mut diagnostics,
        inference,
        ValidateImplFunctionSignatureParams {
            impl_function_id,
            signature_syntax: &signature_syntax,
            signature: &signature,
            impl_function_syntax: function_syntax,
            impl_func_generics: &generic_params,
        },
    );

    let inline_config = get_inline_config(db, &mut diagnostics, &attributes)?;

    forbid_inline_always_with_impl_generic_param(&mut diagnostics, &generic_params, &inline_config);

    let signature = inference.rewrite(signature).no_err();
    let generic_params = inference.rewrite(generic_params).no_err();

    let resolver_data = Arc::new(resolver.data);
    Ok(ImplFunctionDeclarationData {
        function_declaration_data: FunctionDeclarationData {
            diagnostics: diagnostics.build(),
            signature,
            generic_params,
            environment,
            attributes,
            resolver_data,
            inline_config,
            implicit_precedence,
        },
        trait_function_id,
    })
}

/// Struct for the parameters of [validate_impl_function_signature].
struct ValidateImplFunctionSignatureParams<'a> {
    /// The impl function to validate the signature of.
    impl_function_id: ImplFunctionId,
    /// The signature syntax.
    signature_syntax: &'a ast::FunctionSignature,
    // The semantic signature.
    signature: &'a semantic::Signature,
    /// The impl function syntax.
    impl_function_syntax: &'a ast::FunctionWithBody,
    /// The generic parameters of the impl function.
    impl_func_generics: &'a [GenericParam],
}

/// Validates the impl function, and returns the matching trait function id.
fn validate_impl_function_signature(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    inference: &mut Inference<'_>,
    ValidateImplFunctionSignatureParams {
        impl_function_id,
        signature_syntax,
        signature,
        impl_function_syntax,
        impl_func_generics,
    }: ValidateImplFunctionSignatureParams<'_>,
) -> Maybe<TraitFunctionId> {
    let syntax_db = db.upcast();
    let defs_db = db.upcast();
    let impl_def_id = impl_function_id.impl_def_id(defs_db);
    let concrete_trait_id = db.impl_def_concrete_trait(impl_def_id)?;
    let trait_id = concrete_trait_id.trait_id(db);
    let function_name = impl_function_id.name(defs_db);
    let trait_function_id =
        db.trait_function_by_name(trait_id, function_name.clone())?.ok_or_else(|| {
            diagnostics.report(
                impl_function_syntax,
                ImplItemNotInTrait {
                    impl_def_id,
                    impl_item_name: function_name,
                    trait_id,
                    item_kind: "function".into(),
                },
            )
        })?;
    let concrete_trait_function =
        ConcreteTraitGenericFunctionId::new(db, concrete_trait_id, trait_function_id);
    let concrete_trait_signature = db.concrete_trait_function_signature(concrete_trait_function)?;

    // Match generics of the function.
    // TODO(spapini): Compare the actual kinds and traits for the generic params.

    let func_generics = db.concrete_trait_function_generic_params(concrete_trait_function)?;
    if impl_func_generics.len() != func_generics.len() {
        diagnostics.report(
            &impl_function_syntax.declaration(syntax_db).name(syntax_db),
            WrongNumberOfGenericParamsForImplFunction {
                expected: func_generics.len(),
                actual: impl_func_generics.len(),
            },
        );
        return Ok(trait_function_id);
    }
    let substitution =
        GenericSubstitution::new(&func_generics, &generic_params_to_args(impl_func_generics, db));
    let concrete_trait_signature = SubstitutionRewriter { db, substitution: &substitution }
        .rewrite(concrete_trait_signature)?;

    if signature.params.len() != concrete_trait_signature.params.len() {
        diagnostics.report(
            &signature_syntax.parameters(syntax_db),
            WrongNumberOfParameters {
                impl_def_id,
                impl_function_id,
                trait_id,
                expected: concrete_trait_signature.params.len(),
                actual: signature.params.len(),
            },
        );
    }
    let impl_def_substitution = db.impl_def_substitution(impl_def_id)?;
    let concrete_trait_signature =
        SubstitutionRewriter { db, substitution: impl_def_substitution.as_ref() }
            .rewrite(concrete_trait_signature)?;
    for (idx, (param, trait_param)) in
        izip!(signature.params.iter(), concrete_trait_signature.params.iter()).enumerate()
    {
        let expected_ty = inference.rewrite(trait_param.ty).no_err();
        let actual_ty = inference.rewrite(param.ty).no_err();

        if expected_ty != actual_ty {
            diagnostics.report(
                &extract_matches!(
                    signature_syntax.parameters(syntax_db).elements(syntax_db)[idx]
                        .type_clause(syntax_db),
                    OptionTypeClause::TypeClause
                )
                .ty(syntax_db),
                WrongParameterType {
                    impl_def_id,
                    impl_function_id,
                    trait_id,
                    expected_ty,
                    actual_ty,
                },
            );
        }

        if trait_param.mutability != param.mutability {
            if trait_param.mutability == Mutability::Reference {
                diagnostics.report(
                    &signature_syntax.parameters(syntax_db).elements(syntax_db)[idx]
                        .modifiers(syntax_db),
                    ParameterShouldBeReference { impl_def_id, impl_function_id, trait_id },
                );
            }

            if param.mutability == Mutability::Reference {
                diagnostics.report(
                    &signature_syntax.parameters(syntax_db).elements(syntax_db)[idx]
                        .modifiers(syntax_db),
                    ParameterShouldNotBeReference { impl_def_id, impl_function_id, trait_id },
                );
            }
        }

        if trait_param.name != param.name {
            diagnostics.report(
                &signature_syntax.parameters(syntax_db).elements(syntax_db)[idx].name(syntax_db),
                WrongParameterName {
                    impl_def_id,
                    impl_function_id,
                    trait_id,
                    expected_name: trait_param.name.clone(),
                },
            );
        }
    }

    if !concrete_trait_signature.panicable && signature.panicable {
        diagnostics.report(signature_syntax, PassPanicAsNopanic { impl_function_id, trait_id });
    }

    let expected_ty = inference.rewrite(concrete_trait_signature.return_type).no_err();
    let actual_ty = inference.rewrite(signature.return_type).no_err();

    if expected_ty != actual_ty {
        let location_ptr = match signature_syntax.ret_ty(syntax_db) {
            OptionReturnTypeClause::ReturnTypeClause(ret_ty) => {
                ret_ty.ty(syntax_db).as_syntax_node()
            }
            OptionReturnTypeClause::Empty(_) => {
                impl_function_syntax.body(syntax_db).lbrace(syntax_db).as_syntax_node()
            }
        }
        .stable_ptr();
        diagnostics.report(
            location_ptr,
            WrongReturnTypeForImpl {
                impl_def_id,
                impl_function_id,
                trait_id,
                expected_ty,
                actual_ty,
            },
        );
    }
    Ok(trait_function_id)
}

// === Impl Function Body ===

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::impl_function_body_diagnostics].
pub fn impl_function_body_diagnostics(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_function_body_data(impl_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_body].
pub fn impl_function_body(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<Arc<FunctionBody>> {
    Ok(db.priv_impl_function_body_data(impl_function_id)?.body)
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_body_resolver_data].
pub fn impl_function_body_resolver_data(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_impl_function_body_data(impl_function_id)?.resolver_data)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_impl_function_body_data].
pub fn priv_impl_function_body_data(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<FunctionBodyData> {
    let defs_db = db.upcast();
    let mut diagnostics = SemanticDiagnostics::default();
    let impl_def_id = impl_function_id.impl_def_id(defs_db);
    let data = db.priv_impl_definition_data(impl_def_id)?;
    let function_syntax = &data.function_asts[&impl_function_id];
    // Compute declaration semantic.
    let declaration = db.priv_impl_function_declaration_data(impl_function_id)?;
    let parent_resolver_data = declaration.function_declaration_data.resolver_data;
    let inference_id = InferenceId::LookupItemDefinition(LookupItemId::ImplItem(
        ImplItemId::Function(impl_function_id),
    ));
    let resolver =
        Resolver::with_data(db, (*parent_resolver_data).clone_with_inference_id(db, inference_id));
    let environment: Environment = declaration.function_declaration_data.environment;

    let function_id = (|| {
        let trait_function_id = db.impl_function_trait_function(impl_function_id)?;
        let generic_parameters = db.impl_def_generic_params(impl_def_id)?;

        let generic_function = GenericFunctionId::Impl(ImplGenericFunctionId {
            impl_id: ImplLongId::Concrete(
                ConcreteImplLongId {
                    impl_def_id,
                    generic_args: generic_params_to_args(&generic_parameters, db),
                }
                .intern(db),
            )
            .intern(db),
            function: trait_function_id,
        });

        Ok(FunctionLongId::from_generic(db, generic_function)?.intern(db))
    })();
    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(
        db,
        &mut diagnostics,
        resolver,
        Some(&declaration.function_declaration_data.signature),
        environment,
        ContextFunction::Function(function_id),
    );
    let function_body = function_syntax.body(db.upcast());
    let return_type = declaration.function_declaration_data.signature.return_type;
    let body_expr = compute_root_expr(&mut ctx, &function_body, return_type)?;
    let ComputationContext { arenas: Arenas { exprs, patterns, statements }, resolver, .. } = ctx;

    let expr_lookup: UnorderedHashMap<_, _> =
        exprs.iter().map(|(expr_id, expr)| (expr.stable_ptr(), expr_id)).collect();
    let pattern_lookup: UnorderedHashMap<_, _> =
        patterns.iter().map(|(pattern_id, pattern)| (pattern.stable_ptr(), pattern_id)).collect();
    let resolver_data = Arc::new(resolver.data);
    Ok(FunctionBodyData {
        diagnostics: diagnostics.build(),
        expr_lookup,
        pattern_lookup,
        resolver_data,
        body: Arc::new(FunctionBody { arenas: Arenas { exprs, patterns, statements }, body_expr }),
    })
}

pub fn priv_impl_is_fully_concrete(db: &dyn SemanticGroup, impl_id: ImplId) -> bool {
    impl_id.lookup_intern(db).is_fully_concrete(db)
}

pub fn priv_impl_is_var_free(db: &dyn SemanticGroup, impl_id: ImplId) -> bool {
    impl_id.lookup_intern(db).is_var_free(db)
}
