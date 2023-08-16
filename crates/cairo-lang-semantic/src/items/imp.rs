use std::collections::BTreeSet;
use std::sync::Arc;
use std::vec;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    FunctionTitleId, FunctionWithBodyId, GenericKind, GenericParamId, ImplAliasId, ImplDefId,
    ImplFunctionId, ImplFunctionLongId, LanguageElementId, LookupItemId, ModuleId, ModuleItemId,
    TopLevelLanguageElementId, TraitFunctionId, TraitId,
};
use cairo_lang_diagnostics::{
    skip_diagnostic, Diagnostics, DiagnosticsBuilder, Maybe, ToMaybe, ToOption,
};
use cairo_lang_filesystem::ids::UnstableSalsaId;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::ast::{self, ImplItem, MaybeImplBody, OptionReturnTypeClause};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{define_short_id, extract_matches, try_extract_matches};
use itertools::{chain, izip, Itertools};
use smol_str::SmolStr;
use syntax::node::ast::GenericArg;
use syntax::node::db::SyntaxGroup;

use super::enm::SemanticEnumEx;
use super::function_with_body::{get_inline_config, FunctionBody, FunctionBodyData};
use super::functions::{
    forbid_inline_always_with_impl_generic_param, FunctionDeclarationData, InlineConfiguration,
};
use super::generics::{semantic_generic_params, GenericArgumentHead, GenericParamsData};
use super::structure::SemanticStructEx;
use super::trt::{ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId};
use crate::corelib::{copy_trait, core_module, drop_trait};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::compute::{compute_root_expr, ComputationContext, Environment};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::infers::InferenceEmbeddings;
use crate::expr::inference::solver::SolutionSet;
use crate::expr::inference::{ImplVarId, InferenceId};
use crate::items::function_with_body::get_implicit_precedence;
use crate::items::functions::ImplicitPrecedence;
use crate::items::us::SemanticUseEx;
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use crate::{
    semantic, semantic_object_for_id, ConcreteFunction, ConcreteTraitId, ConcreteTraitLongId,
    FunctionId, FunctionLongId, GenericArgumentId, GenericParam, Mutability, SemanticDiagnostic,
    TypeId, TypeLongId,
};

#[cfg(test)]
#[path = "imp_test.rs"]
mod test;

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteImplLongId {
    pub impl_def_id: ImplDefId,
    pub generic_args: Vec<GenericArgumentId>,
}
define_short_id!(ConcreteImplId, ConcreteImplLongId, SemanticGroup, lookup_intern_concrete_impl);
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
                write!(f, "{:?}", arg.debug(db))?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}
impl ConcreteImplId {
    pub fn impl_def_id(&self, db: &dyn SemanticGroup) -> ImplDefId {
        db.lookup_intern_concrete_impl(*self).impl_def_id
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
}

/// Represents a "callee" impl that can be referred to in the code.
/// Traits should be resolved to this.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum ImplId {
    Concrete(ConcreteImplId),
    GenericParameter(GenericParamId),
    ImplVar(ImplVarId),
}
impl ImplId {
    /// Returns the [ImplHead] of an impl if available.
    pub fn head(&self, db: &dyn SemanticGroup) -> Option<ImplHead> {
        Some(match self {
            ImplId::Concrete(concrete) => ImplHead::Concrete(concrete.impl_def_id(db)),
            ImplId::GenericParameter(_) | ImplId::ImplVar(_) => return None,
        })
    }
    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        match self {
            ImplId::Concrete(concrete_impl) => concrete_impl.name(db),
            ImplId::GenericParameter(generic_param_impl) => generic_param_impl.name(db.upcast()),
            ImplId::ImplVar(var) => format!("{var:?}").into(),
        }
    }
    pub fn concrete_trait(&self, db: &dyn SemanticGroup) -> Maybe<ConcreteTraitId> {
        db.impl_concrete_trait(*self)
    }
}
impl DebugWithDb<dyn SemanticGroup> for ImplId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        match self {
            ImplId::Concrete(concrete_impl_id) => write!(f, "{:?}", concrete_impl_id.debug(db)),
            ImplId::GenericParameter(param) => write!(f, "{:?}", param.debug(db)),
            ImplId::ImplVar(var) => write!(f, "?{}", var.get(db).id.0),
        }
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

impl ImplDeclarationData {
    /// Returns Maybe::Err if a cycle is detected here.
    // TODO(orizi): Remove this function when cycle validation is not required through a type's
    // field.
    pub fn check_no_cycle(&self) -> Maybe<()> {
        self.concrete_trait?;
        Ok(())
    }
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
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);

    let module_impls = db.module_impls(module_file_id.0)?;
    let syntax_db = db.upcast();
    let impl_ast = module_impls.get(&impl_def_id).to_maybe()?;
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(ModuleItemId::Impl(impl_def_id)));

    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &impl_ast.generic_params(syntax_db),
    )?;
    resolver.inference().finalize().map(|(_, inference_err)| {
        inference_err.report(&mut diagnostics, impl_ast.stable_ptr().untyped())
    });
    let generic_params = resolver.inference().rewrite(generic_params).no_err();
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

/// Query implementation of [crate::db::SemanticGroup::impl_def_functions_asts].
pub fn impl_def_functions_asts(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<OrderedHashMap<ImplFunctionId, ast::FunctionWithBody>> {
    Ok(db.priv_impl_definition_data(impl_def_id)?.function_asts)
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_concrete_trait].
pub fn impl_def_concrete_trait(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<ConcreteTraitId> {
    db.priv_impl_declaration_data(impl_def_id)?.concrete_trait
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
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);

    let module_impls = db.module_impls(module_file_id.0)?;
    let syntax_db = db.upcast();
    let impl_ast = module_impls.get(&impl_def_id).to_maybe()?;
    let inference_id = InferenceId::ImplDefTrait(impl_def_id);

    let mut resolver = Resolver::new(db, module_file_id, inference_id);

    let trait_path_syntax = impl_ast.trait_path(syntax_db);

    resolver
        .resolve_generic_path_with_args(
            &mut diagnostics,
            &trait_path_syntax,
            NotFoundItemType::Trait,
        )
        .ok()
        .and_then(|generic_item| try_extract_matches!(generic_item, ResolvedGenericItem::Trait))
        .ok_or_else(|| diagnostics.report(&trait_path_syntax, NotATrait))
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_concrete_trait].
pub fn impl_concrete_trait(db: &dyn SemanticGroup, impl_id: ImplId) -> Maybe<ConcreteTraitId> {
    match impl_id {
        ImplId::Concrete(concrete_impl_id) => {
            let long_impl = db.lookup_intern_concrete_impl(concrete_impl_id);
            let substitution = GenericSubstitution::new(
                &db.impl_def_generic_params(long_impl.impl_def_id)?,
                &long_impl.generic_args,
            );

            let impl_concrete_trait_id = db.impl_def_concrete_trait(long_impl.impl_def_id)?;
            SubstitutionRewriter { db, substitution: &substitution }.rewrite(impl_concrete_trait_id)
        }
        ImplId::GenericParameter(param) => {
            let param_impl =
                extract_matches!(db.generic_param_semantic(param)?, GenericParam::Impl);
            param_impl.concrete_trait
        }
        ImplId::ImplVar(var) => Ok(var.get(db).concrete_trait_id),
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
    let module_file_id = impl_def_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);

    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let module_impls = db.module_impls(module_file_id.0)?;
    let syntax_db = db.upcast();
    let impl_ast = module_impls.get(&impl_def_id).to_maybe()?;
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
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);
    let trait_path_syntax = impl_ast.trait_path(syntax_db);

    let concrete_trait = if resolve_trait {
        resolver
            .resolve_concrete_path(&mut diagnostics, &trait_path_syntax, NotFoundItemType::Trait)
            .ok()
            .and_then(|concrete_item| {
                try_extract_matches!(concrete_item, ResolvedConcreteItem::Trait)
            })
    } else {
        None
    }
    .ok_or_else(|| diagnostics.report(&trait_path_syntax, NotATrait));

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = resolver.inference().finalize() {
        inference_err
            .report(&mut diagnostics, stable_ptr.unwrap_or(impl_ast.stable_ptr().untyped()));
    }
    let concrete_trait = resolver.inference().rewrite(concrete_trait).no_err();
    let generic_params = resolver.inference().rewrite(generic_params).no_err();

    let attributes = impl_ast.attributes(syntax_db).structurize(syntax_db);
    let resolver_data = Arc::new(resolver.data);
    Ok(ImplDeclarationData {
        diagnostics: diagnostics.build(),
        generic_params,
        concrete_trait,
        attributes,
        resolver_data,
    })
}

// === Impl Definition ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplDefinitionData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    function_asts: OrderedHashMap<ImplFunctionId, ast::FunctionWithBody>,
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

    diagnostics.extend(data.diagnostics);
    for impl_function_id in data.function_asts.keys() {
        diagnostics.extend(db.impl_function_declaration_diagnostics(*impl_function_id));
        diagnostics.extend(db.impl_function_body_diagnostics(*impl_function_id));
    }

    diagnostics.build()
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
            let function_long_id = db.lookup_intern_impl_function(*function_id);
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
        if db.lookup_intern_impl_function(*impl_function_id).name(defs_db) == name {
            return Ok(Some(*impl_function_id));
        }
    }
    Ok(None)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_impl_definition_data].
pub fn priv_impl_definition_data(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<ImplDefinitionData> {
    let defs_db = db.upcast();
    let syntax_db = db.upcast();

    let module_file_id = impl_def_id.module_file_id(defs_db);
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);

    let generic_params = db.impl_def_generic_params(impl_def_id)?;
    let concrete_trait = db.priv_impl_declaration_data(impl_def_id)?.concrete_trait?;

    let module_impls = db.module_impls(module_file_id.0)?;
    let impl_ast = module_impls.get(&impl_def_id).to_maybe()?;

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

    // TODO(yuval): verify that all functions of `concrete_trait` appear in this impl.

    let mut function_asts = OrderedHashMap::default();
    let mut impl_item_names = OrderedHashSet::default();

    if let MaybeImplBody::Some(body) = impl_ast.body(syntax_db) {
        for item in body.items(syntax_db).elements(syntax_db) {
            match item {
                ImplItem::Constant(constant) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    constant.const_kw(syntax_db),
                ),
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
                ImplItem::Impl(imp) => {
                    report_invalid_impl_item(syntax_db, &mut diagnostics, imp.impl_kw(syntax_db))
                }
                ImplItem::Struct(structure) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    structure.struct_kw(syntax_db),
                ),
                ImplItem::Enum(enm) => {
                    report_invalid_impl_item(syntax_db, &mut diagnostics, enm.enum_kw(syntax_db))
                }
                ImplItem::TypeAlias(ty) => {
                    report_invalid_impl_item(syntax_db, &mut diagnostics, ty.type_kw(syntax_db))
                }
                ImplItem::ImplAlias(imp) => {
                    report_invalid_impl_item(syntax_db, &mut diagnostics, imp.impl_kw(syntax_db))
                }
                ImplItem::Function(func) => {
                    let impl_function_id = db.intern_impl_function(ImplFunctionLongId(
                        module_file_id,
                        func.stable_ptr(),
                    ));
                    if !impl_item_names.insert(impl_function_id.name(defs_db)) {
                        diagnostics.report_by_ptr(
                            func.declaration(syntax_db).name(syntax_db).stable_ptr().untyped(),
                            SemanticDiagnosticKind::NameDefinedMultipleTimes {
                                name: impl_function_id.name(defs_db),
                            },
                        );
                    }
                    function_asts.insert(impl_function_id, func);
                }
                // Report nothing, a parser diagnostic is reported.
                ImplItem::Missing(_) => {}
            }
        }
    }

    // It is later verified that all items in this impl match items from `concrete_trait`.
    // To ensure exact match (up to trait functions with default implementation), it is sufficient
    // to verify here that all items in `concrete_trait` appear in this impl.
    // TODO(yuval): Once default implementation of trait functions is supported, filter such
    // functions out.
    let trait_item_names = db
        .trait_functions(db.lookup_intern_concrete_trait(concrete_trait).trait_id)?
        .into_keys()
        .collect::<OrderedHashSet<_>>();
    let missing_items_in_impl =
        trait_item_names.difference(&impl_item_names).cloned().collect::<Vec<_>>();
    if !missing_items_in_impl.is_empty() {
        diagnostics.report(
            // TODO(yuval): change this to point to impl declaration (need to add ImplDeclaration
            // in cairo_spec).
            &impl_ast.name(syntax_db),
            SemanticDiagnosticKind::MissingItemsInImpl { item_names: missing_items_in_impl },
        );
    }

    Ok(ImplDefinitionData { diagnostics: diagnostics.build(), function_asts })
}

/// An helper function to report diagnostics of items in an impl (used in
/// priv_impl_definition_data).
fn report_invalid_impl_item<Terminal: syntax::node::Terminal>(
    syntax_db: &dyn SyntaxGroup,
    diagnostics: &mut SemanticDiagnostics,
    kw_terminal: Terminal,
) {
    diagnostics.report_by_ptr(
        kw_terminal.as_syntax_node().stable_ptr(),
        InvalidImplItem { item_kw: kw_terminal.text(syntax_db) },
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
    let ConcreteTraitLongId { trait_id, generic_args } =
        db.lookup_intern_concrete_trait(concrete_trait);
    let copy = copy_trait(db);
    let drop = drop_trait(db);

    if trait_id == copy {
        let tys = get_inner_types(db, extract_matches!(generic_args[0], GenericArgumentId::Type))?;
        if let Some(inference_error) = tys
            .into_iter()
            .filter_map(|ty| db.type_info(lookup_context.clone(), ty).to_option())
            .flat_map(|info| info.duplicatable.err())
            .next()
        {
            return Err(
                diagnostics.report_by_ptr(stable_ptr, InvalidCopyTraitImpl { inference_error })
            );
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
            return Err(
                diagnostics.report_by_ptr(stable_ptr, InvalidDropTraitImpl { inference_error })
            );
        }
    }

    Ok(())
}

/// Retrieves all the inner types (members of a struct / tuple or variants of an enum).
/// These are the types that are required to implement some trait,
/// in order for the original type to be able to implement this trait.
fn get_inner_types(db: &dyn SemanticGroup, ty: TypeId) -> Maybe<Vec<TypeId>> {
    Ok(match db.lookup_intern_type(ty) {
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
        TypeLongId::Snapshot(_) => vec![],
        TypeLongId::GenericParameter(_) => {
            return Err(skip_diagnostic());
        }
        TypeLongId::Var(_) => panic!("Types should be fully resolved at this point."),
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
    for impl_def_id in db.module_impls_ids(module_id)?.iter().copied() {
        uninferred_impls.push(UninferredImpl::Def(impl_def_id));
    }
    for impl_alias_id in db.module_impl_aliases_ids(module_id)?.iter().copied() {
        uninferred_impls.push(UninferredImpl::ImplAlias(impl_alias_id));
    }
    for use_id in db.module_uses_ids(module_id)?.iter().copied() {
        if let Ok(ResolvedGenericItem::Impl(impl_def_id)) = db.use_resolved_item(use_id) {
            uninferred_impls.push(UninferredImpl::Def(impl_def_id));
        }
    }
    let mut res = Vec::new();
    for uninferred_impl in uninferred_impls {
        let trait_id = uninferred_impl.trait_id(db)?;
        if trait_id != trait_filter.trait_id {
            continue;
        }
        let concrete_trait_id = uninferred_impl.concrete_trait(db)?;
        if let Ok(true) = concrete_trait_fits_trait_filter(db, concrete_trait_id, &trait_filter) {
            res.push(uninferred_impl);
        }
    }

    Ok(res)
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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ModuleIdById(pub ModuleId);
impl Ord for ModuleIdById {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (
                ModuleIdById(ModuleId::CrateRoot(crate_id)),
                ModuleIdById(ModuleId::CrateRoot(other_crate_id)),
            ) => crate_id.get_internal_id().cmp(other_crate_id.get_internal_id()),
            (ModuleIdById(ModuleId::CrateRoot(_)), ModuleIdById(ModuleId::Submodule(_))) => {
                std::cmp::Ordering::Less
            }
            (ModuleIdById(ModuleId::Submodule(_)), ModuleIdById(ModuleId::CrateRoot(_))) => {
                std::cmp::Ordering::Greater
            }
            (
                ModuleIdById(ModuleId::Submodule(module_id)),
                ModuleIdById(ModuleId::Submodule(other_module_id)),
            ) => module_id.get_internal_id().cmp(other_module_id.get_internal_id()),
        }
    }
}
impl PartialOrd for ModuleIdById {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplLookupContext {
    pub modules: BTreeSet<ModuleIdById>,
    pub generic_params: Vec<GenericParamId>,
}
impl ImplLookupContext {
    pub fn new(module_id: ModuleId, generic_params: Vec<GenericParamId>) -> ImplLookupContext {
        Self { modules: [ModuleIdById(module_id)].into(), generic_params }
    }

    pub fn insert_module(&mut self, module_id: ModuleId) -> bool {
        self.modules.insert(ModuleIdById(module_id))
    }
}

/// An candidate impl for later inference.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum UninferredImpl {
    Def(ImplDefId),
    ImplAlias(ImplAliasId),
    GenericParam(GenericParamId),
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
        }
    }

    fn trait_id(&self, db: &dyn SemanticGroup) -> Maybe<TraitId> {
        match self {
            UninferredImpl::Def(impl_def_id) => db.impl_def_trait(*impl_def_id),
            UninferredImpl::ImplAlias(impl_alias_id) => {
                let impl_id = db.impl_alias_resolved_impl(*impl_alias_id)?;
                impl_id.concrete_trait(db).map(|concrete_trait| concrete_trait.trait_id(db))
            }
            UninferredImpl::GenericParam(param) => {
                let param =
                    extract_matches!(db.generic_param_semantic(*param)?, GenericParam::Impl);
                param.concrete_trait.map(|concrete_trait| concrete_trait.trait_id(db))
            }
        }
    }

    pub fn module_id(&self, db: &dyn SemanticGroup) -> ModuleId {
        let defs_db = db.upcast();
        match self {
            UninferredImpl::Def(impl_def_id) => impl_def_id.module_file_id(defs_db).0,
            UninferredImpl::ImplAlias(impl_alias_id) => impl_alias_id.module_file_id(defs_db).0,
            UninferredImpl::GenericParam(param) => param.module_file_id(defs_db).0,
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
                write!(f, "generic param {}", param.name(db.upcast()))
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
        let trait_id = db.generic_impl_param_trait(*generic_param_id)?;
        if filter.trait_id != trait_id {
            continue;
        }
        let param =
            extract_matches!(db.generic_param_semantic(*generic_param_id)?, GenericParam::Impl);
        let Ok(imp_concrete_trait_id) = param.concrete_trait else { continue };
        if !concrete_trait_fits_trait_filter(db, imp_concrete_trait_id, &filter)? {
            continue;
        }
        res.insert(UninferredImpl::GenericParam(*generic_param_id));
    }
    let core_module = core_module(db);
    for module_id in chain!(lookup_context.modules.iter().map(|x| &x.0), [&core_module]) {
        let imps = db.module_impl_ids_for_trait_filter(*module_id, filter.clone())?;
        for imp in imps {
            res.insert(imp);
        }
    }
    Ok(res)
}

/// Checks if an impl of a trait function with a given self_ty exists.
/// This function does not change the state of the inference context.
pub fn can_infer_impl_by_self(
    ctx: &mut ComputationContext<'_>,
    trait_function_id: TraitFunctionId,
    self_ty: TypeId,
    stable_ptr: SyntaxStablePtrId,
) -> bool {
    let mut temp_inference_data = ctx.resolver.inference().temporary_clone();
    let mut temp_inference = temp_inference_data.inference(ctx.db);
    let lookup_context = ctx.resolver.impl_lookup_context();
    let Some((concrete_trait_id, _)) = temp_inference.infer_concrete_trait_by_self(
        trait_function_id,
        self_ty,
        &lookup_context,
        Some(stable_ptr),
    ) else {
        return false;
    };
    // Find impls for it.
    temp_inference.solve().ok();
    matches!(
        temp_inference.trait_solution_set(concrete_trait_id, lookup_context.clone()),
        Ok(SolutionSet::Unique(_) | SolutionSet::Ambiguous(_))
    )
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
    let Some((concrete_trait_id, n_snapshots)) =
        ctx.resolver.inference().infer_concrete_trait_by_self(
            trait_function_id,
            self_ty,
            &lookup_context,
            Some(stable_ptr),
        )
    else {
        return None;
    };

    let concrete_trait_function_id = ctx.db.intern_concrete_trait_function(
        ConcreteTraitGenericFunctionLongId::new(ctx.db, concrete_trait_id, trait_function_id),
    );
    let trait_func_generic_params =
        ctx.db.concrete_trait_function_generic_params(concrete_trait_function_id).unwrap();
    let generic_args = ctx
        .resolver
        .resolve_generic_args(
            ctx.diagnostics,
            &trait_func_generic_params,
            generic_args_syntax.unwrap_or_default(),
            stable_ptr,
        )
        .unwrap();

    let impl_lookup_context = ctx.resolver.impl_lookup_context();
    let generic_function = ctx
        .resolver
        .inference()
        .infer_trait_generic_function(
            concrete_trait_function_id,
            &impl_lookup_context,
            Some(stable_ptr),
        )
        .map_err(|err| err.report(ctx.diagnostics, stable_ptr))
        .unwrap();

    Some((
        ctx.db.intern_function(FunctionLongId {
            function: ConcreteFunction { generic_function, generic_args },
        }),
        n_snapshots,
    ))
}

/// Returns all the trait functions that fits the given function name and can be called on a given
/// type.
pub fn filter_candidate_traits(
    ctx: &mut ComputationContext<'_>,
    self_ty: TypeId,
    candidate_traits: &[TraitId],
    function_name: SmolStr,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<Vec<TraitFunctionId>> {
    let mut candidates = Vec::new();
    for trait_id in candidate_traits.iter().copied() {
        for (name, trait_function) in ctx.db.trait_functions(trait_id)? {
            if name == function_name
                && can_infer_impl_by_self(ctx, trait_function, self_ty, stable_ptr)
            {
                candidates.push(trait_function);
            }
        }
    }
    Ok(candidates)
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
    Ok(db.impl_function_generic_params_data(impl_function_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_generic_params_data].
pub fn impl_function_generic_params_data(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<GenericParamsData> {
    let module_file_id = impl_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let impl_def_id = impl_function_id.impl_def_id(db.upcast());
    let data = db.priv_impl_definition_data(impl_def_id)?;
    let function_syntax = &data.function_asts[impl_function_id];
    let syntax_db = db.upcast();
    let declaration = function_syntax.declaration(syntax_db);
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ImplFunction(impl_function_id));
    let resolver_data = db.impl_def_resolver_data(impl_def_id)?;
    let mut resolver =
        Resolver::with_data(db, resolver_data.clone_with_inference_id(db, inference_id));
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &declaration.generic_params(syntax_db),
    )?;
    resolver.inference().finalize().map(|(_, inference_err)| {
        inference_err.report(&mut diagnostics, function_syntax.stable_ptr().untyped())
    });
    let generic_params = resolver.inference().rewrite(generic_params).no_err();
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
    let module_file_id = impl_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let impl_def_id = impl_function_id.impl_def_id(db.upcast());
    let data = db.priv_impl_definition_data(impl_def_id)?;
    let function_syntax = &data.function_asts[impl_function_id];
    let syntax_db = db.upcast();
    let declaration = function_syntax.declaration(syntax_db);

    let generic_params_data = db.impl_function_generic_params_data(impl_function_id)?;
    let generic_params = generic_params_data.generic_params;
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ImplFunction(impl_function_id));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);

    let signature_syntax = declaration.signature(syntax_db);

    let mut environment = Environment::default();
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        FunctionTitleId::Impl(impl_function_id),
        &mut environment,
    );

    let trait_function_id = validate_impl_function_signature(
        db,
        &mut diagnostics,
        impl_function_id,
        &signature_syntax,
        &signature,
        function_syntax,
        &generic_params,
    );

    let attributes = function_syntax.attributes(syntax_db).structurize(syntax_db);

    let inline_config = get_inline_config(db, &mut diagnostics, &attributes)?;

    forbid_inline_always_with_impl_generic_param(&mut diagnostics, &generic_params, &inline_config);

    let (implicit_precedence, _) = get_implicit_precedence(db, &mut diagnostics, &attributes)?;

    // Check fully resolved.
    if let Some((stable_ptr, inference_err)) = resolver.inference().finalize() {
        inference_err
            .report(&mut diagnostics, stable_ptr.unwrap_or(function_syntax.stable_ptr().untyped()));
    }
    let signature = resolver.inference().rewrite(signature).no_err();
    let generic_params = resolver.inference().rewrite(generic_params).no_err();

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

fn validate_impl_function_signature(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    impl_function_id: ImplFunctionId,
    signature_syntax: &ast::FunctionSignature,
    signature: &semantic::Signature,
    function_syntax: &ast::FunctionWithBody,
    impl_func_generics: &[GenericParam],
) -> Maybe<TraitFunctionId> {
    let syntax_db = db.upcast();
    let impl_def_id = impl_function_id.impl_def_id(db.upcast());
    let declaration_data = db.priv_impl_declaration_data(impl_def_id)?;
    let concrete_trait_id = declaration_data.concrete_trait?;
    let concrete_trait_long_id = db.lookup_intern_concrete_trait(concrete_trait_id);
    let trait_id = concrete_trait_long_id.trait_id;
    let trait_functions = db.trait_functions(trait_id)?;
    let function_name = db.lookup_intern_impl_function(impl_function_id).name(db.upcast());
    let trait_function_id = *trait_functions.get(&function_name).ok_or_else(|| {
        diagnostics.report(
            function_syntax,
            FunctionNotMemberOfTrait { impl_def_id, impl_function_id, trait_id },
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
            &function_syntax.declaration(syntax_db).name(syntax_db),
            WrongNumberOfGenericParamsForImplFunction {
                expected: func_generics.len(),
                actual: impl_func_generics.len(),
            },
        );
        return Ok(trait_function_id);
    }
    let substitution = GenericSubstitution::new(
        &func_generics,
        &impl_func_generics
            .iter()
            .map(|param| {
                GenericArgumentId::Type(db.intern_type(TypeLongId::GenericParameter(param.id())))
            })
            .collect_vec(),
    );
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
    for (idx, (param, trait_param)) in
        izip!(signature.params.iter(), concrete_trait_signature.params.iter()).enumerate()
    {
        let expected_ty = trait_param.ty;
        let actual_ty = param.ty;

        if expected_ty != actual_ty {
            diagnostics.report(
                &signature_syntax.parameters(syntax_db).elements(syntax_db)[idx]
                    .type_clause(syntax_db)
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

    let expected_ty = concrete_trait_signature.return_type;
    let actual_ty = signature.return_type;
    if expected_ty != actual_ty {
        let location_ptr = match signature_syntax.ret_ty(syntax_db) {
            OptionReturnTypeClause::ReturnTypeClause(ret_ty) => {
                ret_ty.ty(syntax_db).as_syntax_node()
            }
            OptionReturnTypeClause::Empty(_) => {
                function_syntax.body(syntax_db).lbrace(syntax_db).as_syntax_node()
            }
        }
        .stable_ptr();
        diagnostics.report_by_ptr(
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
    let module_file_id = impl_function_id.module_file_id(defs_db);
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let impl_def_id = impl_function_id.impl_def_id(defs_db);
    let data = db.priv_impl_definition_data(impl_def_id)?;
    let function_syntax = &data.function_asts[impl_function_id];
    // Compute declaration semantic.
    let declaration = db.priv_impl_function_declaration_data(impl_function_id)?;
    let parent_resolver_data = declaration.function_declaration_data.resolver_data;
    let inference_id =
        InferenceId::LookupItemDefinition(LookupItemId::ImplFunction(impl_function_id));
    let resolver =
        Resolver::with_data(db, (*parent_resolver_data).clone_with_inference_id(db, inference_id));
    let environment = declaration.function_declaration_data.environment;

    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(
        db,
        &mut diagnostics,
        Some(FunctionWithBodyId::Impl(impl_function_id)),
        resolver,
        Some(&declaration.function_declaration_data.signature),
        environment,
    );
    let function_body = function_syntax.body(db.upcast());
    let return_type = declaration.function_declaration_data.signature.return_type;
    let body_expr = compute_root_expr(&mut ctx, &function_body, return_type)?;
    let ComputationContext { exprs, patterns, statements, resolver, .. } = ctx;

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
        body: Arc::new(FunctionBody { exprs, patterns, statements, body_expr }),
    })
}
