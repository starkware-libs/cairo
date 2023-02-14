use std::collections::HashSet;
use std::sync::Arc;
use std::vec;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    FunctionSignatureId, GenericParamId, ImplDefId, ImplFunctionId, ImplFunctionLongId,
    LanguageElementId, ModuleId, TopLevelLanguageElementId, TraitFunctionId,
};
use cairo_lang_diagnostics::{
    skip_diagnostic, Diagnostics, DiagnosticsBuilder, Maybe, ToMaybe, ToOption,
};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::ast::{self, Item, MaybeImplBody, OptionReturnTypeClause};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{define_short_id, extract_matches, try_extract_matches};
use itertools::{chain, izip, Itertools};
use smol_str::SmolStr;

use super::attribute::{ast_attributes_to_semantic, Attribute};
use super::enm::SemanticEnumEx;
use super::function_with_body::{FunctionBody, FunctionBodyData};
use super::functions::{substitute_signature, FunctionDeclarationData};
use super::generics::semantic_generic_params;
use super::structure::SemanticStructEx;
use crate::corelib::{copy_trait, core_module, drop_trait};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::compute::{compute_root_expr, ComputationContext, Environment};
use crate::expr::inference::Inference;
use crate::items::us::SemanticUseEx;
use crate::resolve_path::{ResolvedConcreteItem, ResolvedGenericItem, ResolvedLookback, Resolver};
use crate::types::{substitute_generics_args_inplace, GenericSubstitution};
use crate::{
    semantic, ConcreteTraitId, ConcreteTraitLongId, Expr, FunctionId, GenericArgumentId,
    GenericParam, Mutability, SemanticDiagnostic, TypeId, TypeLongId,
};

#[cfg(test)]
#[path = "imp_test.rs"]
mod test;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConcreteImplLongId {
    pub impl_def_id: ImplDefId,
    pub generic_args: Vec<GenericArgumentId>,
}
define_short_id!(ConcreteImplId, ConcreteImplLongId, SemanticGroup, lookup_intern_concrete_impl);
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
}

/// Represents a "callee" impl that can be referred to in the code.
/// Traits should be resolved to this.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ImplId {
    Concrete(ConcreteImplId),
    GenericParameter(GenericParamId),
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
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<semantic::GenericParam>,
    /// The concrete trait this impl implements, or Err if cannot be resolved.
    concrete_trait: Maybe<ConcreteTraitId>,
    attributes: Vec<Attribute>,
    resolved_lookback: Arc<ResolvedLookback>,
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

/// Query implementation of [crate::db::SemanticGroup::impl_semantic_declaration_diagnostics].
pub fn impl_semantic_declaration_diagnostics(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_declaration_data(impl_def_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_generic_params].
pub fn impl_def_generic_params(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Vec<semantic::GenericParam>> {
    Ok(db.priv_impl_declaration_data(impl_def_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_resolved_lookback].
pub fn impl_def_resolved_lookback(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db.priv_impl_declaration_data(impl_def_id)?.resolved_lookback)
}

/// Query implementation of [crate::db::SemanticGroup::impl_def_concrete_trait].
pub fn impl_def_concrete_trait(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<ConcreteTraitId> {
    db.priv_impl_declaration_data(impl_def_id)?.concrete_trait
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
            let mut long_concrete_trait = db.lookup_intern_concrete_trait(impl_concrete_trait_id);
            substitute_generics_args_inplace(
                db,
                &substitution,
                &mut long_concrete_trait.generic_args,
            );
            Ok(db.intern_concrete_trait(long_concrete_trait))
        }
        ImplId::GenericParameter(param) => {
            let param_impl =
                extract_matches!(db.generic_param_semantic(param)?, GenericParam::Impl);
            param_impl.concrete_trait
        }
    }
}

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
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);

    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let module_impls = db.module_impls(module_file_id.0)?;
    let syntax_db = db.upcast();
    let impl_ast = module_impls.get(&impl_def_id).to_maybe()?;

    // Generic params.
    let mut resolver = Resolver::new_with_inference(db, module_file_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &impl_ast.generic_params(syntax_db),
    );

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

    let attributes = ast_attributes_to_semantic(syntax_db, impl_ast.attributes(syntax_db));
    let resolved_lookback = Arc::new(resolver.lookback);
    Ok(ImplDeclarationData {
        diagnostics: diagnostics.build(),
        generic_params,
        concrete_trait,
        attributes,
        resolved_lookback,
    })
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplDefinitionData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    function_asts: OrderedHashMap<ImplFunctionId, ast::FunctionWithBody>,
}

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

/// Query implementation of [crate::db::SemanticGroup::priv_impl_definition_data].
pub fn priv_impl_definition_data(
    db: &dyn SemanticGroup,
    impl_def_id: ImplDefId,
) -> Maybe<ImplDefinitionData> {
    let defs_db = db.upcast();
    let syntax_db = db.upcast();

    let module_file_id = impl_def_id.module_file_id(defs_db);
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);

    let declaration_data = db.priv_impl_declaration_data(impl_def_id)?;
    let concrete_trait = declaration_data.concrete_trait?;

    let module_impls = db.module_impls(module_file_id.0)?;
    let impl_ast = module_impls.get(&impl_def_id).to_maybe()?;

    let lookup_context = ImplLookupContext {
        module_id: module_file_id.0,
        extra_modules: vec![],
        generic_params: declaration_data.generic_params,
    };
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
                Item::Constant(constant) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    constant.const_kw(syntax_db),
                ),
                Item::Module(module) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    module.module_kw(syntax_db),
                ),

                Item::Use(use_item) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    use_item.use_kw(syntax_db),
                ),
                Item::ExternFunction(extern_func) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    extern_func.extern_kw(syntax_db),
                ),
                Item::ExternType(extern_type) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    extern_type.extern_kw(syntax_db),
                ),
                Item::Trait(trt) => {
                    report_invalid_impl_item(syntax_db, &mut diagnostics, trt.trait_kw(syntax_db))
                }
                Item::Impl(imp) => {
                    report_invalid_impl_item(syntax_db, &mut diagnostics, imp.impl_kw(syntax_db))
                }
                Item::Struct(structure) => report_invalid_impl_item(
                    syntax_db,
                    &mut diagnostics,
                    structure.struct_kw(syntax_db),
                ),
                Item::Enum(enm) => {
                    report_invalid_impl_item(syntax_db, &mut diagnostics, enm.enum_kw(syntax_db))
                }
                Item::TypeAlias(ty) => {
                    report_invalid_impl_item(syntax_db, &mut diagnostics, ty.type_kw(syntax_db))
                }
                Item::FreeFunction(func) => {
                    let impl_function_id = db.intern_impl_function(ImplFunctionLongId(
                        module_file_id,
                        func.stable_ptr(),
                    ));
                    function_asts.insert(impl_function_id, func);
                    impl_item_names.insert(impl_function_id.name(defs_db));
                }
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
        if !tys
            .into_iter()
            .filter_map(|ty| db.type_info(lookup_context.clone(), ty).to_option())
            .all(|info| info.duplicatable)
        {
            return Err(diagnostics.report_by_ptr(stable_ptr, InvalidCopyTraitImpl));
        }
    }
    if trait_id == drop {
        let tys = get_inner_types(db, extract_matches!(generic_args[0], GenericArgumentId::Type))?;
        if !tys
            .into_iter()
            .filter_map(|ty| db.type_info(lookup_context.clone(), ty).to_option())
            .all(|info| info.droppable)
        {
            return Err(diagnostics.report_by_ptr(stable_ptr, InvalidDropTraitImpl));
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

/// Finds implementations for a concrete trait in a module.
fn find_impls_at_module(
    db: &dyn SemanticGroup,
    inference: &Inference<'_>,
    module_id: ModuleId,
    concrete_trait_id: ConcreteTraitId,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<Vec<UninferredImpl>> {
    let mut res = Vec::new();

    let mut impls = db.module_impls_ids(module_id)?;
    for use_id in db.module_uses_ids(module_id)? {
        if let Ok(ResolvedGenericItem::Impl(impl_def_id)) = db.use_resolved_item(use_id) {
            impls.push(impl_def_id);
        }
    }
    // TODO(spapini): Index better.
    for impl_def_id in impls {
        if !inference.can_impl_trait(impl_def_id, concrete_trait_id, stable_ptr) {
            continue;
        }
        res.push(UninferredImpl::Def(impl_def_id));
    }
    Ok(res)
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ImplLookupContext {
    pub module_id: ModuleId,
    pub extra_modules: Vec<ModuleId>,
    pub generic_params: Vec<semantic::GenericParam>,
}

/// An candidate impl for later inference.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum UninferredImpl {
    Def(ImplDefId),
    GenericParam(GenericParamId),
}
impl DebugWithDb<dyn SemanticGroup> for UninferredImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &dyn SemanticGroup) -> std::fmt::Result {
        match self {
            UninferredImpl::Def(impl_def) => write!(f, "{:?}", impl_def.full_path(db.upcast())),
            UninferredImpl::GenericParam(param) => {
                write!(f, "generic param {}", param.name(db.upcast()))
            }
        }
    }
}

/// Finds all the implementations of a concrete trait, in a specific lookup context.
fn find_impls_at_context(
    db: &dyn SemanticGroup,
    inference: &Inference<'_>,
    lookup_context: &ImplLookupContext,
    concrete_trait_id: ConcreteTraitId,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<OrderedHashSet<UninferredImpl>> {
    let mut res = OrderedHashSet::default();
    for generic_param in &lookup_context.generic_params {
        let GenericParam::Impl(param) = generic_param else {continue};
        let Ok(imp_concrete_trait_id) = param.concrete_trait else {continue};

        let mut temp_inference = inference.clone();
        if temp_inference.conform_traits(concrete_trait_id, imp_concrete_trait_id).is_err() {
            continue;
        }
        res.insert(UninferredImpl::GenericParam(generic_param.id()));
    }
    res.extend(find_impls_at_module(
        db,
        inference,
        lookup_context.module_id,
        concrete_trait_id,
        stable_ptr,
    )?);
    let core_module = core_module(db);
    for module_id in chain!(&lookup_context.extra_modules, [&core_module]) {
        if let Ok(imps) =
            find_impls_at_module(db, inference, *module_id, concrete_trait_id, stable_ptr)
        {
            res.extend(imps);
        }
    }
    for submodule in db.module_submodules_ids(lookup_context.module_id)? {
        res.extend(find_impls_at_module(
            db,
            inference,
            ModuleId::Submodule(submodule),
            concrete_trait_id,
            stable_ptr,
        )?);
    }
    for use_id in db.module_uses_ids(lookup_context.module_id)? {
        if let Ok(ResolvedGenericItem::Module(submodule)) = db.use_resolved_item(use_id) {
            res.extend(find_impls_at_module(
                db,
                inference,
                submodule,
                concrete_trait_id,
                stable_ptr,
            )?);
        }
    }
    Ok(res)
}

/// Infers a unique impl for a trait. If more or less than one found, fails and emits diagnostics.
pub fn infer_impl_at_context(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    inference: &mut Inference<'_>,
    lookup_context: &ImplLookupContext,
    concrete_trait_id: ConcreteTraitId,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<ImplId> {
    let uninferred_impl_id =
        match &find_impls_at_context(db, inference, lookup_context, concrete_trait_id, stable_ptr)?
            .into_iter()
            .collect_vec()[..]
        {
            &[] => {
                return Err(diagnostics
                    .report_by_ptr(stable_ptr, NoImplementationOfTrait { concrete_trait_id }));
            }
            &[uninferred_impl_id] => uninferred_impl_id,
            impls => {
                return Err(diagnostics.report_by_ptr(
                    stable_ptr,
                    MultipleImplementationOfTrait {
                        trait_id: concrete_trait_id.trait_id(db),
                        all_impl_ids: impls.to_vec(),
                    },
                ));
            }
        };
    Ok(match uninferred_impl_id {
        UninferredImpl::Def(impl_def_id) => inference
            .infer_impl_trait(impl_def_id, concrete_trait_id, stable_ptr)
            .map_err(|err| diagnostics.report_by_ptr(stable_ptr, InternalInferenceError(err)))?,
        UninferredImpl::GenericParam(param) => ImplId::GenericParameter(param),
    })
}

/// Checks if there is at least one impl that can be inferred for a specific concrete trait.
pub fn has_impl_at_context(
    db: &dyn SemanticGroup,
    inference: &Inference<'_>,
    lookup_context: &ImplLookupContext,
    concrete_trait_id: ConcreteTraitId,
    stable_ptr: SyntaxStablePtrId,
) -> Maybe<bool> {
    Ok(!find_impls_at_context(db, inference, lookup_context, concrete_trait_id, stable_ptr)?
        .is_empty())
}

// === Declaration ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplFunctionDeclarationData {
    pub function_declaration_data: FunctionDeclarationData,
    trait_function_id: Maybe<TraitFunctionId>,
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

/// Query implementation of [crate::db::SemanticGroup::impl_function_generic_params].
pub fn impl_function_generic_params(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<Vec<semantic::GenericParam>> {
    Ok(db
        .priv_impl_function_declaration_data(impl_function_id)?
        .function_declaration_data
        .generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_declaration_diagnostics].
pub fn impl_function_declaration_diagnostics(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_function_declaration_data(impl_function_id)
        .map(|data| data.function_declaration_data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_resolved_lookback].
pub fn impl_function_resolved_lookback(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db
        .priv_impl_function_declaration_data(impl_function_id)?
        .function_declaration_data
        .resolved_lookback)
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_trait_function].
pub fn impl_function_trait_function(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<TraitFunctionId> {
    db.priv_impl_function_declaration_data(impl_function_id)?.trait_function_id
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_function_declaration_data].
pub fn priv_impl_function_declaration_data(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<ImplFunctionDeclarationData> {
    let module_file_id = impl_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let impl_def_id = impl_function_id.impl_def_id(db.upcast());
    let data = db.priv_impl_definition_data(impl_def_id)?;
    let function_syntax = &data.function_asts[impl_function_id];
    let syntax_db = db.upcast();
    let declaration = function_syntax.declaration(syntax_db);
    let mut resolver = Resolver::new_with_inference(db, module_file_id);
    let impl_def_generic_params = db.impl_def_generic_params(impl_def_id)?;
    for generic_param in impl_def_generic_params {
        resolver.add_generic_param(generic_param);
    }
    let function_generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &declaration.generic_params(syntax_db),
    );

    let signature_syntax = declaration.signature(syntax_db);

    let mut environment = Environment::default();
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        FunctionSignatureId::Impl(impl_function_id),
        &mut environment,
    );

    let trait_function_id = validate_impl_function_signature(
        db,
        &mut diagnostics,
        impl_function_id,
        &signature_syntax,
        &signature,
        function_syntax,
        &function_generic_params,
    );

    let attributes = ast_attributes_to_semantic(syntax_db, function_syntax.attributes(syntax_db));
    let resolved_lookback = Arc::new(resolver.lookback);

    Ok(ImplFunctionDeclarationData {
        function_declaration_data: FunctionDeclarationData {
            diagnostics: diagnostics.build(),
            signature,
            generic_params: function_generic_params,
            environment,
            attributes,
            resolved_lookback,
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
    let declaraton_data = db.priv_impl_declaration_data(impl_def_id)?;
    let concrete_trait = declaraton_data.concrete_trait?;
    let concrete_trait_long_id = db.lookup_intern_concrete_trait(concrete_trait);
    let trait_id = concrete_trait_long_id.trait_id;
    let trait_functions = db.trait_functions(trait_id)?;
    let function_name = db.lookup_intern_impl_function(impl_function_id).name(db.upcast());
    let trait_function_id = *trait_functions.get(&function_name).ok_or_else(|| {
        diagnostics.report(
            function_syntax,
            FunctionNotMemberOfTrait { impl_def_id, impl_function_id, trait_id },
        )
    })?;
    let trait_signature = db.trait_function_signature(trait_function_id)?;

    // Find concrete trait substitution.
    let trait_generic_params = db.trait_generic_params(trait_id)?;
    let substitution =
        GenericSubstitution::new(&trait_generic_params, &concrete_trait_long_id.generic_args);
    let concrete_trait_signature = substitute_signature(db, substitution, trait_signature);

    // Match generics of the function.
    let trait_func_generics = db.trait_function_generic_params(trait_function_id)?;
    if impl_func_generics.len() != trait_func_generics.len() {
        diagnostics.report(
            &function_syntax.declaration(syntax_db).name(syntax_db),
            WrongNumberOfGenericArguments {
                expected: trait_func_generics.len(),
                actual: impl_func_generics.len(),
            },
        );
        return Ok(trait_function_id);
    }
    let substitution = GenericSubstitution::new(
        &trait_func_generics,
        &impl_func_generics
            .iter()
            .map(|param| {
                GenericArgumentId::Type(db.intern_type(TypeLongId::GenericParameter(param.id())))
            })
            .collect_vec(),
    );
    let concrete_trait_signature = substitute_signature(db, substitution, concrete_trait_signature);

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
                    ParamaterShouldBeReference { impl_def_id, impl_function_id, trait_id },
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

// === Body ===

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

/// Query implementation of [crate::db::SemanticGroup::impl_function_body_resolved_lookback].
pub fn impl_function_body_resolved_lookback(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db.priv_impl_function_body_data(impl_function_id)?.resolved_lookback)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_impl_function_body_data].
pub fn priv_impl_function_body_data(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Maybe<FunctionBodyData> {
    let defs_db = db.upcast();
    let module_file_id = impl_function_id.module_file_id(defs_db);
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let impl_def_id = impl_function_id.impl_def_id(defs_db);
    let data = db.priv_impl_definition_data(impl_def_id)?;
    let function_syntax = &data.function_asts[impl_function_id];
    // Compute declaration semantic.
    let declaration = db.priv_impl_function_declaration_data(impl_function_id)?;
    let mut resolver = Resolver::new_with_inference(db, module_file_id);
    for generic_param in declaration.function_declaration_data.generic_params {
        resolver.add_generic_param(generic_param);
    }
    let environment = declaration.function_declaration_data.environment;

    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(
        db,
        &mut diagnostics,
        resolver,
        Some(&declaration.function_declaration_data.signature),
        environment,
    );
    let function_body = function_syntax.body(db.upcast());
    let return_type = declaration.function_declaration_data.signature.return_type;
    let body_expr = compute_root_expr(&mut ctx, &function_body, return_type)?;
    let ComputationContext { exprs, statements, resolver, .. } = ctx;

    let direct_callees: HashSet<FunctionId> = exprs
        .iter()
        .filter_map(|(_id, expr)| try_extract_matches!(expr, Expr::FunctionCall))
        .map(|f| f.function)
        .collect();

    let expr_lookup: UnorderedHashMap<_, _> =
        exprs.iter().map(|(expr_id, expr)| (expr.stable_ptr(), expr_id)).collect();
    let resolved_lookback = Arc::new(resolver.lookback);
    Ok(FunctionBodyData {
        diagnostics: diagnostics.build(),
        expr_lookup,
        resolved_lookback,
        body: Arc::new(FunctionBody {
            exprs,
            statements,
            body_expr,
            direct_callees: direct_callees.into_iter().collect(),
        }),
    })
}
