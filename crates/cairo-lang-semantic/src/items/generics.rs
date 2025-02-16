use std::hash::Hash;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    GenericItemId, GenericKind, GenericModuleItemId, GenericParamId, GenericParamLongId,
    LanguageElementId, LookupItemId, ModuleFileId, TraitId, TraitTypeId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::ast::{AssociatedItemConstraints, OptionAssociatedItemConstraints};
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use cairo_lang_utils::{Intern, LookupIntern, extract_matches};
use syntax::node::TypedStablePtr;
use syntax::node::db::SyntaxGroup;

use super::constant::{ConstValue, ConstValueId};
use super::imp::{ImplHead, ImplId, ImplLongId};
use super::resolve_trait_path;
use super::trt::ConcreteTraitTypeId;
use crate::db::SemanticGroup;
use crate::diagnostic::{
    NotFoundItemType, SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder,
};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::lookup_item::LookupItemEx;
use crate::resolve::{ResolvedConcreteItem, Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::types::{ImplTypeId, TypeHead, resolve_type};
use crate::{ConcreteTraitId, ConcreteTraitLongId, SemanticDiagnostic, TypeId, TypeLongId};

/// Generic argument.
/// A value assigned to a generic parameter.
/// May be a type, impl, constant, etc..
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum GenericArgumentId {
    Type(TypeId),
    Constant(ConstValueId),
    Impl(ImplId),
    NegImpl,
}
impl GenericArgumentId {
    pub fn kind(&self) -> GenericKind {
        match self {
            GenericArgumentId::Type(_) => GenericKind::Type,
            GenericArgumentId::Constant(_) => GenericKind::Const,
            GenericArgumentId::Impl(_) => GenericKind::Impl,
            GenericArgumentId::NegImpl => GenericKind::NegImpl,
        }
    }
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        match self {
            GenericArgumentId::Type(ty) => ty.format(db),
            GenericArgumentId::Constant(value) => value.format(db),
            GenericArgumentId::Impl(imp) => imp.format(db),
            GenericArgumentId::NegImpl => "_".into(),
        }
    }
    /// Returns the [GenericArgumentHead] for a generic argument if available.
    pub fn head(&self, db: &dyn SemanticGroup) -> Option<GenericArgumentHead> {
        Some(match self {
            GenericArgumentId::Type(ty) => GenericArgumentHead::Type(ty.head(db)?),
            GenericArgumentId::Constant(_) => GenericArgumentHead::Const,
            GenericArgumentId::Impl(impl_id) => GenericArgumentHead::Impl(impl_id.head(db)?),
            GenericArgumentId::NegImpl => GenericArgumentHead::NegImpl,
        })
    }
    /// Returns true if the generic argument does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        match self {
            GenericArgumentId::Type(type_id) => type_id.is_fully_concrete(db),
            GenericArgumentId::Constant(const_value_id) => const_value_id.is_fully_concrete(db),
            GenericArgumentId::Impl(impl_id) => impl_id.is_fully_concrete(db),
            GenericArgumentId::NegImpl => true,
        }
    }
    /// Returns true if the generic argument does not depend on impl or type variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        match self {
            GenericArgumentId::Type(type_id) => type_id.is_var_free(db),
            GenericArgumentId::Constant(const_value_id) => const_value_id.is_var_free(db),
            GenericArgumentId::Impl(impl_id) => impl_id.is_var_free(db),
            GenericArgumentId::NegImpl => true,
        }
    }
    /// Short name of the generic argument.
    pub fn short_name(&self, db: &dyn SemanticGroup) -> String {
        if let GenericArgumentId::Type(ty) = self { ty.short_name(db) } else { self.format(db) }
    }
}
impl DebugWithDb<dyn SemanticGroup> for GenericArgumentId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        match self {
            GenericArgumentId::Type(id) => write!(f, "{:?}", id.debug(db)),
            GenericArgumentId::Constant(id) => write!(f, "{:?}", id.debug(db)),
            GenericArgumentId::Impl(id) => write!(f, "{:?}", id.debug(db)),
            GenericArgumentId::NegImpl => write!(f, "_"),
        }
    }
}

/// Head of a generic argument.
///
/// A non-param non-variable generic argument has a head, which represents the kind of the root node
/// in its tree. This is used for caching queries for fast lookups when the generic argument is not
/// completely inferred yet.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericArgumentHead {
    Type(TypeHead),
    Impl(ImplHead),
    Const,
    NegImpl,
}

/// Generic parameter.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum GenericParam {
    Type(GenericParamType),
    // TODO(spapini): Add expression.
    Const(GenericParamConst),
    Impl(GenericParamImpl),
    NegImpl(GenericParamImpl),
}
impl GenericParam {
    pub fn id(&self) -> GenericParamId {
        match self {
            GenericParam::Type(param) => param.id,
            GenericParam::Const(param) => param.id,
            GenericParam::Impl(param) => param.id,
            GenericParam::NegImpl(param) => param.id,
        }
    }
    pub fn kind(&self) -> GenericKind {
        match self {
            GenericParam::Type(_) => GenericKind::Type,
            GenericParam::Const(_) => GenericKind::Const,
            GenericParam::Impl(_) => GenericKind::Impl,
            GenericParam::NegImpl(_) => GenericKind::NegImpl,
        }
    }
    pub fn stable_ptr(&self, db: &dyn DefsGroup) -> ast::GenericParamPtr {
        self.id().stable_ptr(db)
    }
    /// Returns the generic param as a generic argument.
    pub fn as_arg(&self, db: &dyn SemanticGroup) -> GenericArgumentId {
        match self {
            GenericParam::Type(param_type) => {
                GenericArgumentId::Type(TypeLongId::GenericParameter(param_type.id).intern(db))
            }
            GenericParam::Const(param_const) => {
                GenericArgumentId::Constant(ConstValue::Generic(param_const.id).intern(db))
            }
            GenericParam::Impl(param_impl) => {
                GenericArgumentId::Impl(ImplLongId::GenericParameter(param_impl.id).intern(db))
            }
            GenericParam::NegImpl(_) => GenericArgumentId::NegImpl,
        }
    }
}
impl DebugWithDb<dyn SemanticGroup> for GenericParam {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{:?}", self.id().debug(db))
    }
}

/// Converts each generic param to a generic argument that passes the same generic param.
pub fn generic_params_to_args(
    params: &[GenericParam],
    db: &dyn SemanticGroup,
) -> Vec<GenericArgumentId> {
    params.iter().map(|param| param.as_arg(db)).collect()
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct GenericParamType {
    pub id: GenericParamId,
}
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct GenericParamConst {
    pub id: GenericParamId,
    pub ty: TypeId,
}
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct GenericParamImpl {
    pub id: GenericParamId,
    pub concrete_trait: Maybe<ConcreteTraitId>,
    pub type_constraints: OrderedHashMap<TraitTypeId, TypeId>,
}
impl Hash for GenericParamImpl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.concrete_trait.hash(state);
        self.type_constraints.iter().for_each(|(trait_type_id, type_id)| {
            trait_type_id.hash(state);
            type_id.hash(state);
        });
    }
}

/// The result of the computation of the semantic model of a generic parameter.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct GenericParamData {
    pub generic_param: Maybe<GenericParam>,
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub resolver_data: Arc<ResolverData>,
}

/// The result of the computation of the semantic model of a generic parameters list.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct GenericParamsData {
    pub generic_params: Vec<GenericParam>,
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub resolver_data: Arc<ResolverData>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::generic_param_semantic].
pub fn generic_param_semantic(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
) -> Maybe<GenericParam> {
    db.priv_generic_param_data(generic_param_id, false)?.generic_param
}

/// Query implementation of [crate::db::SemanticGroup::generic_param_diagnostics].
pub fn generic_param_diagnostics(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_generic_param_data(generic_param_id, false)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::generic_param_resolver_data].
pub fn generic_param_resolver_data(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_generic_param_data(generic_param_id, false)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::generic_impl_param_trait].
pub fn generic_impl_param_trait(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
) -> Maybe<TraitId> {
    let syntax_db = db.upcast();
    let module_file_id = generic_param_id.module_file_id(db.upcast());
    let option_generic_params_syntax = generic_param_generic_params_list(db, generic_param_id)?;
    let generic_params_syntax = extract_matches!(
        option_generic_params_syntax,
        ast::OptionWrappedGenericParamList::WrappedGenericParamList
    );
    let generic_param_syntax = generic_params_syntax
        .generic_params(syntax_db)
        .elements(syntax_db)
        .into_iter()
        .find(|param_syntax| {
            GenericParamLongId(module_file_id, param_syntax.stable_ptr()).intern(db)
                == generic_param_id
        })
        .unwrap();

    let trait_path_syntax = match generic_param_syntax {
        ast::GenericParam::ImplNamed(syntax) => syntax.trait_path(syntax_db),
        ast::GenericParam::ImplAnonymous(syntax) => syntax.trait_path(syntax_db),
        _ => {
            panic!("generic_impl_param_trait() called on a non impl generic param.")
        }
    };

    let mut diagnostics = SemanticDiagnostics::default();
    let inference_id = InferenceId::GenericImplParamTrait(generic_param_id);
    // TODO(spapini): We should not create a new resolver -  we are missing the other generic params
    // in the context.
    // Remove also GenericImplParamTrait.
    let mut resolver = Resolver::new(db, module_file_id, inference_id);

    resolve_trait_path(&mut diagnostics, &mut resolver, &trait_path_syntax)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_generic_param_data].
pub fn priv_generic_param_data(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
    in_cycle: bool,
) -> Maybe<GenericParamData> {
    if in_cycle {
        let mut diagnostics = SemanticDiagnostics::default();
        return Ok(GenericParamData {
            generic_param: Err(diagnostics.report(
                generic_param_id.stable_ptr(db.upcast()).untyped(),
                SemanticDiagnosticKind::ImplRequirementCycle,
            )),
            diagnostics: diagnostics.build(),
            resolver_data: Arc::new(ResolverData::new(
                generic_param_id.module_file_id(db.upcast()),
                InferenceId::GenericParam(generic_param_id),
            )),
        });
    }
    let syntax_db: &dyn SyntaxGroup = db.upcast();
    let module_file_id = generic_param_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();
    let parent_item_id = generic_param_id.generic_item(db.upcast());
    let lookup_item: LookupItemId = parent_item_id.into();
    let context_resolver_data = lookup_item.resolver_context(db)?;
    let inference_id = InferenceId::GenericParam(generic_param_id);
    let mut resolver =
        Resolver::with_data(db, (*context_resolver_data).clone_with_inference_id(db, inference_id));
    resolver.set_feature_config(
        &lookup_item,
        &lookup_item.untyped_stable_ptr(db.upcast()).lookup(db.upcast()),
        &mut diagnostics,
    );
    let generic_params_syntax = extract_matches!(
        generic_param_generic_params_list(db, generic_param_id)?,
        ast::OptionWrappedGenericParamList::WrappedGenericParamList
    );

    let mut opt_generic_param_syntax = None;
    for param_syntax in
        generic_params_syntax.generic_params(syntax_db).elements(syntax_db).into_iter()
    {
        let cur_generic_param_id =
            GenericParamLongId(module_file_id, param_syntax.stable_ptr()).intern(db);
        resolver.add_generic_param(cur_generic_param_id);

        if cur_generic_param_id == generic_param_id {
            opt_generic_param_syntax = Some(param_syntax);
        }
    }
    let generic_param_syntax =
        opt_generic_param_syntax.expect("Query called on a non existing generic param.");
    let param_semantic = semantic_from_generic_param_ast(
        db,
        &mut resolver,
        &mut diagnostics,
        module_file_id,
        &generic_param_syntax,
        parent_item_id,
    );
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, generic_param_syntax.stable_ptr().untyped());

    let param_semantic = inference.rewrite(param_semantic).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamData {
        generic_param: Ok(param_semantic),
        diagnostics: diagnostics.build(),
        resolver_data,
    })
}

/// Cycle handling for [crate::db::SemanticGroup::priv_generic_param_data].
pub fn priv_generic_param_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    generic_param_id: &GenericParamId,
    _in_cycle: &bool,
) -> Maybe<GenericParamData> {
    priv_generic_param_data(db, *generic_param_id, true)
}

/// Query implementation of [crate::db::SemanticGroup::generic_params_type_constraints].
pub fn generic_params_type_constraints(
    db: &dyn SemanticGroup,
    generic_params: Vec<GenericParamId>,
) -> Vec<(TypeId, TypeId)> {
    let mut constraints = vec![];
    for param in &generic_params {
        let GenericParam::Impl(imp) = db.generic_param_semantic(*param).unwrap() else {
            continue;
        };
        let Ok(concrete_trait_id) = imp.concrete_trait else {
            continue;
        };
        for (trait_ty, ty1) in imp.type_constraints {
            let impl_type = TypeLongId::ImplType(ImplTypeId::new(
                ImplLongId::GenericParameter(*param).intern(db),
                trait_ty,
                db,
            ))
            .intern(db);
            constraints.push((impl_type, ty1));
        }
        let ConcreteTraitLongId { trait_id, generic_args } = concrete_trait_id.lookup_intern(db);
        if trait_id != db.core_info().type_eq_trt {
            continue;
        }
        let [GenericArgumentId::Type(ty0), GenericArgumentId::Type(ty1)] = generic_args.as_slice()
        else {
            unreachable!("TypeEqual should have 2 arguments");
        };
        constraints.push((*ty0, *ty1));
    }
    constraints
}

// --- Helpers ---

/// Returns the generic parameters list AST node of a generic parameter.
fn generic_param_generic_params_list(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
) -> Maybe<ast::OptionWrappedGenericParamList> {
    let generic_param_long_id = generic_param_id.lookup_intern(db);

    // The generic params list is 2 level up the tree.
    let syntax_db = db.upcast();
    let wrapped_generic_param_list = generic_param_long_id.1.0.nth_parent(syntax_db, 2);

    Ok(ast::OptionWrappedGenericParamListPtr(wrapped_generic_param_list).lookup(syntax_db))
}

/// Returns the semantic model of a generic parameters list given the list AST, and updates the
/// diagnostics and resolver accordingly.
pub fn semantic_generic_params(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    module_file_id: ModuleFileId,
    generic_params: &ast::OptionWrappedGenericParamList,
) -> Vec<GenericParam> {
    semantic_generic_params_ex(db, diagnostics, resolver, module_file_id, generic_params, false)
}

pub fn semantic_generic_params_ex(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    module_file_id: ModuleFileId,
    generic_params: &ast::OptionWrappedGenericParamList,
    in_cycle: bool,
) -> Vec<GenericParam> {
    let syntax_db = db.upcast();
    match generic_params {
        syntax::node::ast::OptionWrappedGenericParamList::Empty(_) => vec![],
        syntax::node::ast::OptionWrappedGenericParamList::WrappedGenericParamList(syntax) => syntax
            .generic_params(syntax_db)
            .elements(syntax_db)
            .iter()
            .filter_map(|param_syntax| {
                let generic_param_id =
                    GenericParamLongId(module_file_id, param_syntax.stable_ptr()).intern(db);
                let generic_param_data =
                    db.priv_generic_param_data(generic_param_id, in_cycle).ok()?;
                let generic_param = generic_param_data.generic_param;
                diagnostics.extend(generic_param_data.diagnostics);
                resolver.add_generic_param(generic_param_id);
                resolver
                    .data
                    .used_items
                    .extend(generic_param_data.resolver_data.used_items.iter().copied());
                generic_param.ok()
            })
            .collect(),
    }
}

/// Returns true if negative impls are enabled in the module.
fn are_negative_impls_enabled(db: &dyn SemanticGroup, module_file_id: ModuleFileId) -> bool {
    let owning_crate = module_file_id.0.owning_crate(db.upcast());
    let Some(config) = db.crate_config(owning_crate) else { return false };
    config.settings.experimental_features.negative_impls
}

/// Returns true if associated_item_constraints is enabled in the module.
fn is_associated_item_constraints_enabled(
    db: &dyn SemanticGroup,
    module_file_id: ModuleFileId,
) -> bool {
    let owning_crate = module_file_id.0.owning_crate(db.upcast());
    db.crate_config(owning_crate)
        .is_some_and(|c| c.settings.experimental_features.associated_item_constraints)
}

/// Computes the semantic model of a generic parameter give its ast.
fn semantic_from_generic_param_ast(
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    diagnostics: &mut SemanticDiagnostics,
    module_file_id: ModuleFileId,
    param_syntax: &ast::GenericParam,
    parent_item_id: GenericItemId,
) -> GenericParam {
    let id = GenericParamLongId(module_file_id, param_syntax.stable_ptr()).intern(db);
    let mut item_constraints_into_option = |constraint| match constraint {
        OptionAssociatedItemConstraints::Empty(_) => None,
        OptionAssociatedItemConstraints::AssociatedItemConstraints(associated_type_args) => {
            if !is_associated_item_constraints_enabled(db, module_file_id) {
                diagnostics.report(
                    associated_type_args.stable_ptr(),
                    SemanticDiagnosticKind::TypeConstraintsSyntaxNotEnabled,
                );
            }
            Some(associated_type_args)
        }
    };
    match param_syntax {
        ast::GenericParam::Type(_) => GenericParam::Type(GenericParamType { id }),
        ast::GenericParam::Const(syntax) => {
            let ty = resolve_type(db, diagnostics, resolver, &syntax.ty(db.upcast()));
            GenericParam::Const(GenericParamConst { id, ty })
        }
        ast::GenericParam::ImplNamed(syntax) => {
            let path_syntax = syntax.trait_path(db.upcast());
            let item_constrains = item_constraints_into_option(syntax.type_constrains(db.upcast()));
            GenericParam::Impl(impl_generic_param_semantic(
                db,
                resolver,
                diagnostics,
                &path_syntax,
                item_constrains,
                id,
            ))
        }
        ast::GenericParam::ImplAnonymous(syntax) => {
            let path_syntax = syntax.trait_path(db.upcast());
            let item_constrains = item_constraints_into_option(syntax.type_constrains(db.upcast()));
            GenericParam::Impl(impl_generic_param_semantic(
                db,
                resolver,
                diagnostics,
                &path_syntax,
                item_constrains,
                id,
            ))
        }
        ast::GenericParam::NegativeImpl(syntax) => {
            if !are_negative_impls_enabled(db, module_file_id) {
                diagnostics.report(param_syntax, SemanticDiagnosticKind::NegativeImplsNotEnabled);
            }

            if !matches!(parent_item_id, GenericItemId::ModuleItem(GenericModuleItemId::Impl(_))) {
                diagnostics.report(param_syntax, SemanticDiagnosticKind::NegativeImplsOnlyOnImpls);
            }

            let path_syntax = syntax.trait_path(db.upcast());
            GenericParam::NegImpl(impl_generic_param_semantic(
                db,
                resolver,
                diagnostics,
                &path_syntax,
                None,
                id,
            ))
        }
    }
}

/// Computes the semantic model of an impl generic parameter given its trait path.
fn impl_generic_param_semantic(
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    diagnostics: &mut SemanticDiagnostics,
    path_syntax: &ast::ExprPath,
    item_constraints: Option<AssociatedItemConstraints>,
    id: GenericParamId,
) -> GenericParamImpl {
    let concrete_trait = resolver
        .resolve_concrete_path(diagnostics, path_syntax, NotFoundItemType::Trait)
        .and_then(|resolved_item| match resolved_item {
            ResolvedConcreteItem::Trait(id) | ResolvedConcreteItem::SelfTrait(id) => Ok(id),
            _ => Err(diagnostics.report(path_syntax, SemanticDiagnosticKind::UnknownTrait)),
        });
    let type_constraints = concrete_trait
        .ok()
        .and_then(|concrete_trait| {
            item_constraints.map(|type_constraints| (concrete_trait, type_constraints))
        })
        .map(|(concrete_trait_id, constraints)| {
            let mut map = OrderedHashMap::default();

            for constraint in
                constraints.associated_item_constraints(db.upcast()).elements(db.upcast())
            {
                let Some(trait_type_id) = db
                    .trait_type_by_name(
                        concrete_trait_id.trait_id(db),
                        constraint.item(db.upcast()).text(db.upcast()),
                    )
                    .unwrap()
                else {
                    diagnostics.report(
                        constraint.stable_ptr(),
                        SemanticDiagnosticKind::NonTraitTypeConstrained {
                            identifier: constraint.item(db.upcast()).text(db.upcast()),
                            concrete_trait_id,
                        },
                    );
                    return map;
                };

                let concrete_trait_type_id =
                    ConcreteTraitTypeId::new(db, concrete_trait_id, trait_type_id);
                match map.entry(trait_type_id) {
                    Entry::Vacant(entry) => {
                        entry.insert(resolve_type(
                            db,
                            diagnostics,
                            resolver,
                            &constraint.value(db.upcast()),
                        ));
                    }
                    Entry::Occupied(_) => {
                        diagnostics.report(
                            path_syntax,
                            SemanticDiagnosticKind::DuplicateTypeConstraint {
                                concrete_trait_type_id,
                            },
                        );
                    }
                }
            }
            map
        })
        .unwrap_or_default();

    GenericParamImpl { id, concrete_trait, type_constraints }
}

/// Formats a list of generic arguments.
pub fn fmt_generic_args(
    generic_args: &[GenericArgumentId],
    f: &mut std::fmt::Formatter<'_>,
    db: &(dyn SemanticGroup + 'static),
) -> std::fmt::Result {
    if !generic_args.is_empty() {
        write!(f, "::<")?;
        for (i, arg) in generic_args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg.format(db))?;
        }
        write!(f, ">")?;
    }
    Ok(())
}
