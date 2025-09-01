use std::fmt::Write;
use std::hash::Hash;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    GenericItemId, GenericKind, GenericModuleItemId, GenericParamId, GenericParamLongId,
    LanguageElementId, LookupItemId, ModuleFileId, TraitId, TraitTypeId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, skip_diagnostic};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::ast::{
    AssociatedItemConstraints, GenericArgValue, OptionAssociatedItemConstraints,
};
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use cairo_lang_utils::{Intern, extract_matches};
use salsa::Database;
use syntax::node::TypedStablePtr;

use super::constant::{ConstValue, ConstValueId};
use super::imp::{ImplHead, ImplId, ImplLongId};
use super::resolve_trait_path;
use super::trt::ConcreteTraitTypeId;
use crate::db::{SemanticGroup, SemanticGroupData};
use crate::diagnostic::{
    NotFoundItemType, SemanticDiagnosticKind, SemanticDiagnostics, SemanticDiagnosticsBuilder,
};
use crate::expr::fmt::CountingWriter;
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::lookup_item::LookupItemEx;
use crate::resolve::{
    ResolutionContext, ResolvedConcreteItem, ResolvedGenericItem, Resolver, ResolverData,
};
use crate::substitution::SemanticRewriter;
use crate::types::{
    ImplTypeId, ShallowGenericArg, TypeHead, maybe_resolve_shallow_generic_arg_type, resolve_type,
};
use crate::{ConcreteTraitId, ConcreteTraitLongId, SemanticDiagnostic, TypeId, TypeLongId};

/// Generic argument.
/// A value assigned to a generic parameter.
/// May be a type, impl, constant, etc..
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject, salsa::Update)]
pub enum GenericArgumentId<'db> {
    Type(TypeId<'db>),
    Constant(ConstValueId<'db>),
    Impl(ImplId<'db>),
    NegImpl,
}
impl<'db> GenericArgumentId<'db> {
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
    pub fn head(&self, db: &'db dyn SemanticGroup) -> Option<GenericArgumentHead<'db>> {
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
impl<'db> DebugWithDb<'db> for GenericArgumentId<'db> {
    type Db = dyn SemanticGroup;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn SemanticGroup) -> std::fmt::Result {
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
#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub enum GenericArgumentHead<'db> {
    Type(TypeHead<'db>),
    Impl(ImplHead<'db>),
    Const,
    NegImpl,
}

/// Generic parameter.
#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject, salsa::Update)]
pub enum GenericParam<'db> {
    Type(GenericParamType<'db>),
    // TODO(spapini): Add expression.
    Const(GenericParamConst<'db>),
    Impl(GenericParamImpl<'db>),
    NegImpl(GenericParamImpl<'db>),
}
impl<'db> GenericParam<'db> {
    pub fn id(&self) -> GenericParamId<'db> {
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
    pub fn stable_ptr(&self, db: &'db dyn Database) -> ast::GenericParamPtr<'db> {
        self.id().stable_ptr(db)
    }
    /// Returns the generic param as a generic argument.
    pub fn as_arg(&self, db: &'db dyn SemanticGroup) -> GenericArgumentId<'db> {
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
impl<'db> DebugWithDb<'db> for GenericParam<'db> {
    type Db = dyn SemanticGroup;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn SemanticGroup) -> std::fmt::Result {
        write!(f, "{:?}", self.id().debug(db))
    }
}

/// Converts each generic param to a generic argument that passes the same generic param.
pub fn generic_params_to_args<'db>(
    params: &[GenericParam<'db>],
    db: &'db dyn SemanticGroup,
) -> Vec<GenericArgumentId<'db>> {
    params.iter().map(|param| param.as_arg(db)).collect()
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(dyn SemanticGroup)]
pub struct GenericParamType<'db> {
    pub id: GenericParamId<'db>,
}
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(dyn SemanticGroup)]
pub struct GenericParamConst<'db> {
    pub id: GenericParamId<'db>,
    pub ty: TypeId<'db>,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(dyn SemanticGroup)]
pub struct GenericParamImpl<'db> {
    pub id: GenericParamId<'db>,
    pub concrete_trait: Maybe<ConcreteTraitId<'db>>,
    pub type_constraints: OrderedHashMap<TraitTypeId<'db>, TypeId<'db>>,
}

/// The result of the computation of the semantic model of a generic parameter.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn SemanticGroup)]
pub struct GenericParamData<'db> {
    pub generic_param: Maybe<GenericParam<'db>>,
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    pub resolver_data: Arc<ResolverData<'db>>,
}

/// The result of the computation of the semantic model of a generic parameters list.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn SemanticGroup)]
pub struct GenericParamsData<'db> {
    pub generic_params: Vec<GenericParam<'db>>,
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    pub resolver_data: Arc<ResolverData<'db>>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::generic_param_semantic].
pub fn generic_param_semantic<'db>(
    db: &'db dyn SemanticGroup,
    generic_param_id: GenericParamId<'db>,
) -> Maybe<GenericParam<'db>> {
    db.priv_generic_param_data(generic_param_id, false)?.generic_param
}

/// Query implementation of [crate::db::SemanticGroup::generic_param_diagnostics].
pub fn generic_param_diagnostics<'db>(
    db: &'db dyn SemanticGroup,
    generic_param_id: GenericParamId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_generic_param_data(generic_param_id, false)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::generic_param_resolver_data].
pub fn generic_param_resolver_data<'db>(
    db: &'db dyn SemanticGroup,
    generic_param_id: GenericParamId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db.priv_generic_param_data(generic_param_id, false)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::generic_impl_param_trait].
pub fn generic_impl_param_trait<'db>(
    db: &'db dyn SemanticGroup,
    generic_param_id: GenericParamId<'db>,
) -> Maybe<TraitId<'db>> {
    let syntax_db = db;
    let module_file_id = generic_param_id.module_file_id(db);
    let option_generic_params_syntax = generic_param_generic_params_list(db, generic_param_id)?;
    let generic_params_syntax = extract_matches!(
        option_generic_params_syntax,
        ast::OptionWrappedGenericParamList::WrappedGenericParamList
    );
    let generic_param_syntax = generic_params_syntax
        .generic_params(syntax_db)
        .elements(syntax_db)
        .find(|param_syntax| {
            GenericParamLongId(module_file_id, param_syntax.stable_ptr(syntax_db)).intern(db)
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

    resolve_trait_path(syntax_db, &mut diagnostics, &mut resolver, &trait_path_syntax)
}

/// Query implementation of
/// [crate::db::SemanticGroup::generic_impl_param_shallow_trait_generic_args].
pub fn generic_impl_param_shallow_trait_generic_args<'db>(
    db: &'db dyn SemanticGroup,
    impl_def_id: GenericParamId<'db>,
) -> Maybe<&'db [(GenericParamId<'db>, ShallowGenericArg<'db>)]> {
    generic_impl_param_shallow_trait_generic_args_helper(db, impl_def_id)
        .as_ref()
        .map(|res| res.as_slice())
        .map_err(|e| *e)
}

/// Helper for [generic_impl_param_shallow_trait_generic_args]
/// The actual query implementation, separated to allow returning a reference.
#[salsa::tracked(returns(ref))]
fn generic_impl_param_shallow_trait_generic_args_helper<'db>(
    db: &'db dyn SemanticGroup,
    generic_param_id: GenericParamId<'db>,
) -> Maybe<Vec<(GenericParamId<'db>, ShallowGenericArg<'db>)>> {
    let syntax_db: &dyn Database = db;
    let module_file_id = generic_param_id.module_file_id(db);
    let mut diagnostics: cairo_lang_diagnostics::DiagnosticsBuilder<'_, SemanticDiagnostic<'_>> =
        SemanticDiagnostics::default();
    let parent_item_id = generic_param_id.generic_item(db);
    let lookup_item: LookupItemId<'_> = parent_item_id.into();
    let context_resolver_data = lookup_item.resolver_context(db)?;
    let inference_id = InferenceId::GenericParam(generic_param_id);
    let mut resolver =
        Resolver::with_data(db, (*context_resolver_data).clone_with_inference_id(db, inference_id));
    resolver.set_feature_config(
        &lookup_item,
        &lookup_item.untyped_stable_ptr(db).lookup(db),
        &mut diagnostics,
    );
    let generic_params_syntax = extract_matches!(
        generic_param_generic_params_list(db, generic_param_id)?,
        ast::OptionWrappedGenericParamList::WrappedGenericParamList
    );

    let mut opt_generic_param_syntax = None;
    for param_syntax in generic_params_syntax.generic_params(syntax_db).elements(syntax_db) {
        let cur_generic_param_id =
            GenericParamLongId(module_file_id, param_syntax.stable_ptr(syntax_db)).intern(db);
        resolver.add_generic_param(cur_generic_param_id);

        if cur_generic_param_id == generic_param_id {
            opt_generic_param_syntax = Some(param_syntax);
            break;
        }
    }

    let generic_param_syntax =
        opt_generic_param_syntax.expect("Query called on a non existing generic param.");

    let trait_path_syntax = match generic_param_syntax {
        ast::GenericParam::ImplNamed(syntax) => syntax.trait_path(syntax_db),
        ast::GenericParam::ImplAnonymous(syntax) => syntax.trait_path(syntax_db),
        ast::GenericParam::NegativeImpl(syntax) => syntax.trait_path(syntax_db),
        _ => {
            unreachable!(
                "generic_impl_param_shallow_trait_generic_args called on a non impl generic param."
            )
        }
    };

    let ResolvedGenericItem::Trait(trait_id) = resolver.resolve_generic_path_with_args(
        &mut diagnostics,
        &trait_path_syntax,
        NotFoundItemType::Trait,
        ResolutionContext::Default,
    )?
    else {
        return Err(skip_diagnostic());
    };
    let generic_params = db
        .trait_generic_params_ids(trait_id)?
        .iter()
        .map(|param_syntax| {
            GenericParamLongId(trait_id.module_file_id(db), param_syntax.stable_ptr(db)).intern(db)
        })
        .collect::<Vec<_>>();

    let elements = trait_path_syntax.segments(db).elements(db);
    let Some(last) = elements.last() else {
        return Ok(Vec::new());
    };

    match last {
        ast::PathSegment::Simple(_) => Ok(Vec::new()),
        ast::PathSegment::WithGenericArgs(path_segment_with_generic_args) => {
            let generic_args =
                path_segment_with_generic_args.generic_args(db).generic_args(db).elements_vec(db);

            let arg_syntax_per_param = resolver.get_arg_syntax_per_param(
                &mut diagnostics,
                &generic_params,
                &generic_args,
            )?;
            Ok(generic_params
                .iter()
                .filter_map(|generic_param| {
                    let value = arg_syntax_per_param.get(generic_param)?;
                    let GenericArgValue::Expr(expr) = value else {
                        return None;
                    };
                    let x = maybe_resolve_shallow_generic_arg_type(
                        db,
                        &mut diagnostics,
                        &mut resolver,
                        &expr.expr(db),
                    )?;
                    Some((*generic_param, x))
                })
                .collect::<Vec<_>>())
        }
        ast::PathSegment::Missing(_) => Ok(Vec::new()),
    }
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_generic_param_data].
pub fn priv_generic_param_data<'db>(
    db: &'db dyn SemanticGroup,
    generic_param_id: GenericParamId<'db>,
    in_cycle: bool,
) -> Maybe<GenericParamData<'db>> {
    if in_cycle {
        let mut diagnostics = SemanticDiagnostics::default();
        return Ok(GenericParamData {
            generic_param: Err(diagnostics.report(
                generic_param_id.stable_ptr(db).untyped(),
                SemanticDiagnosticKind::ImplRequirementCycle,
            )),
            diagnostics: diagnostics.build(),
            resolver_data: Arc::new(ResolverData::new(
                generic_param_id.module_file_id(db),
                InferenceId::GenericParam(generic_param_id),
            )),
        });
    }
    let module_file_id = generic_param_id.module_file_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let parent_item_id = generic_param_id.generic_item(db);
    let lookup_item: LookupItemId<'_> = parent_item_id.into();
    let context_resolver_data = lookup_item.resolver_context(db)?;
    let inference_id = InferenceId::GenericParam(generic_param_id);
    let mut resolver =
        Resolver::with_data(db, (*context_resolver_data).clone_with_inference_id(db, inference_id));
    resolver.set_feature_config(
        &lookup_item,
        &lookup_item.untyped_stable_ptr(db).lookup(db),
        &mut diagnostics,
    );
    let generic_params_syntax = extract_matches!(
        generic_param_generic_params_list(db, generic_param_id)?,
        ast::OptionWrappedGenericParamList::WrappedGenericParamList
    );

    let mut opt_generic_param_syntax = None;
    for param_syntax in generic_params_syntax.generic_params(db).elements(db) {
        let cur_generic_param_id =
            GenericParamLongId(module_file_id, param_syntax.stable_ptr(db)).intern(db);
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
    inference.finalize(&mut diagnostics, generic_param_syntax.stable_ptr(db).untyped());

    let param_semantic = inference.rewrite(param_semantic).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamData {
        generic_param: Ok(param_semantic),
        diagnostics: diagnostics.build(),
        resolver_data,
    })
}

/// Cycle handling for [crate::db::SemanticGroup::priv_generic_param_data].
pub fn priv_generic_param_data_cycle<'db>(
    db: &'db dyn SemanticGroup,
    _input: SemanticGroupData,
    generic_param_id: GenericParamId<'db>,
    _in_cycle: bool,
) -> Maybe<GenericParamData<'db>> {
    priv_generic_param_data(db, generic_param_id, true)
}

/// Query implementation of [crate::db::SemanticGroup::generic_params_type_constraints].
pub fn generic_params_type_constraints<'db>(
    db: &'db dyn SemanticGroup,
    generic_params: Vec<GenericParamId<'db>>,
) -> Vec<(TypeId<'db>, TypeId<'db>)> {
    let mut constraints = vec![];
    for param in &generic_params {
        let Ok(GenericParam::Impl(imp)) = db.generic_param_semantic(*param) else {
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
        let ConcreteTraitLongId { trait_id, generic_args } = concrete_trait_id.long(db);
        if trait_id != &db.core_info().type_eq_trt {
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
fn generic_param_generic_params_list<'db>(
    db: &'db dyn SemanticGroup,
    generic_param_id: GenericParamId<'db>,
) -> Maybe<ast::OptionWrappedGenericParamList<'db>> {
    let generic_param_long_id = generic_param_id.long(db);

    // The generic params list is 2 level up the tree.
    let syntax_db = db;
    let wrapped_generic_param_list = generic_param_long_id.1.0.nth_parent(syntax_db, 2);

    Ok(ast::OptionWrappedGenericParamListPtr(wrapped_generic_param_list).lookup(syntax_db))
}

/// Returns the semantic model of a generic parameters list given the list AST, and updates the
/// diagnostics and resolver accordingly.
pub fn semantic_generic_params<'db>(
    db: &'db dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics<'db>,
    resolver: &mut Resolver<'db>,
    module_file_id: ModuleFileId<'db>,
    generic_params: &ast::OptionWrappedGenericParamList<'db>,
) -> Vec<GenericParam<'db>> {
    semantic_generic_params_ex(db, diagnostics, resolver, module_file_id, generic_params, false)
}

pub fn semantic_generic_params_ex<'db>(
    db: &'db dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics<'db>,
    resolver: &mut Resolver<'db>,
    module_file_id: ModuleFileId<'db>,
    generic_params: &ast::OptionWrappedGenericParamList<'db>,
    in_cycle: bool,
) -> Vec<GenericParam<'db>> {
    let syntax_db = db;
    match generic_params {
        syntax::node::ast::OptionWrappedGenericParamList::Empty(_) => vec![],
        syntax::node::ast::OptionWrappedGenericParamList::WrappedGenericParamList(syntax) => syntax
            .generic_params(syntax_db)
            .elements(syntax_db)
            .filter_map(|param_syntax| {
                let generic_param_id =
                    GenericParamLongId(module_file_id, param_syntax.stable_ptr(syntax_db))
                        .intern(db);
                let generic_param_data =
                    db.priv_generic_param_data(generic_param_id, in_cycle).ok()?;
                let generic_param = generic_param_data.generic_param;
                diagnostics.extend(generic_param_data.diagnostics);
                resolver.add_generic_param(generic_param_id);
                resolver
                    .data
                    .used_uses
                    .extend(generic_param_data.resolver_data.used_uses.iter().copied());
                generic_param.ok()
            })
            .collect(),
    }
}

/// Returns true if negative impls are enabled in the module.
fn are_negative_impls_enabled<'db>(
    db: &dyn SemanticGroup,
    module_file_id: ModuleFileId<'db>,
) -> bool {
    let owning_crate = module_file_id.0.owning_crate(db);
    let Some(config) = db.crate_config(owning_crate) else { return false };
    config.settings.experimental_features.negative_impls
}

/// Returns true if associated_item_constraints is enabled in the module.
fn is_associated_item_constraints_enabled(
    db: &dyn SemanticGroup,
    module_file_id: ModuleFileId<'_>,
) -> bool {
    let owning_crate = module_file_id.0.owning_crate(db);
    db.crate_config(owning_crate)
        .is_some_and(|c| c.settings.experimental_features.associated_item_constraints)
}

/// Computes the semantic model of a generic parameter give its ast.
fn semantic_from_generic_param_ast<'db>(
    db: &'db dyn SemanticGroup,
    resolver: &mut Resolver<'db>,
    diagnostics: &mut SemanticDiagnostics<'db>,
    module_file_id: ModuleFileId<'db>,
    param_syntax: &ast::GenericParam<'db>,
    parent_item_id: GenericItemId<'db>,
) -> GenericParam<'db> {
    let syntax_db = db;
    let id = GenericParamLongId(module_file_id, param_syntax.stable_ptr(syntax_db)).intern(db);
    let mut item_constraints_into_option = |constraint| match constraint {
        OptionAssociatedItemConstraints::Empty(_) => None,
        OptionAssociatedItemConstraints::AssociatedItemConstraints(associated_type_args) => {
            if !is_associated_item_constraints_enabled(db, module_file_id) {
                diagnostics.report(
                    associated_type_args.stable_ptr(syntax_db),
                    SemanticDiagnosticKind::TypeConstraintsSyntaxNotEnabled,
                );
            }
            Some(associated_type_args)
        }
    };
    match param_syntax {
        ast::GenericParam::Type(_) => GenericParam::Type(GenericParamType { id }),
        ast::GenericParam::Const(syntax) => {
            let ty = resolve_type(db, diagnostics, resolver, &syntax.ty(db));
            GenericParam::Const(GenericParamConst { id, ty })
        }
        ast::GenericParam::ImplNamed(syntax) => {
            let path_syntax = syntax.trait_path(db);
            let item_constrains = item_constraints_into_option(syntax.type_constrains(db));
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
            let path_syntax = syntax.trait_path(db);
            let item_constrains = item_constraints_into_option(syntax.type_constrains(db));
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
                diagnostics.report(
                    param_syntax.stable_ptr(syntax_db),
                    SemanticDiagnosticKind::NegativeImplsNotEnabled,
                );
            }

            if !matches!(parent_item_id, GenericItemId::ModuleItem(GenericModuleItemId::Impl(_))) {
                diagnostics.report(
                    param_syntax.stable_ptr(syntax_db),
                    SemanticDiagnosticKind::NegativeImplsOnlyOnImpls,
                );
            }

            let path_syntax = syntax.trait_path(db);
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
fn impl_generic_param_semantic<'db>(
    db: &'db dyn SemanticGroup,
    resolver: &mut Resolver<'db>,
    diagnostics: &mut SemanticDiagnostics<'db>,
    path_syntax: &ast::ExprPath<'db>,
    item_constraints: Option<AssociatedItemConstraints<'db>>,
    id: GenericParamId<'db>,
) -> GenericParamImpl<'db> {
    let syntax_db = db;
    let concrete_trait = resolver
        .resolve_concrete_path(diagnostics, path_syntax, NotFoundItemType::Trait)
        .and_then(|resolved_item| match resolved_item {
            ResolvedConcreteItem::Trait(id) | ResolvedConcreteItem::SelfTrait(id) => Ok(id),
            _ => Err(diagnostics
                .report(path_syntax.stable_ptr(syntax_db), SemanticDiagnosticKind::UnknownTrait)),
        });
    let type_constraints = concrete_trait
        .ok()
        .and_then(|concrete_trait| {
            item_constraints.map(|type_constraints| (concrete_trait, type_constraints))
        })
        .map(|(concrete_trait_id, constraints)| {
            let mut map = OrderedHashMap::default();

            for constraint in constraints.associated_item_constraints(syntax_db).elements(db) {
                let Ok(trait_type_id_opt) = db.trait_type_by_name(
                    concrete_trait_id.trait_id(db),
                    constraint.item(syntax_db).text(syntax_db).into(),
                ) else {
                    continue;
                };
                let Some(trait_type_id) = trait_type_id_opt else {
                    diagnostics.report(
                        constraint.stable_ptr(syntax_db),
                        SemanticDiagnosticKind::NonTraitTypeConstrained {
                            identifier: constraint.item(db).text(db).into(),
                            concrete_trait_id,
                        },
                    );
                    return map;
                };

                let concrete_trait_type_id =
                    ConcreteTraitTypeId::new_from_data(db, concrete_trait_id, trait_type_id);
                match map.entry(trait_type_id) {
                    Entry::Vacant(entry) => {
                        entry.insert(resolve_type(
                            db,
                            diagnostics,
                            resolver,
                            &constraint.value(syntax_db),
                        ));
                    }
                    Entry::Occupied(_) => {
                        diagnostics.report(
                            path_syntax.stable_ptr(syntax_db),
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
    generic_args: &[GenericArgumentId<'_>],
    f: &mut CountingWriter<'_, '_>,
    db: &dyn SemanticGroup,
) -> std::fmt::Result {
    let mut generic_args = generic_args.iter();
    if let Some(first) = generic_args.next() {
        // Soft limit for the number of chars in the formatted type.
        const CHARS_BOUND: usize = 500;
        write!(f, "::<")?;
        write!(f, "{}", &first.format(db))?;

        for arg in generic_args {
            write!(f, ", ")?;
            if f.count() > CHARS_BOUND {
                // If the formatted type is becoming too long, add short version of arguments.
                write!(f, "{}", &arg.short_name(db))?;
            } else {
                write!(f, "{}", &arg.format(db))?;
            }
        }
        write!(f, ">")?;
    }
    Ok(())
}
