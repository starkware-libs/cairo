use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    GenericItemId, GenericKind, GenericParamId, GenericParamLongId, LanguageElementId,
    LookupItemId, ModuleFileId, TraitId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::{extract_matches, try_extract_matches};
use syntax::node::db::SyntaxGroup;
use syntax::node::stable_ptr::SyntaxStablePtr;

use super::imp::{ImplHead, ImplId};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::InferenceId;
use crate::literals::LiteralId;
use crate::lookup_item::LookupItemEx;
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::types::{resolve_type, TypeHead};
use crate::{ConcreteTraitId, SemanticDiagnostic, TypeId};

/// Generic argument.
/// A value assigned to a generic parameter.
/// May be a type, impl, constant, etc..
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum GenericArgumentId {
    Type(TypeId),
    Literal(LiteralId),
    Impl(ImplId), // TODO(spapini): impls and constants as generic values.
    NegImpl,
}
impl GenericArgumentId {
    pub fn kind(&self) -> GenericKind {
        match self {
            GenericArgumentId::Type(_) => GenericKind::Type,
            GenericArgumentId::Literal(_) => GenericKind::Const,
            GenericArgumentId::Impl(_) => GenericKind::Impl,
            GenericArgumentId::NegImpl => GenericKind::NegImpl,
        }
    }
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        match self {
            GenericArgumentId::Type(ty) => ty.format(db),
            GenericArgumentId::Literal(lit) => lit.format(db),
            GenericArgumentId::Impl(imp) => format!("{:?}", imp.debug(db.elongate())),
            GenericArgumentId::NegImpl => "_".into(),
        }
    }
    /// Returns the [GenericArgumentHead] for a generic argument if available.
    pub fn head(&self, db: &dyn SemanticGroup) -> Option<GenericArgumentHead> {
        Some(match self {
            GenericArgumentId::Type(ty) => GenericArgumentHead::Type(ty.head(db)?),
            GenericArgumentId::Literal(_) => GenericArgumentHead::Const,
            GenericArgumentId::Impl(impl_id) => GenericArgumentHead::Impl(impl_id.head(db)?),
            GenericArgumentId::NegImpl => GenericArgumentHead::NegImpl,
        })
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
            GenericArgumentId::Literal(id) => write!(f, "{:?}", id.debug(db)),
            GenericArgumentId::Impl(id) => write!(f, "{:?}", id.debug(db)),
            GenericArgumentId::NegImpl => write!(f, "_"),
        }
    }
}

/// Head of a generic argument. A non-param non-variable generic argument has a head, which
/// represents the kind of the root node in its tree. This is used for caching queries for fast
/// lookups when the generic argument is not completely inferred yet.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericArgumentHead {
    Type(TypeHead),
    Impl(ImplHead),
    Const,
    NegImpl,
}

/// Generic parameter.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
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
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct GenericParamImpl {
    pub id: GenericParamId,
    pub concrete_trait: Maybe<ConcreteTraitId>,
}

/// The result of the computation of the semantic model of a generic parameter.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct GenericParamData {
    pub generic_param: GenericParam,
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

/// Query implementation of [crate::db::SemanticGroup::generic_param_semantic].
pub fn generic_param_semantic(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
) -> Maybe<GenericParam> {
    Ok(db.generic_param_data(generic_param_id)?.generic_param)
}

/// Query implementation of [crate::db::SemanticGroup::generic_param_diagnostics].
pub fn generic_param_diagnostics(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
) -> Diagnostics<SemanticDiagnostic> {
    db.generic_param_data(generic_param_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::generic_param_resolver_data].
pub fn generic_param_resolver_data(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.generic_param_data(generic_param_id)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::generic_param_data].
pub fn generic_param_data(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
) -> Maybe<GenericParamData> {
    let syntax_db: &dyn SyntaxGroup = db.upcast();
    let module_file_id = generic_param_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let generic_item_id = generic_param_id.generic_item(db.upcast());
    // Right now, generic consts are allowed only in extern functions and types.
    let allow_consts =
        matches!(generic_item_id, GenericItemId::ExternFunc(_) | GenericItemId::ExternType(_));
    let lookup_item: LookupItemId = generic_item_id.into();
    let context_resolver_data = lookup_item.resolver_context(db)?;
    let inference_id = InferenceId::GenericParam(generic_param_id);
    let mut resolver =
        Resolver::with_data(db, (*context_resolver_data).clone_with_inference_id(db, inference_id));
    let generic_params_syntax = extract_matches!(
        generic_param_generic_params_list(db, generic_param_id)?,
        ast::OptionWrappedGenericParamList::WrappedGenericParamList
    );

    let mut opt_generic_param_syntax = None;
    for param_syntax in
        generic_params_syntax.generic_params(syntax_db).elements(syntax_db).into_iter()
    {
        let cur_generic_param_id =
            db.intern_generic_param(GenericParamLongId(module_file_id, param_syntax.stable_ptr()));
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
        allow_consts,
    );
    if let Some((stable_ptr, inference_err)) = resolver.inference().finalize() {
        inference_err.report(
            &mut diagnostics,
            stable_ptr.unwrap_or(generic_param_syntax.stable_ptr().untyped()),
        );
    }
    let param_semantic = resolver.inference().rewrite(param_semantic).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamData {
        generic_param: param_semantic,
        diagnostics: diagnostics.build(),
        resolver_data,
    })
}

/// Cycle handling for [crate::db::SemanticGroup::generic_param_data].
pub fn generic_param_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    generic_param_id: &GenericParamId,
) -> Maybe<GenericParamData> {
    let diagnostics = &mut SemanticDiagnostics::new(
        generic_param_id.module_file_id(db.upcast()).file_id(db.upcast())?,
    );
    Err(diagnostics.report_by_ptr(
        generic_param_id.stable_ptr(db.upcast()).untyped(),
        SemanticDiagnosticKind::ImplRequirementCycle,
    ))
}

/// Returns the generic parameters list AST node of a generic parameter.
fn generic_param_generic_params_list(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
) -> Maybe<ast::OptionWrappedGenericParamList> {
    let generic_param_long_id = db.lookup_intern_generic_param(generic_param_id);

    // Traverse up the tree to the generic params list ptr.
    let SyntaxStablePtr::Child { parent, .. } =
        db.lookup_intern_stable_ptr(generic_param_long_id.1.0)
    else {
        panic!()
    };
    let SyntaxStablePtr::Child { parent, .. } = db.lookup_intern_stable_ptr(parent) else {
        panic!()
    };

    let generic_param_list = ast::OptionWrappedGenericParamListPtr(parent).lookup(db.upcast());
    Ok(generic_param_list)
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
            db.intern_generic_param(GenericParamLongId(module_file_id, param_syntax.stable_ptr()))
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

    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let inference_id = InferenceId::GenericImplParamTrait(generic_param_id);
    // TODO(spapini): We should not create a new resolver -  we are missing the other generic params
    // in the context.
    // Remove also GenericImplParamTrait.
    let mut resolver = Resolver::new(db, module_file_id, inference_id);

    try_extract_matches!(
        resolver.resolve_generic_path_with_args(
            &mut diagnostics,
            &trait_path_syntax,
            NotFoundItemType::Trait,
        )?,
        ResolvedGenericItem::Trait
    )
    .ok_or_else(|| diagnostics.report(&trait_path_syntax, NotATrait))
}

/// Returns the semantic model of a generic parameters list given the list AST, and updates the
/// diagnostics and resolver accordingly.
pub fn semantic_generic_params(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    module_file_id: ModuleFileId,
    generic_params: &ast::OptionWrappedGenericParamList,
) -> Maybe<Vec<GenericParam>> {
    let syntax_db = db.upcast();
    let res = match generic_params {
        syntax::node::ast::OptionWrappedGenericParamList::Empty(_) => Ok(vec![]),
        syntax::node::ast::OptionWrappedGenericParamList::WrappedGenericParamList(syntax) => syntax
            .generic_params(syntax_db)
            .elements(syntax_db)
            .iter()
            .map(|param_syntax| {
                let generic_param_id = db.intern_generic_param(GenericParamLongId(
                    module_file_id,
                    param_syntax.stable_ptr(),
                ));
                let generic_param_data = db.generic_param_data(generic_param_id)?;
                let generic_param = generic_param_data.generic_param;
                diagnostics.diagnostics.extend(generic_param_data.diagnostics);
                resolver.add_generic_param(generic_param_id);
                Ok(generic_param)
            })
            .collect::<Result<Vec<_>, _>>(),
    };
    res
}

/// Computes the semantic model of a generic parameter give its ast.
fn semantic_from_generic_param_ast(
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    diagnostics: &mut SemanticDiagnostics,
    module_file_id: ModuleFileId,
    param_syntax: &ast::GenericParam,
    allow_consts: bool,
) -> GenericParam {
    let id = db.intern_generic_param(GenericParamLongId(module_file_id, param_syntax.stable_ptr()));
    match param_syntax {
        ast::GenericParam::Type(_) => GenericParam::Type(GenericParamType { id }),
        ast::GenericParam::Const(syntax) => {
            if !allow_consts {
                diagnostics
                    .report(param_syntax, SemanticDiagnosticKind::ConstGenericParamNotSupported);
            }
            let ty = resolve_type(db, diagnostics, resolver, &syntax.ty(db.upcast()));
            GenericParam::Const(GenericParamConst { id, ty })
        }
        ast::GenericParam::ImplNamed(syntax) => {
            let path_syntax = syntax.trait_path(db.upcast());
            GenericParam::Impl(impl_generic_param_semantic(resolver, diagnostics, &path_syntax, id))
        }
        ast::GenericParam::ImplAnonymous(syntax) => {
            let path_syntax = syntax.trait_path(db.upcast());
            GenericParam::Impl(impl_generic_param_semantic(resolver, diagnostics, &path_syntax, id))
        }
        ast::GenericParam::NegativeImpl(syntax) => {
            diagnostics.report(param_syntax, SemanticDiagnosticKind::NegativeImplsNotEnabled);

            let path_syntax = syntax.trait_path(db.upcast());
            GenericParam::NegImpl(impl_generic_param_semantic(
                resolver,
                diagnostics,
                &path_syntax,
                id,
            ))
        }
    }
}

/// Computes the semantic model of an impl generic parameter given its trait path.
fn impl_generic_param_semantic(
    resolver: &mut Resolver<'_>,
    diagnostics: &mut SemanticDiagnostics,
    path_syntax: &ast::ExprPath,
    id: GenericParamId,
) -> GenericParamImpl {
    let concrete_trait = resolver
        .resolve_concrete_path(diagnostics, path_syntax, NotFoundItemType::Trait)
        .and_then(|resolved_item| {
            try_extract_matches!(resolved_item, ResolvedConcreteItem::Trait).ok_or_else(|| {
                diagnostics.report(path_syntax, SemanticDiagnosticKind::UnknownTrait)
            })
        });
    GenericParamImpl { id, concrete_trait }
}
