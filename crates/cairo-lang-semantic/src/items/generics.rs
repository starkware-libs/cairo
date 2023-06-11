use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    GenericItemId, GenericKind, GenericParamId, GenericParamLongId, LanguageElementId,
    ModuleFileId, TraitId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::{extract_matches, try_extract_matches};
use syntax::node::ast::{ExprPath, OptionWrappedGenericParamList};
use syntax::node::db::SyntaxGroup;

use super::imp::{ImplHead, ImplId};
use crate::db::SemanticGroup;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnosticKind, SemanticDiagnostics};
use crate::literals::LiteralId;
use crate::resolve::{ResolvedConcreteItem, ResolvedGenericItem, Resolver, ResolverData};
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
}
impl GenericArgumentId {
    pub fn kind(&self) -> GenericKind {
        match self {
            GenericArgumentId::Type(_) => GenericKind::Type,
            GenericArgumentId::Literal(_) => GenericKind::Const,
            GenericArgumentId::Impl(_) => GenericKind::Impl,
        }
    }
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        match self {
            GenericArgumentId::Type(ty) => ty.format(db),
            GenericArgumentId::Literal(lit) => lit.format(db),
            GenericArgumentId::Impl(imp) => format!("{:?}", imp.debug(db.elongate())),
        }
    }
    /// Returns the [GenericArgumentHead] for a generic argument if available.
    pub fn head(&self, db: &dyn SemanticGroup) -> Option<GenericArgumentHead> {
        Some(match self {
            GenericArgumentId::Type(ty) => GenericArgumentHead::Type(ty.head(db)?),
            GenericArgumentId::Literal(_) => GenericArgumentHead::Const,
            GenericArgumentId::Impl(impl_id) => GenericArgumentHead::Impl(impl_id.head(db)?),
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
}

/// Generic parameter.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub enum GenericParam {
    Type(GenericParamType),
    // TODO(spapini): Add expression.
    Const(GenericParamConst),
    Impl(GenericParamImpl),
}
impl GenericParam {
    pub fn id(&self) -> GenericParamId {
        match self {
            GenericParam::Type(param) => param.id,
            GenericParam::Const(param) => param.id,
            GenericParam::Impl(param) => param.id,
        }
    }
    pub fn kind(&self) -> GenericKind {
        match self {
            GenericParam::Type(_) => GenericKind::Type,
            GenericParam::Const(_) => GenericKind::Const,
            GenericParam::Impl(_) => GenericKind::Impl,
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
    let generic_item = generic_param_id.generic_item(db.upcast());
    let generic_params = generic_item_generic_params(db, generic_item)?;
    Ok(*generic_params.iter().find(|generic_param| generic_param.id() == generic_param_id).unwrap())
}

/// Computes the semantic model of a generic parameter given a specific context.
pub fn generic_param_semantic_with_context(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
    resolver: &mut Resolver<'_>,
    diagnostics: &mut SemanticDiagnostics,
    allow_consts: bool,
) -> Maybe<GenericParam> {
    let syntax_db: &dyn SyntaxGroup = db.upcast();
    let generic_item = generic_param_id.generic_item(db.upcast());
    let module_file_id = generic_item_module_file_id(db, generic_item);
    let generic_params_syntax = extract_matches!(
        generic_item_generic_params_syntax(db, generic_item)?,
        OptionWrappedGenericParamList::WrappedGenericParamList
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

    // If the generic parameter is an impl, we need to add to the resolver context all other
    // generic types, and all other generic impls that implement traits which are required by
    // the current impl. Not all impls are added to avoid cycles.
    if let ast::GenericParam::Impl(syntax) = generic_param_syntax.clone() {
        let trait_path_syntax = syntax.trait_path(db.upcast());
        if let Some(trait_id) = extract_trait_id_from_path(trait_path_syntax, resolver) {
            let required_impls = db.trait_required_impls(trait_id)?;
            for cur_generic_param_syntax in
                generic_params_syntax.generic_params(syntax_db).elements(syntax_db).iter()
            {
                let cur_generic_param_id = db.intern_generic_param(GenericParamLongId(
                    module_file_id,
                    cur_generic_param_syntax.stable_ptr(),
                ));
                if cur_generic_param_id == generic_param_id {
                    continue;
                }
                // Filter impls which are not required by the current impl to avoid cycles.
                if let ast::GenericParam::Impl(generic_impl_syntax) = cur_generic_param_syntax {
                    let trait_path_syntax = generic_impl_syntax.trait_path(db.upcast());
                    if let Some(trait_id) = extract_trait_id_from_path(trait_path_syntax, resolver)
                    {
                        if !required_impls.contains(&trait_id) {
                            continue;
                        }
                    }
                }
                // TODO(Gil): Handle real dependency cycles, and raise diagnostics.
                let cur_generic_param = generic_param_semantic_with_context(
                    db,
                    cur_generic_param_id,
                    resolver,
                    diagnostics,
                    allow_consts,
                )?;
                resolver.add_generic_param(cur_generic_param);
            }
        }
    };

    let param_semantic = semantic_from_generic_param_ast(
        db,
        resolver,
        diagnostics,
        module_file_id,
        &generic_param_syntax,
        allow_consts,
    );
    Ok(param_semantic)
}

/// Returns the trait id of the trait represented by a given path.
fn extract_trait_id_from_path(
    trait_path_syntax: ExprPath,
    resolver: &mut Resolver<'_>,
) -> Option<TraitId> {
    // A temporary diagnostics object is created to avoid adding the same diagnostic multiple times.
    // Any diagnostic related to the generic parameter will be added when this parameter semantic
    // model is computed.
    let diagnostics = &mut SemanticDiagnostics::new(resolver.module_file_id);
    let trait_id = resolver
        .resolve_generic_path_with_args(diagnostics, &trait_path_syntax, NotFoundItemType::Trait)
        .ok()
        .and_then(|generic_item| try_extract_matches!(generic_item, ResolvedGenericItem::Trait));
    trait_id
}

/// Returns the module file id of a given generic item.
fn generic_item_module_file_id(
    db: &dyn SemanticGroup,
    generic_item: GenericItemId,
) -> ModuleFileId {
    match generic_item {
        GenericItemId::FreeFunc(id) => id.module_file_id(db.upcast()),
        GenericItemId::ExternFunc(id) => id.module_file_id(db.upcast()),
        GenericItemId::TraitFunc(id) => id.module_file_id(db.upcast()),
        GenericItemId::ImplFunc(id) => id.module_file_id(db.upcast()),
        GenericItemId::Trait(id) => id.module_file_id(db.upcast()),
        GenericItemId::Impl(id) => id.module_file_id(db.upcast()),
        GenericItemId::Struct(id) => id.module_file_id(db.upcast()),
        GenericItemId::Enum(id) => id.module_file_id(db.upcast()),
        GenericItemId::ExternType(id) => id.module_file_id(db.upcast()),
        GenericItemId::TypeAlias(id) => id.module_file_id(db.upcast()),
        GenericItemId::ImplAlias(id) => id.module_file_id(db.upcast()),
    }
}

fn generic_item_generic_params(
    db: &dyn SemanticGroup,
    generic_item: GenericItemId,
) -> Maybe<Vec<GenericParam>> {
    match generic_item {
        GenericItemId::FreeFunc(id) => db.free_function_generic_params(id),
        GenericItemId::ExternFunc(id) => db.extern_function_declaration_generic_params(id),
        GenericItemId::TraitFunc(id) => db.trait_function_generic_params(id),
        GenericItemId::ImplFunc(id) => db.impl_function_generic_params(id),
        GenericItemId::Trait(id) => db.trait_generic_params(id),
        GenericItemId::Impl(id) => db.impl_def_generic_params(id),
        GenericItemId::Struct(id) => db.struct_generic_params(id),
        GenericItemId::Enum(id) => db.enum_generic_params(id),
        GenericItemId::ExternType(id) => db.extern_type_declaration_generic_params(id),
        GenericItemId::TypeAlias(id) => db.type_alias_generic_params(id),
        GenericItemId::ImplAlias(id) => db.impl_alias_generic_params(id),
    }
}

/// Returns the ast of a generic parameters list of a given generic item.
pub fn generic_item_generic_params_syntax(
    db: &dyn SemanticGroup,
    generic_item: GenericItemId,
) -> Maybe<OptionWrappedGenericParamList> {
    let module_file_id = generic_item_module_file_id(db, generic_item);
    let syntax_db = db.upcast();
    let generic_params_syntax: OptionWrappedGenericParamList = match generic_item {
        GenericItemId::FreeFunc(free_function_id) => {
            let module_free_functions = db.module_free_functions(module_file_id.0)?;
            let function_syntax = module_free_functions.get(&free_function_id).to_maybe()?;
            function_syntax.declaration(syntax_db).generic_params(syntax_db)
        }
        GenericItemId::ExternFunc(extern_function_id) => {
            let module_extern_functions = db.module_extern_functions(module_file_id.0)?;
            let function_syntax = module_extern_functions.get(&extern_function_id).to_maybe()?;
            function_syntax.declaration(syntax_db).generic_params(syntax_db)
        }
        GenericItemId::TraitFunc(trait_function_id) => {
            let trait_id = trait_function_id.trait_id(db.upcast());
            let functions_asts = db.trait_function_asts(trait_id)?;
            let function_syntax = functions_asts.get(&trait_function_id).to_maybe()?;
            function_syntax.declaration(syntax_db).generic_params(syntax_db)
        }
        GenericItemId::ImplFunc(impl_function_id) => {
            let impl_def_id = impl_function_id.impl_def_id(db.upcast());
            let functions_asts = db.impl_def_functions_asts(impl_def_id)?;
            let function_syntax = functions_asts.get(&impl_function_id).to_maybe()?;
            function_syntax.declaration(syntax_db).generic_params(syntax_db)
        }
        GenericItemId::Trait(trait_id) => {
            let module_traits = db.module_traits(module_file_id.0)?;
            let trait_syntax = module_traits.get(&trait_id).to_maybe()?;
            trait_syntax.generic_params(syntax_db)
        }
        GenericItemId::Impl(impl_id) => {
            let module_impls = db.module_impls(module_file_id.0)?;
            let impl_syntax = module_impls.get(&impl_id).to_maybe()?;
            impl_syntax.generic_params(syntax_db)
        }
        GenericItemId::Struct(struct_id) => {
            let module_structs = db.module_structs(module_file_id.0)?;
            let struct_syntax = module_structs.get(&struct_id).to_maybe()?;
            struct_syntax.generic_params(syntax_db)
        }
        GenericItemId::Enum(enum_id) => {
            let module_enums = db.module_enums(module_file_id.0)?;
            let enum_syntax = module_enums.get(&enum_id).to_maybe()?;
            enum_syntax.generic_params(syntax_db)
        }
        GenericItemId::ExternType(extern_type_id) => {
            let module_extern_types = db.module_extern_types(module_file_id.0)?;
            let extern_type_syntax = module_extern_types.get(&extern_type_id).to_maybe()?;
            extern_type_syntax.generic_params(syntax_db)
        }
        GenericItemId::TypeAlias(type_alias_id) => {
            let module_type_aliases = db.module_type_aliases(module_file_id.0)?;
            let type_alias_syntax = module_type_aliases.get(&type_alias_id).to_maybe()?;
            type_alias_syntax.generic_params(syntax_db)
        }
        GenericItemId::ImplAlias(impl_alias_id) => {
            let module_impl_aliases = db.module_impl_aliases(module_file_id.0)?;
            let impl_alias_syntax = module_impl_aliases.get(&impl_alias_id).to_maybe()?;
            impl_alias_syntax.generic_params(syntax_db)
        }
    };
    Ok(generic_params_syntax)
}

/// Returns the semantic model of a generic parameters list given the list AST.
pub fn semantic_generic_params(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    module_file_id: ModuleFileId,
    generic_params: &ast::OptionWrappedGenericParamList,
    allow_consts: bool,
) -> Maybe<Vec<GenericParam>> {
    let syntax_db = db.upcast();

    let res = match generic_params {
        syntax::node::ast::OptionWrappedGenericParamList::Empty(_) => Ok(vec![]),
        syntax::node::ast::OptionWrappedGenericParamList::WrappedGenericParamList(syntax) => syntax
            .generic_params(syntax_db)
            .elements(syntax_db)
            .iter()
            .map(|param_syntax| {
                let id = db.intern_generic_param(GenericParamLongId(
                    module_file_id,
                    param_syntax.stable_ptr(),
                ));
                let param_semantic = generic_param_semantic_with_context(
                    db,
                    id,
                    resolver,
                    diagnostics,
                    allow_consts,
                )?;
                resolver.add_generic_param(param_semantic);
                Ok(param_semantic)
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
                    .report(param_syntax, SemanticDiagnosticKind::ConstGenericParamSupported);
            }
            let ty = resolve_type(db, diagnostics, resolver, &syntax.ty(db.upcast()));
            GenericParam::Const(GenericParamConst { id, ty })
        }
        ast::GenericParam::Impl(syntax) => {
            let path_syntax = syntax.trait_path(db.upcast());
            let concrete_trait = resolver
                .resolve_concrete_path(diagnostics, &path_syntax, NotFoundItemType::Trait)
                .and_then(|resolved_item| {
                    try_extract_matches!(resolved_item, ResolvedConcreteItem::Trait).ok_or_else(
                        || diagnostics.report(&path_syntax, SemanticDiagnosticKind::UnknownTrait),
                    )
                });
            GenericParam::Impl(GenericParamImpl { id, concrete_trait })
        }
    }
}
