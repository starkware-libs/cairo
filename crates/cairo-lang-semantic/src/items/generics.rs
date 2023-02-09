use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    GenericItemId, GenericKind, GenericParamId, GenericParamLongId, ModuleFileId,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;

use super::imp::ImplId;
use crate::db::SemanticGroup;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnosticKind, SemanticDiagnostics};
use crate::literals::LiteralId;
use crate::resolve_path::{ResolvedConcreteItem, Resolver};
use crate::{ConcreteTraitId, TypeId};

/// Generic argument.
/// A value assigned to a generic parameter.
/// May be a type, impl, constant, etc..
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
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

/// Generic parameter.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
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

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct GenericParamType {
    pub id: GenericParamId,
}
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct GenericParamConst {
    pub id: GenericParamId,
}
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct GenericParamImpl {
    pub id: GenericParamId,
    pub concrete_trait: Maybe<ConcreteTraitId>,
}

pub fn generic_param_semantic(
    db: &dyn SemanticGroup,
    generic_param_id: GenericParamId,
) -> Maybe<GenericParam> {
    let generic_item = generic_param_id.generic_item(db.upcast());
    let generic_params = generic_item_generic_params(db, generic_item)?;
    Ok(*generic_params.iter().find(|generic_param| generic_param.id() == generic_param_id).unwrap())
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
    }
}

/// Returns the parameters of the given function signature's AST.
pub fn semantic_generic_params(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    module_file_id: ModuleFileId,
    generic_params: &ast::OptionWrappedGenericParamList,
) -> Vec<GenericParam> {
    let syntax_db = db.upcast();

    match generic_params {
        syntax::node::ast::OptionWrappedGenericParamList::Empty(_) => vec![],
        syntax::node::ast::OptionWrappedGenericParamList::WrappedGenericParamList(syntax) => syntax
            .generic_params(syntax_db)
            .elements(syntax_db)
            .iter()
            .map(|param_syntax| {
                let param_semantic = semantic_from_generic_param_ast(
                    db,
                    resolver,
                    diagnostics,
                    module_file_id,
                    param_syntax,
                );
                resolver.add_generic_param(param_semantic);
                param_semantic
            })
            .collect(),
    }

    // TODO(spapini): Make sure the generic params are fully resolved.
}

/// Computes the semantic model of a generic parameter give its ast.
fn semantic_from_generic_param_ast(
    db: &dyn SemanticGroup,
    resolver: &mut Resolver<'_>,
    diagnostics: &mut SemanticDiagnostics,
    module_file_id: ModuleFileId,
    param_syntax: &ast::GenericParam,
) -> GenericParam {
    let id = db.intern_generic_param(GenericParamLongId(module_file_id, param_syntax.stable_ptr()));
    match param_syntax {
        ast::GenericParam::Type(_) => GenericParam::Type(GenericParamType { id }),
        ast::GenericParam::Const(_) => GenericParam::Const(GenericParamConst { id }),
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
