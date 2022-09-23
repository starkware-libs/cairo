#[cfg(test)]
#[path = "resolve_path_test.rs"]
mod test;

use std::collections::HashMap;

use defs::ids::{
    GenericFunctionId, GenericParamId, GenericTypeId, LanguageElementId, ModuleId, ModuleItemId,
};
use filesystem::ids::CrateLongId;
use smol_str::SmolStr;
use syntax::node::ast::{self};
use syntax::node::helpers::GetIdentifier;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;
use utils::{OptionFrom, OptionHelper};

use crate::corelib::core_module;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::types::resolve_type;
use crate::{
    ConcreteFunction, ConcreteType, FunctionId, FunctionLongId, GenericArgumentId, TypeId,
    TypeLongId,
};

pub enum ResolvedItem {
    Module(ModuleId),
    GenericFunction(GenericFunctionId),
    GenericType(GenericTypeId),
    Function(FunctionId),
    Type(TypeId),
}

impl OptionFrom<ResolvedItem> for FunctionId {
    fn option_from(other: ResolvedItem) -> Option<Self> {
        if let ResolvedItem::Function(res) = other { Some(res) } else { None }
    }
}
impl OptionFrom<ResolvedItem> for TypeId {
    fn option_from(other: ResolvedItem) -> Option<Self> {
        if let ResolvedItem::Type(res) = other { Some(res) } else { None }
    }
}

// Scope information needed to resolve paths.
pub struct ResolveScope {
    // Current module in which to resolve the path.
    pub module_id: ModuleId,
    // Generic parameters accessible to the resolver.
    pub generic_params: HashMap<SmolStr, GenericParamId>,
}
impl ResolveScope {
    pub fn new(
        db: &dyn SemanticGroup,
        module_id: ModuleId,
        generic_params: &[GenericParamId],
    ) -> Self {
        Self {
            module_id,
            generic_params: generic_params
                .iter()
                .map(|generic_param| (generic_param.name(db.upcast()), *generic_param))
                .collect(),
        }
    }
}

/// Resolves a concrete item, given a path.
/// Guaranteed to result in at most one diagnostic.
pub fn resolve_path(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    scope: &ResolveScope,
    path: &ast::ExprPath,
) -> Option<ResolvedItem> {
    let syntax_db = db.upcast();
    let elements_vec = path.elements(syntax_db);
    let mut elements = elements_vec.iter().peekable();

    let mut item = determine_base_item(db, &mut elements, scope);

    // Follow modules.
    for segment in elements {
        let ident = segment.identifier(syntax_db);
        let generic_args = if let ast::PathSegment::WithGenericArgs(generic_segment) = segment {
            generic_segment
                .generic_args(syntax_db)
                .generic_args(syntax_db)
                .elements(syntax_db)
                .iter()
                .map(|generic_arg_syntax| {
                    let ty = resolve_type(db, diagnostics, scope, generic_arg_syntax);
                    GenericArgumentId::Type(ty)
                })
                .collect()
        } else {
            vec![]
        };
        if let ResolvedItem::Module(module_id) = item {
            let module_item = db
                .module_item_by_name(module_id, ident)
                .on_none(|| diagnostics.report(segment, PathNotFound))?;
            item = match module_item {
                ModuleItemId::Submodule(id) => {
                    check_no_generics(diagnostics, segment);
                    ResolvedItem::Module(ModuleId::Submodule(id))
                }
                ModuleItemId::Use(_) => todo!("Follow uses."),
                ModuleItemId::FreeFunction(id) => ResolvedItem::Function(specialize_function(
                    db,
                    diagnostics,
                    segment.stable_ptr().untyped(),
                    GenericFunctionId::Free(id),
                    generic_args,
                )?),
                ModuleItemId::ExternFunction(id) => ResolvedItem::Function(specialize_function(
                    db,
                    diagnostics,
                    segment.stable_ptr().untyped(),
                    GenericFunctionId::Extern(id),
                    generic_args,
                )?),
                ModuleItemId::Struct(id) => ResolvedItem::Type(specialize_type(
                    db,
                    diagnostics,
                    segment.stable_ptr().untyped(),
                    GenericTypeId::Struct(id),
                    generic_args,
                )?),
                ModuleItemId::Enum(id) => ResolvedItem::Type(specialize_type(
                    db,
                    diagnostics,
                    segment.stable_ptr().untyped(),
                    GenericTypeId::Enum(id),
                    generic_args,
                )?),
                ModuleItemId::ExternType(id) => ResolvedItem::Type(specialize_type(
                    db,
                    diagnostics,
                    segment.stable_ptr().untyped(),
                    GenericTypeId::Extern(id),
                    generic_args,
                )?),
            };
            continue;
        };
        diagnostics.report(segment, InvalidPath);
        return None;
    }
    Some(item)
}

fn check_no_generics(
    diagnostics: &mut SemanticDiagnostics,
    segment: &syntax::node::ast::PathSegment,
) -> Option<()> {
    if let ast::PathSegment::WithGenericArgs(generics) = segment {
        diagnostics.report(generics, InvalidPath);
        None
    } else {
        Some(())
    }
}

/// Determines the base module for the path resolving.
fn determine_base_item(
    db: &dyn SemanticGroup,
    segments: &mut std::iter::Peekable<std::slice::Iter<'_, syntax::node::ast::PathSegment>>,
    scope: &ResolveScope,
) -> ResolvedItem {
    let syntax_db = db.upcast();
    let ident = segments.peek().unwrap().identifier(syntax_db);

    // If a generic param with this name is found, use it.
    if let Some(generic_param_id) = scope.generic_params.get(&ident) {
        segments.next();
        return ResolvedItem::Type(db.intern_type(TypeLongId::GenericParameter(*generic_param_id)));
    }

    // If an item with this name is found inside the current module, use the current module.
    if db.module_item_by_name(scope.module_id, ident.clone()).is_some() {
        return ResolvedItem::Module(scope.module_id);
    }

    // If the first segment is a name of a crate, use the crate's root module as the base module.
    let crate_id = db.intern_crate(CrateLongId(ident));
    // TODO(spapini): Use a better interface to check if the crate exists (not using `dir`).
    if db.crate_root_dir(crate_id).is_some() {
        // Consume this segment.
        segments.next();
        return ResolvedItem::Module(ModuleId::CrateRoot(crate_id));
    }

    // Last resort, use the `core` crate root module as the base module.
    ResolvedItem::Module(core_module(db))
}

/// Specializes a generic function.
pub fn specialize_function(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    stable_ptr: SyntaxStablePtrId,
    generic_function: GenericFunctionId,
    mut generic_args: Vec<GenericArgumentId>,
) -> Option<FunctionId> {
    let _signature = db
        .generic_function_signature(generic_function)
        .on_none(|| diagnostics.report_by_ptr(stable_ptr, UnknownFunction))?;
    let generic_params = db
        .generic_function_generic_params(generic_function)
        .on_none(|| diagnostics.report_by_ptr(stable_ptr, UnknownFunction))?;

    if generic_args.len() != generic_params.len() {
        diagnostics.report_by_ptr(
            stable_ptr,
            WrongNumberOfGenericArguments {
                expected: generic_params.len(),
                actual: generic_args.len(),
            },
        );
        generic_args.resize(generic_params.len(), GenericArgumentId::Type(TypeId::missing(db)));
    }

    Some(db.intern_function(FunctionLongId::Concrete(ConcreteFunction {
        generic_function,
        generic_args,
    })))
}

/// Specializes a generic type.
fn specialize_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    stable_ptr: SyntaxStablePtrId,
    generic_type: GenericTypeId,
    mut generic_args: Vec<GenericArgumentId>,
) -> Option<TypeId> {
    let generic_params = db
        .generic_type_generic_params(generic_type)
        .on_none(|| diagnostics.report_by_ptr(stable_ptr, UnknownType))?;

    if generic_args.len() != generic_params.len() {
        diagnostics.report_by_ptr(
            stable_ptr,
            WrongNumberOfGenericArguments {
                expected: generic_params.len(),
                actual: generic_args.len(),
            },
        );
        generic_args.resize(generic_params.len(), GenericArgumentId::Type(TypeId::missing(db)));
    }

    Some(db.intern_type(TypeLongId::Concrete(ConcreteType { generic_type, generic_args })))
}
