#[cfg(test)]
#[path = "resolve_path_test.rs"]
mod test;

use defs::ids::{
    GenericFunctionId, GenericParamId, GenericTypeId, LanguageElementId, ModuleId, ModuleItemId,
};
use diagnostics_proc_macros::DebugWithDb;
use filesystem::ids::CrateLongId;
use smol_str::SmolStr;
use syntax::node::ast::{self};
use syntax::node::helpers::{GetIdentifier, PathSegmentEx};
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;
use utils::unordered_hash_map::UnorderedHashMap;
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

// TODO(spapini): Reintroduce GenericFunction and GenericType when they are supported.
#[derive(Copy, Clone, PartialEq, Eq, Debug, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum ResolvedItem {
    Module(ModuleId),
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

// Resolves paths semantically.
pub struct Resolver<'db> {
    db: &'db dyn SemanticGroup,
    // Current module in which to resolve the path.
    pub module_id: ModuleId,
    // Generic parameters accessible to the resolver.
    generic_params: UnorderedHashMap<SmolStr, GenericParamId>,
    // Lookback map for resolved identifiers in path. Used in "Go to definition".
    pub resolved_lookback: UnorderedHashMap<ast::TerminalIdentifierPtr, ResolvedItem>,
}
impl<'db> Resolver<'db> {
    pub fn new(
        db: &'db dyn SemanticGroup,
        module_id: ModuleId,
        generic_params: &[GenericParamId],
    ) -> Self {
        Self {
            db,
            module_id,
            generic_params: generic_params
                .iter()
                .map(|generic_param| (generic_param.name(db.upcast()), *generic_param))
                .collect(),
            resolved_lookback: UnorderedHashMap::default(),
        }
    }

    // Relates a path segment to a ResolvedItem, and adds to a lookback map. This will be used in
    // "Go to definition".
    fn mark_for_lookback(
        &mut self,
        segment: &syntax::node::ast::PathSegment,
        resolved_item: ResolvedItem,
    ) -> ResolvedItem {
        let identifier = segment.identifier_ast(self.db.upcast());
        self.resolved_lookback.insert(identifier.stable_ptr(), resolved_item);
        resolved_item
    }

    /// Resolves a concrete item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    pub fn resolve_path(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        path: &ast::ExprPath,
    ) -> Option<ResolvedItem> {
        let syntax_db = self.db.upcast();
        let elements_vec = path.elements(syntax_db);
        let mut elements = elements_vec.iter().peekable();

        let mut item = self.determine_base_item(&mut elements);

        // Follow modules.
        for segment in elements {
            item = self.resolve_next(diagnostics, item, segment)?;
            self.mark_for_lookback(segment, item);
        }
        Some(item)
    }

    /// Given the current resolved item, resolves the next segment.
    fn resolve_next(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        item: ResolvedItem,
        segment: &ast::PathSegment,
    ) -> Option<ResolvedItem> {
        let syntax_db = self.db.upcast();
        let ident = segment.identifier(syntax_db);
        let generic_args = if let ast::PathSegment::WithGenericArgs(generic_segment) = segment {
            generic_segment
                .generic_args(syntax_db)
                .generic_args(syntax_db)
                .elements(syntax_db)
                .iter()
                .map(|generic_arg_syntax| {
                    let ty = resolve_type(self.db, diagnostics, self, generic_arg_syntax);
                    GenericArgumentId::Type(ty)
                })
                .collect()
        } else {
            vec![]
        };
        if let ResolvedItem::Module(module_id) = item {
            let module_item = self
                .db
                .module_item_by_name(module_id, ident)
                .on_none(|| diagnostics.report(segment, PathNotFound))?;
            Some(match module_item {
                ModuleItemId::Submodule(id) => {
                    self.check_no_generics(diagnostics, segment);
                    ResolvedItem::Module(ModuleId::Submodule(id))
                }
                ModuleItemId::Use(id) => {
                    self.check_no_generics(diagnostics, segment);
                    // TODO(spapini): Right now we call priv_use_semantic_data() directly for cycle
                    // handling. Otherise, we need to handle cycle both on it and on the selector
                    // use_resolved_item(). Fix this,
                    self.db.priv_use_semantic_data(id)?.resolved_item?
                }
                ModuleItemId::FreeFunction(id) => ResolvedItem::Function(specialize_function(
                    self.db,
                    diagnostics,
                    segment.stable_ptr().untyped(),
                    GenericFunctionId::Free(id),
                    generic_args,
                )?),
                ModuleItemId::ExternFunction(id) => ResolvedItem::Function(specialize_function(
                    self.db,
                    diagnostics,
                    segment.stable_ptr().untyped(),
                    GenericFunctionId::Extern(id),
                    generic_args,
                )?),
                ModuleItemId::Struct(id) => ResolvedItem::Type(specialize_type(
                    self.db,
                    diagnostics,
                    segment.stable_ptr().untyped(),
                    GenericTypeId::Struct(id),
                    generic_args,
                )?),
                ModuleItemId::Enum(id) => ResolvedItem::Type(specialize_type(
                    self.db,
                    diagnostics,
                    segment.stable_ptr().untyped(),
                    GenericTypeId::Enum(id),
                    generic_args,
                )?),
                ModuleItemId::ExternType(id) => ResolvedItem::Type(specialize_type(
                    self.db,
                    diagnostics,
                    segment.stable_ptr().untyped(),
                    GenericTypeId::Extern(id),
                    generic_args,
                )?),
            })
        } else {
            diagnostics.report(segment, InvalidPath);
            None
        }
    }

    fn check_no_generics(
        &mut self,
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
        &mut self,
        segments: &mut std::iter::Peekable<std::slice::Iter<'_, syntax::node::ast::PathSegment>>,
    ) -> ResolvedItem {
        let syntax_db = self.db.upcast();
        let ident = segments.peek().unwrap().identifier(syntax_db);

        // If a generic param with this name is found, use it.
        if let Some(generic_param_id) = self.generic_params.get(&ident) {
            return self.mark_for_lookback(
                segments.next().unwrap(),
                ResolvedItem::Type(
                    self.db.intern_type(TypeLongId::GenericParameter(*generic_param_id)),
                ),
            );
        }

        // If an item with this name is found inside the current module, use the current module.
        if self.db.module_item_by_name(self.module_id, ident.clone()).is_some() {
            return ResolvedItem::Module(self.module_id);
        }

        // If the first segment is a name of a crate, use the crate's root module as the base
        // module.
        let crate_id = self.db.intern_crate(CrateLongId(ident));
        // TODO(spapini): Use a better interface to check if the crate exists (not using `dir`).
        if self.db.crate_root_dir(crate_id).is_some() {
            return self.mark_for_lookback(
                segments.next().unwrap(),
                ResolvedItem::Module(ModuleId::CrateRoot(crate_id)),
            );
        }

        // Last resort, use the `core` crate root module as the base module.
        ResolvedItem::Module(core_module(self.db))
    }
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

    Some(db.intern_type(TypeLongId::Concrete(ConcreteType::new(generic_type, generic_args))))
}
