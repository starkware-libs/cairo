#[cfg(test)]
#[path = "resolve_path_test.rs"]
mod test;

use std::iter::Peekable;

use cairo_lang_defs::ids::{
    ConstantId, GenericParamId, GenericTypeId, ImplId, LanguageElementId, ModuleFileId, ModuleId,
    ModuleItemId, TraitFunctionId, TraitId, TypeAliasId,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::ids::CrateLongId;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::helpers::PathSegmentEx;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::Itertools;
use smol_str::SmolStr;

use crate::corelib::core_module;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::inference::Inference;
use crate::items::enm::{ConcreteVariant, SemanticEnumEx};
use crate::items::functions::GenericFunctionId;
use crate::items::imp::{
    find_impls_at_context, ConcreteImplId, ConcreteImplLongId, ImplLookupContext,
};
use crate::items::trt::{
    ConcreteTraitFunctionId, ConcreteTraitFunctionLongId, ConcreteTraitId, ConcreteTraitLongId,
};
use crate::literals::LiteralLongId;
use crate::types::{resolve_type_with_inference, substitute_generics, GenericSubstitution};
use crate::{
    ConcreteFunction, ConcreteTypeId, FunctionId, FunctionLongId, GenericArgumentId, TypeId,
    TypeLongId, Variant,
};

// Resolved items:
// ResolvedConcreteItem - returned by resolve_concrete_path(). Paths with generic arguments.
// ResolvedGenericItem - returned by resolve_generic_path(). Paths without generic arguments.
#[derive(Clone, PartialEq, Eq, Debug, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum ResolvedConcreteItem {
    Constant(ConstantId),
    Module(ModuleId),
    Function(FunctionId),
    TraitFunction(ConcreteTraitFunctionId),
    Type(TypeId),
    Variant(ConcreteVariant),
    Trait(ConcreteTraitId),
    Impl(ConcreteImplId),
}
#[derive(Clone, PartialEq, Eq, Debug, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum ResolvedGenericItem {
    Constant(ConstantId),
    Module(ModuleId),
    GenericFunction(GenericFunctionId),
    TraitFunction(TraitFunctionId),
    GenericType(GenericTypeId),
    GenericTypeAlias(TypeAliasId),
    Variant(Variant),
    Trait(TraitId),
    Impl(ImplId),
}
impl ResolvedConcreteItem {
    pub fn generic(&self, db: &dyn SemanticGroup) -> Option<ResolvedGenericItem> {
        Some(match self {
            ResolvedConcreteItem::Constant(id) => ResolvedGenericItem::Constant(*id),
            ResolvedConcreteItem::Module(item) => ResolvedGenericItem::Module(*item),
            ResolvedConcreteItem::Function(function) => ResolvedGenericItem::GenericFunction(
                db.lookup_intern_function(*function).function.generic_function,
            ),
            ResolvedConcreteItem::TraitFunction(trait_function) => {
                ResolvedGenericItem::TraitFunction(trait_function.function_id(db))
            }
            ResolvedConcreteItem::Type(ty) => {
                if let TypeLongId::Concrete(concrete) = db.lookup_intern_type(*ty) {
                    ResolvedGenericItem::GenericType(concrete.generic_type(db))
                } else {
                    return None;
                }
            }
            ResolvedConcreteItem::Variant(ConcreteVariant { concrete_enum_id, id, ty, idx }) => {
                ResolvedGenericItem::Variant(Variant {
                    enum_id: concrete_enum_id.enum_id(db),
                    id: *id,
                    ty: *ty,
                    idx: *idx,
                })
            }
            ResolvedConcreteItem::Trait(concrete_trait) => ResolvedGenericItem::Trait(
                db.lookup_intern_concrete_trait(*concrete_trait).trait_id,
            ),
            ResolvedConcreteItem::Impl(concrete_impl) => {
                ResolvedGenericItem::Impl(db.lookup_intern_concrete_impl(*concrete_impl).impl_id)
            }
        })
    }
}

/// Lookback maps for item resolving. Can be used to quickly check what is the semantic resolution
/// of any path segment.
#[derive(Clone, Default, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ResolvedLookback {
    pub concrete: UnorderedHashMap<ast::TerminalIdentifierPtr, ResolvedConcreteItem>,
    pub generic: UnorderedHashMap<ast::TerminalIdentifierPtr, ResolvedGenericItem>,
}
impl ResolvedLookback {
    // Relates a path segment to a ResolvedConcreteItem, and adds to a lookback map. This will be
    // used in "Go to definition".
    pub fn mark_concrete(
        &mut self,
        db: &dyn SemanticGroup,
        segment: &syntax::node::ast::PathSegment,
        resolved_item: ResolvedConcreteItem,
    ) -> ResolvedConcreteItem {
        let identifier = segment.identifier_ast(db.upcast());
        if let Some(generic_item) = resolved_item.generic(db) {
            // Mark the generic item as well, for language server lookback.
            self.generic.insert(identifier.stable_ptr(), generic_item);
        }
        self.concrete.insert(identifier.stable_ptr(), resolved_item.clone());
        resolved_item
    }
    // Relates a path segment to a ResolvedGenericItem, and adds to a lookback map. This will be
    // used in "Go to definition".
    pub fn mark_generic(
        &mut self,
        db: &dyn SemanticGroup,
        segment: &syntax::node::ast::PathSegment,
        resolved_item: ResolvedGenericItem,
    ) -> ResolvedGenericItem {
        let identifier = segment.identifier_ast(db.upcast());
        self.generic.insert(identifier.stable_ptr(), resolved_item.clone());
        resolved_item
    }
}

/// Resolves paths semantically.
pub struct Resolver<'db> {
    db: &'db dyn SemanticGroup,
    // Current module in which to resolve the path.
    pub module_file_id: ModuleFileId,
    // Generic parameters accessible to the resolver.
    generic_params: OrderedHashMap<SmolStr, GenericParamId>,
    // Lookback map for resolved identifiers in path. Used in "Go to definition".
    pub lookback: ResolvedLookback,
}
impl<'db> Resolver<'db> {
    pub fn new(
        db: &'db dyn SemanticGroup,
        module_file_id: ModuleFileId,
        generic_params: &[GenericParamId],
    ) -> Self {
        Self {
            db,
            module_file_id,
            generic_params: generic_params
                .iter()
                .map(|generic_param| (generic_param.name(db.upcast()), *generic_param))
                .collect(),
            lookback: ResolvedLookback::default(),
        }
    }

    /// Resolves a concrete item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    pub fn resolve_concrete_path(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        inference: &mut Inference<'_>,
        path: &ast::ExprPath,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedConcreteItem> {
        let syntax_db = self.db.upcast();
        let elements_vec = path.elements(syntax_db);
        let mut segments = elements_vec.iter().peekable();

        // Find where the first segment lies in.
        let mut item = self.resolve_concrete_path_first_segment(diagnostics, &mut segments)?;

        // Follow modules.
        while segments.peek().is_some() {
            let segment = segments.next().unwrap();
            let (identifier, generic_args) = match segment {
                syntax::node::ast::PathSegment::WithGenericArgs(segment) => {
                    let mut generic_args = vec![];
                    for generic_arg_syntax in
                        segment.generic_args(syntax_db).generic_args(syntax_db).elements(syntax_db)
                    {
                        match generic_arg_syntax {
                            ast::Expr::Literal(literal_syntax) => {
                                let literal =
                                    LiteralLongId::try_from(literal_syntax.text(syntax_db))
                                        .map_err(|_| {
                                            diagnostics.report(&literal_syntax, UnknownLiteral)
                                        })?;
                                generic_args.push(GenericArgumentId::Literal(
                                    self.db.intern_literal(literal),
                                ));
                            }
                            ast::Expr::Unary(unary)
                                if matches!(unary.expr(syntax_db), ast::Expr::Literal(_))
                                    && matches!(
                                        unary.op(syntax_db),
                                        ast::UnaryOperator::Minus(_)
                                    ) =>
                            {
                                let mut literal = LiteralLongId::try_from(
                                    extract_matches!(unary.expr(syntax_db), ast::Expr::Literal)
                                        .text(syntax_db),
                                )
                                .map_err(|_| diagnostics.report(&unary, UnknownLiteral))?;
                                literal.value *= -1;
                                generic_args.push(GenericArgumentId::Literal(
                                    self.db.intern_literal(literal),
                                ));
                            }
                            _ => {
                                let ty = resolve_type_with_inference(
                                    self.db,
                                    diagnostics,
                                    inference,
                                    self,
                                    &generic_arg_syntax,
                                );
                                generic_args.push(GenericArgumentId::Type(ty));
                            }
                        }
                    }
                    (segment.ident(syntax_db), Some(generic_args))
                }
                syntax::node::ast::PathSegment::Simple(segment) => (segment.ident(syntax_db), None),
            };

            // If this is not the last segment, set the expected type to
            // [NotFoundItemType::Identifier].
            let cur_item_type =
                if segments.peek().is_some() { NotFoundItemType::Identifier } else { item_type };
            item = self.resolve_next_concrete(
                diagnostics,
                inference,
                &item,
                &identifier,
                generic_args,
                cur_item_type,
            )?;
            self.lookback.mark_concrete(self.db, segment, item.clone());
        }
        Ok(item)
    }

    /// Resolves the first segment of a concrete path.
    fn resolve_concrete_path_first_segment(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        segments: &mut Peekable<std::slice::Iter<'_, ast::PathSegment>>,
    ) -> Maybe<ResolvedConcreteItem> {
        if let Some(base_module) = self.try_handle_super_segments(diagnostics, segments) {
            return Ok(ResolvedConcreteItem::Module(base_module?));
        }
        let syntax_db = self.db.upcast();
        Ok(match segments.peek().unwrap() {
            syntax::node::ast::PathSegment::WithGenericArgs(generic_segment) => {
                let identifier = generic_segment.ident(syntax_db);
                // Identifier with generic args cannot be a local item.
                if let Some(module_id) = self.determine_base_module(&identifier) {
                    ResolvedConcreteItem::Module(module_id)
                } else {
                    // Crates do not have generics.
                    return Err(diagnostics
                        .report(&generic_segment.generic_args(syntax_db), UnexpectedGenericArgs));
                }
            }
            syntax::node::ast::PathSegment::Simple(simple_segment) => {
                let identifier = simple_segment.ident(syntax_db);
                if let Some(local_item) = self.determine_base_item_in_local_scope(&identifier) {
                    self.lookback.mark_concrete(self.db, segments.next().unwrap(), local_item)
                } else if let Some(module_id) = self.determine_base_module(&identifier) {
                    // This item lies inside a module.
                    ResolvedConcreteItem::Module(module_id)
                } else {
                    // This identifier is a crate.
                    self.lookback.mark_concrete(
                        self.db,
                        segments.next().unwrap(),
                        ResolvedConcreteItem::Module(ModuleId::CrateRoot(
                            self.db.intern_crate(CrateLongId(identifier.text(syntax_db))),
                        )),
                    )
                }
            }
        })
    }

    /// Resolves a generic item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    pub fn resolve_generic_path(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        path: &ast::ExprPath,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedGenericItem> {
        let syntax_db = self.db.upcast();
        let elements_vec = path.elements(syntax_db);
        let mut segments = elements_vec.iter().peekable();

        // Find where the first segment lies in.
        let mut item = self.resolve_generic_path_first_segment(diagnostics, &mut segments)?;

        // Follow modules.
        while segments.peek().is_some() {
            let segment = segments.next().unwrap();
            let identifier = match segment {
                syntax::node::ast::PathSegment::WithGenericArgs(segment) => {
                    return Err(
                        diagnostics.report(&segment.generic_args(syntax_db), UnexpectedGenericArgs)
                    );
                }
                syntax::node::ast::PathSegment::Simple(segment) => segment.ident(syntax_db),
            };

            // If this is not the last segment, set the expected type to
            // [NotFoundItemType::Identifier].
            let cur_item_type =
                if segments.peek().is_some() { NotFoundItemType::Identifier } else { item_type };
            item = self.resolve_next_generic(diagnostics, &item, &identifier, cur_item_type)?;
            self.lookback.mark_generic(self.db, segment, item.clone());
        }
        Ok(item)
    }

    /// Resolves the first segment of a generic path.
    fn resolve_generic_path_first_segment(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        segments: &mut Peekable<std::slice::Iter<'_, ast::PathSegment>>,
    ) -> Maybe<ResolvedGenericItem> {
        if let Some(base_module) = self.try_handle_super_segments(diagnostics, segments) {
            return Ok(ResolvedGenericItem::Module(base_module?));
        }
        let syntax_db = self.db.upcast();
        Ok(match segments.peek().unwrap() {
            syntax::node::ast::PathSegment::WithGenericArgs(generic_segment) => {
                return Err(diagnostics
                    .report(&generic_segment.generic_args(syntax_db), UnexpectedGenericArgs));
            }
            syntax::node::ast::PathSegment::Simple(simple_segment) => {
                let identifier = simple_segment.ident(syntax_db);
                if let Some(module_id) = self.determine_base_module(&identifier) {
                    // This item lies inside a module.
                    ResolvedGenericItem::Module(module_id)
                } else {
                    // This identifier is a crate.
                    self.lookback.mark_generic(
                        self.db,
                        segments.next().unwrap(),
                        ResolvedGenericItem::Module(ModuleId::CrateRoot(
                            self.db.intern_crate(CrateLongId(identifier.text(syntax_db))),
                        )),
                    )
                }
            }
        })
    }

    /// Handles `super::` initial segments, by removing them, and returning the valid module if
    /// exists. If there's none - returns None.
    /// If there are, but that's an invalid path, adds to diagnostics and returns `Some(Err)`.
    fn try_handle_super_segments(
        &self,
        diagnostics: &mut SemanticDiagnostics,
        segments: &mut Peekable<std::slice::Iter<'_, ast::PathSegment>>,
    ) -> Option<Maybe<ModuleId>> {
        let syntax_db = self.db.upcast();
        let mut module_id = self.module_file_id.0;
        for segment in segments.peeking_take_while(|segment| match segment {
            ast::PathSegment::WithGenericArgs(_) => false,
            ast::PathSegment::Simple(simple) => simple.ident(syntax_db).text(syntax_db) == "super",
        }) {
            module_id = match module_id {
                ModuleId::CrateRoot(_) => {
                    return Some(Err(diagnostics.report(segment, SuperUsedInRootModule)));
                }
                ModuleId::Submodule(submodule_id) => submodule_id.parent_module(self.db.upcast()),
            };
        }
        if module_id == self.module_file_id.0 { None } else { Some(Ok(module_id)) }
    }

    /// Given the current resolved item, resolves the next segment.
    fn resolve_next_concrete(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        inference: &mut Inference<'_>,
        item: &ResolvedConcreteItem,
        identifier: &ast::TerminalIdentifier,
        generic_args: Option<Vec<GenericArgumentId>>,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedConcreteItem> {
        let syntax_db = self.db.upcast();
        let ident = identifier.text(syntax_db);
        match item {
            ResolvedConcreteItem::Module(module_id) => {
                if ident == "super" {
                    return Err(diagnostics.report(identifier, InvalidPath));
                }
                let module_item = self
                    .db
                    .module_item_by_name(*module_id, ident)?
                    .ok_or_else(|| diagnostics.report(identifier, PathNotFound(item_type)))?;
                let generic_item = self.module_item_to_generic_item(diagnostics, module_item)?;
                Ok(self.specialize_generic_module_item(
                    diagnostics,
                    inference,
                    identifier,
                    generic_item,
                    generic_args,
                )?)
            }
            ResolvedConcreteItem::Type(ty) => {
                if let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) =
                    self.db.lookup_intern_type(*ty)
                {
                    let enum_id = concrete_enum_id.enum_id(self.db);
                    let variants = self
                        .db
                        .enum_variants(enum_id)
                        .map_err(|_| diagnostics.report(identifier, UnknownEnum))?;
                    let variant_id = variants.get(&ident).ok_or_else(|| {
                        diagnostics
                            .report(identifier, NoSuchVariant { enum_id, variant_name: ident })
                    })?;
                    let variant = self.db.variant_semantic(enum_id, *variant_id)?;
                    let concrete_variant =
                        self.db.concrete_enum_variant(concrete_enum_id, &variant)?;
                    Ok(ResolvedConcreteItem::Variant(concrete_variant))
                } else {
                    Err(diagnostics.report(identifier, InvalidPath))
                }
            }
            ResolvedConcreteItem::Trait(concrete_trait_id) => {
                // TODO(spapini): Currently, generics of the trait function itself (not the trait)
                // are not handle. Use `generic_args` to handle them.
                // Find the relevant function in the trait.
                let long_trait_id = self.db.lookup_intern_concrete_trait(*concrete_trait_id);
                let trait_id = long_trait_id.trait_id;
                let Some(trait_function_id) = self.db.trait_function_by_name(trait_id, ident)? else {
                    return Err(diagnostics.report(identifier, InvalidPath));
                };

                let trait_function =
                    self.db.intern_concrete_trait_function(ConcreteTraitFunctionLongId::new(
                        self.db,
                        *concrete_trait_id,
                        trait_function_id,
                    ));
                Ok(ResolvedConcreteItem::TraitFunction(trait_function))
            }
            _ => Err(diagnostics.report(identifier, InvalidPath)),
        }
    }

    /// Given a concrete trait (with possible type variables in generic args), resolves it to its
    /// implementation.
    pub fn resolve_trait(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        inference: &mut Inference<'_>,
        concrete_trait_function_id: ConcreteTraitFunctionId,
        stable_ptr: SyntaxStablePtrId,
    ) -> Maybe<ConcreteImplId> {
        let defs_db = self.db.upcast();

        let concrete_trait_id = concrete_trait_function_id.concrete_trait_id(self.db);
        let function_id = concrete_trait_function_id.function_id(self.db);

        // Find the relevant impl of the trait.
        let trait_id = function_id.trait_id(defs_db);
        let lookup_context = self.impl_lookup_context(trait_id);

        // TODO(yuval): Support trait function default implementations.
        let impl_id =
            match &find_impls_at_context(self.db, inference, &lookup_context, concrete_trait_id)?
                .into_iter()
                .collect_vec()[..]
            {
                &[] => {
                    return Err(diagnostics.report_by_ptr(
                        stable_ptr,
                        NoImplementationOfTraitFunction {
                            trait_id,
                            function_name: function_id.name(defs_db),
                        },
                    ));
                }
                &[impl_id] => impl_id,
                impls => {
                    return Err(diagnostics.report_by_ptr(
                        stable_ptr,
                        MultipleImplementationOfTraitFunction {
                            trait_id,
                            all_impl_ids: impls.to_vec(),
                            function_name: function_id.name(defs_db),
                        },
                    ));
                }
            };

        // Infer the impl.
        let long_concrete_trait = self.db.lookup_intern_concrete_trait(concrete_trait_id);
        let impl_generic_params = self.db.impl_generic_params(impl_id)?;
        let impl_concrete_trait = self.db.impl_trait(impl_id)?;
        let long_imp_concrete_trait = self.db.lookup_intern_concrete_trait(impl_concrete_trait);
        let generic_args = inference
            .infer_generics(
                &impl_generic_params,
                &long_imp_concrete_trait.generic_args,
                &long_concrete_trait.generic_args,
            )
            .expect("Impl returned from find_impls_at_context() must be conformable.");
        Ok(self.db.intern_concrete_impl(ConcreteImplLongId { impl_id, generic_args }))
    }

    /// Retrieve an impl lookup context for finding impls for a trait in the current context.
    pub fn impl_lookup_context(&mut self, trait_id: TraitId) -> ImplLookupContext {
        let lookup_context = ImplLookupContext {
            module_id: self.module_file_id.0,
            extra_modules: vec![trait_id.module_file_id(self.db.upcast()).0],
            generic_params: self.generic_params.values().copied().collect(),
        };
        lookup_context
    }

    /// Specializes a ResolvedGenericItem that came from a ModuleItem.
    fn specialize_generic_module_item(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        inference: &mut Inference<'_>,
        identifier: &syntax::node::ast::TerminalIdentifier,
        generic_item: ResolvedGenericItem,
        generic_args: Option<Vec<GenericArgumentId>>,
    ) -> Maybe<ResolvedConcreteItem> {
        Ok(match generic_item {
            ResolvedGenericItem::Constant(id) => ResolvedConcreteItem::Constant(id),
            ResolvedGenericItem::Module(module_id) => {
                if generic_args.is_some() {
                    return Err(diagnostics.report(identifier, UnexpectedGenericArgs));
                }
                ResolvedConcreteItem::Module(module_id)
            }
            ResolvedGenericItem::GenericFunction(generic_function) => {
                ResolvedConcreteItem::Function(specialize_function(
                    self.db,
                    diagnostics,
                    inference,
                    identifier.stable_ptr().untyped(),
                    generic_function,
                    generic_args.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::GenericType(generic_type) => {
                ResolvedConcreteItem::Type(specialize_type(
                    self.db,
                    diagnostics,
                    inference,
                    identifier.stable_ptr().untyped(),
                    generic_type,
                    generic_args.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::GenericTypeAlias(type_alias_id) => {
                // Check for cycles in this type alias definition.
                // TODO(orizi): Handle this without using `priv_type_alias_semantic_data`.
                self.db.priv_type_alias_semantic_data(type_alias_id)?.check_no_cycle()?;

                let ty = self.db.type_alias_resolved_type(type_alias_id)?;
                let mut generic_args = generic_args.unwrap_or_default();
                let generic_params = self.db.type_alias_generic_params(type_alias_id)?;
                conform_generic_args(
                    self.db,
                    diagnostics,
                    inference,
                    &generic_params,
                    &mut generic_args,
                    identifier.stable_ptr().untyped(),
                );
                let substitution = GenericSubstitution(
                    generic_params.into_iter().zip(generic_args.into_iter()).collect(),
                );

                ResolvedConcreteItem::Type(substitute_generics(self.db, &substitution, ty))
            }
            ResolvedGenericItem::Trait(trait_id) => ResolvedConcreteItem::Trait(specialize_trait(
                self.db,
                diagnostics,
                inference,
                identifier.stable_ptr().untyped(),
                trait_id,
                generic_args.unwrap_or_default(),
            )?),
            ResolvedGenericItem::Impl(impl_id) => ResolvedConcreteItem::Impl(specialize_impl(
                self.db,
                diagnostics,
                inference,
                identifier.stable_ptr().untyped(),
                impl_id,
                generic_args.unwrap_or_default(),
            )?),
            ResolvedGenericItem::Variant(_) => panic!("Variant is not a module item."),
            ResolvedGenericItem::TraitFunction(_) => panic!("TraitFunction is not a module item."),
        })
    }

    /// Given the current resolved item, resolves the next segment.
    fn resolve_next_generic(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        item: &ResolvedGenericItem,
        identifier: &ast::TerminalIdentifier,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedGenericItem> {
        let syntax_db = self.db.upcast();
        let ident = identifier.text(syntax_db);
        match item {
            ResolvedGenericItem::Module(module_id) => {
                let module_item = self
                    .db
                    .module_item_by_name(*module_id, ident)?
                    .ok_or_else(|| diagnostics.report(identifier, PathNotFound(item_type)))?;
                self.module_item_to_generic_item(diagnostics, module_item)
            }
            ResolvedGenericItem::GenericType(GenericTypeId::Enum(enum_id)) => {
                let variants = self.db.enum_variants(*enum_id)?;
                let variant_id = variants.get(&ident).ok_or_else(|| {
                    diagnostics.report(
                        identifier,
                        NoSuchVariant { enum_id: *enum_id, variant_name: ident },
                    )
                })?;
                let variant = self.db.variant_semantic(*enum_id, *variant_id)?;
                Ok(ResolvedGenericItem::Variant(variant))
            }
            _ => Err(diagnostics.report(identifier, InvalidPath)),
        }
    }

    /// Wraps a ModuleItem with the corresponding ResolveGenericItem.
    fn module_item_to_generic_item(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        module_item: ModuleItemId,
    ) -> Maybe<ResolvedGenericItem> {
        Ok(match module_item {
            ModuleItemId::Constant(id) => ResolvedGenericItem::Constant(id),
            ModuleItemId::Submodule(id) => ResolvedGenericItem::Module(ModuleId::Submodule(id)),
            ModuleItemId::Use(id) => {
                // TODO(spapini): Before the last change, we called priv_use_semantic_data()
                // directly for cycle handling. Do we need to handle cycle both on
                // it and on the selector use_resolved_item() now?
                diagnostics.diagnostics.extend(self.db.use_semantic_diagnostics(id));
                self.db.use_resolved_item(id)?
            }
            ModuleItemId::FreeFunction(id) => {
                ResolvedGenericItem::GenericFunction(GenericFunctionId::Free(id))
            }
            ModuleItemId::ExternFunction(id) => {
                ResolvedGenericItem::GenericFunction(GenericFunctionId::Extern(id))
            }
            ModuleItemId::Struct(id) => ResolvedGenericItem::GenericType(GenericTypeId::Struct(id)),
            ModuleItemId::Enum(id) => ResolvedGenericItem::GenericType(GenericTypeId::Enum(id)),
            ModuleItemId::TypeAlias(id) => ResolvedGenericItem::GenericTypeAlias(id),
            ModuleItemId::ExternType(id) => {
                ResolvedGenericItem::GenericType(GenericTypeId::Extern(id))
            }
            ModuleItemId::Trait(id) => ResolvedGenericItem::Trait(id),
            ModuleItemId::Impl(id) => ResolvedGenericItem::Impl(id),
        })
    }

    /// Determines whether the first identifier of a path is a local item.
    fn determine_base_item_in_local_scope(
        &mut self,
        identifier: &ast::TerminalIdentifier,
    ) -> Option<ResolvedConcreteItem> {
        let syntax_db = self.db.upcast();
        let ident = identifier.text(syntax_db);

        // If a generic param with this name is found, use it.
        if let Some(generic_param_id) = self.generic_params.get(&ident) {
            return Some(ResolvedConcreteItem::Type(
                self.db.intern_type(TypeLongId::GenericParameter(*generic_param_id)),
            ));
        }

        // TODO(spapini): Resolve local variables.

        None
    }

    /// Determines the base module for the path resolving. Looks only in non-local scope (i.e.
    /// current module, or crates).
    /// Returns Some(module) if the identifier is an item in a module. Otherwise, the path is fully
    /// qualified, which means the identifier is a crate. In this case, returns None.
    fn determine_base_module(&mut self, identifier: &ast::TerminalIdentifier) -> Option<ModuleId> {
        let syntax_db = self.db.upcast();
        let ident = identifier.text(syntax_db);

        // If an item with this name is found inside the current module, use the current module.
        if let Ok(Some(_)) = self.db.module_item_by_name(self.module_file_id.0, ident.clone()) {
            return Some(self.module_file_id.0);
        }

        // If the first segment is a name of a crate, use the crate's root module as the base
        // module.
        let crate_id = self.db.intern_crate(CrateLongId(ident));
        // TODO(spapini): Use a better interface to check if the crate exists (not using `dir`).
        if self.db.crate_root_dir(crate_id).is_some() {
            return None;
        }

        // Last resort, use the `core` crate root module as the base module.
        Some(core_module(self.db))
    }
}

/// Specializes a trait.
fn specialize_trait(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    inference: &mut Inference<'_>,
    stable_ptr: SyntaxStablePtrId,
    trait_id: TraitId,
    mut generic_args: Vec<GenericArgumentId>,
) -> Maybe<ConcreteTraitId> {
    // TODO(lior): Should we report diagnostic if `trait_generic_params` failed?
    let generic_params = db
        .trait_generic_params(trait_id)
        .map_err(|_| diagnostics.report_by_ptr(stable_ptr, UnknownTrait))?;

    conform_generic_args(
        db,
        diagnostics,
        inference,
        &generic_params,
        &mut generic_args,
        stable_ptr,
    );

    Ok(db.intern_concrete_trait(ConcreteTraitLongId { trait_id, generic_args }))
}

/// Specializes an impl.
fn specialize_impl(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    inference: &mut Inference<'_>,
    stable_ptr: SyntaxStablePtrId,
    impl_id: ImplId,
    mut generic_args: Vec<GenericArgumentId>,
) -> Maybe<ConcreteImplId> {
    // Check for cycles in this type alias definition.
    // TODO(orizi): Handle this without using `priv_impl_declaration_data`.
    db.priv_impl_declaration_data(impl_id)?.check_no_cycle()?;

    // TODO(lior): Should we report diagnostic if `impl_generic_params` failed?
    let generic_params = db
        .impl_generic_params(impl_id)
        .map_err(|_| diagnostics.report_by_ptr(stable_ptr, UnknownImpl))?;

    conform_generic_args(
        db,
        diagnostics,
        inference,
        &generic_params,
        &mut generic_args,
        stable_ptr,
    );

    Ok(db.intern_concrete_impl(ConcreteImplLongId { impl_id, generic_args }))
}

/// Specializes a generic function.
pub fn specialize_function(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    inference: &mut Inference<'_>,
    stable_ptr: SyntaxStablePtrId,
    generic_function: GenericFunctionId,
    mut generic_args: Vec<GenericArgumentId>,
) -> Maybe<FunctionId> {
    // TODO(lior): Should we report diagnostic if `impl_generic_params` failed?
    let generic_params: Vec<_> = db
        .function_signature_generic_params(generic_function.signature(db))
        .map_err(|_| diagnostics.report_by_ptr(stable_ptr, UnknownFunction))?;

    conform_generic_args(
        db,
        diagnostics,
        inference,
        &generic_params,
        &mut generic_args,
        stable_ptr,
    );

    Ok(db.intern_function(FunctionLongId {
        function: ConcreteFunction { generic_function, generic_args },
    }))
}

/// Specializes a generic type.
pub fn specialize_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    inference: &mut Inference<'_>,
    stable_ptr: SyntaxStablePtrId,
    generic_type: GenericTypeId,
    mut generic_args: Vec<GenericArgumentId>,
) -> Maybe<TypeId> {
    let generic_params = db
        .generic_type_generic_params(generic_type)
        .map_err(|_| diagnostics.report_by_ptr(stable_ptr, UnknownType))?;

    conform_generic_args(
        db,
        diagnostics,
        inference,
        &generic_params,
        &mut generic_args,
        stable_ptr,
    );

    Ok(db.intern_type(TypeLongId::Concrete(ConcreteTypeId::new(db, generic_type, generic_args))))
}

fn conform_generic_args(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    inference: &mut Inference<'_>,
    generic_params: &[GenericParamId],
    generic_args: &mut Vec<GenericArgumentId>,
    stable_ptr: SyntaxStablePtrId,
) {
    let mut diag_added = None;
    let err = WrongNumberOfGenericArguments {
        expected: generic_params.len(),
        actual: generic_args.len(),
    };
    let mut report =
        || *diag_added.get_or_insert_with(|| diagnostics.report_by_ptr(stable_ptr, err.clone()));
    if generic_args.len() > generic_params.len() {
        generic_args
            .resize(generic_params.len(), GenericArgumentId::Type(TypeId::missing(db, report())));
    }
    for (i, _) in generic_params.iter().enumerate() {
        // TODO(spapini): Handle `_` passed as generic argument.
        if i >= generic_args.len() {
            if inference.enabled {
                // Infer.
                generic_args.push(GenericArgumentId::Type(inference.new_var(stable_ptr)))
            } else {
                generic_args.push(GenericArgumentId::Type(TypeId::missing(db, report())))
            }
        }
    }
}
