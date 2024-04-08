use std::iter::Peekable;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

use cairo_lang_defs::ids::{
    GenericKind, GenericParamId, GenericTypeId, ImplContext, ImplDefId, LanguageElementId,
    ModuleFileId, ModuleId, TraitContext, TraitId, TraitOrImplContext,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::db::Edition;
use cairo_lang_filesystem::ids::{CrateId, CrateLongId};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::helpers::PathSegmentEx;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::try_extract_matches;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
pub use item::{ResolvedConcreteItem, ResolvedGenericItem};
use itertools::Itertools;
use smol_str::SmolStr;
use syntax::node::db::SyntaxGroup;
use syntax::node::TypedStablePtr;

use crate::corelib::{core_submodule, get_submodule};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::compute::{compute_expr_semantic, ComputationContext, Environment};
use crate::expr::inference::conform::InferenceConform;
use crate::expr::inference::infers::InferenceEmbeddings;
use crate::expr::inference::{Inference, InferenceData, InferenceId};
use crate::items::constant::{resolve_const_expr_and_evaluate, ConstValue};
use crate::items::enm::SemanticEnumEx;
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::imp::{ConcreteImplId, ConcreteImplLongId, ImplId, ImplLookupContext};
use crate::items::module::ModuleItemInfo;
use crate::items::trt::{ConcreteTraitGenericFunctionLongId, ConcreteTraitId, ConcreteTraitLongId};
use crate::items::visibility;
use crate::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use crate::types::{are_coupons_enabled, resolve_type};
use crate::{
    ConcreteFunction, ConcreteTypeId, FunctionId, FunctionLongId, GenericArgumentId, GenericParam,
    TypeId, TypeLongId,
};

#[cfg(test)]
mod test;

mod item;

/// Lookback maps for item resolving. Can be used to quickly check what is the semantic resolution
/// of any path segment.
#[derive(Clone, Default, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ResolvedItems {
    pub concrete: UnorderedHashMap<ast::TerminalIdentifierPtr, ResolvedConcreteItem>,
    pub generic: UnorderedHashMap<ast::TerminalIdentifierPtr, ResolvedGenericItem>,
}
impl ResolvedItems {
    // Relates a path segment to a ResolvedConcreteItem, and adds to a resolved_items map. This will
    // be used in "Go to definition".
    pub fn mark_concrete(
        &mut self,
        db: &dyn SemanticGroup,
        segment: &syntax::node::ast::PathSegment,
        resolved_item: ResolvedConcreteItem,
    ) -> ResolvedConcreteItem {
        let identifier = segment.identifier_ast(db.upcast());
        if let Some(generic_item) = resolved_item.generic(db) {
            // Mark the generic item as well, for language server resolved_items.
            self.generic.insert(identifier.stable_ptr(), generic_item);
        }
        self.concrete.insert(identifier.stable_ptr(), resolved_item.clone());
        resolved_item
    }
    // Relates a path segment to a ResolvedGenericItem, and adds to a resolved_items map. This will
    // be used in "Go to definition".
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

#[derive(Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ResolverData {
    /// Current module in which to resolve the path.
    pub module_file_id: ModuleFileId,
    /// Named generic parameters accessible to the resolver.
    generic_param_by_name: OrderedHashMap<SmolStr, GenericParamId>,
    /// All generic parameters accessible to the resolver.
    pub generic_params: Vec<GenericParamId>,
    /// Lookback map for resolved identifiers in path. Used in "Go to definition".
    pub resolved_items: ResolvedItems,
    /// Inference data for the resolver.
    pub inference_data: InferenceData,
    /// The trait/impl context the resolver is currently in. Used to resolve "Self::" paths.
    pub trait_or_impl_ctx: TraitOrImplContext,
}
impl ResolverData {
    pub fn new(module_file_id: ModuleFileId, inference_id: InferenceId) -> Self {
        Self {
            module_file_id,
            generic_param_by_name: Default::default(),
            generic_params: Default::default(),
            resolved_items: Default::default(),
            inference_data: InferenceData::new(inference_id),
            trait_or_impl_ctx: TraitOrImplContext::None,
        }
    }
    pub fn clone_with_inference_id(
        &self,
        db: &dyn SemanticGroup,
        inference_id: InferenceId,
    ) -> Self {
        Self {
            module_file_id: self.module_file_id,
            generic_param_by_name: self.generic_param_by_name.clone(),
            generic_params: self.generic_params.clone(),
            resolved_items: self.resolved_items.clone(),
            inference_data: self.inference_data.clone_with_inference_id(db, inference_id),
            trait_or_impl_ctx: self.trait_or_impl_ctx,
        }
    }
}

/// Resolves paths semantically.
pub struct Resolver<'db> {
    db: &'db dyn SemanticGroup,
    pub data: ResolverData,
    pub edition: Edition,
}
impl Deref for Resolver<'_> {
    type Target = ResolverData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
impl DerefMut for Resolver<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

/// A trait for things that can be interpreted as a path of segments.
pub trait AsSegments {
    fn to_segments(self, db: &dyn SyntaxGroup) -> Vec<ast::PathSegment>;
}
impl AsSegments for &ast::ExprPath {
    fn to_segments(self, db: &dyn SyntaxGroup) -> Vec<ast::PathSegment> {
        self.elements(db)
    }
}
impl AsSegments for Vec<ast::PathSegment> {
    fn to_segments(self, _: &dyn SyntaxGroup) -> Vec<ast::PathSegment> {
        self
    }
}

impl<'db> Resolver<'db> {
    pub fn new(
        db: &'db dyn SemanticGroup,
        module_file_id: ModuleFileId,
        inference_id: InferenceId,
    ) -> Self {
        Self::with_data(db, ResolverData::new(module_file_id, inference_id))
    }

    pub fn with_data(db: &'db dyn SemanticGroup, data: ResolverData) -> Self {
        Self {
            edition: extract_edition(db, data.module_file_id.0.owning_crate(db.upcast())),
            db,
            data,
        }
    }

    pub fn inference(&mut self) -> Inference<'_> {
        self.data.inference_data.inference(self.db)
    }

    /// Adds a generic param to an existing resolver.
    /// This is required since a resolver needs to exist before resolving the generic params,
    /// and thus, they are added to the Resolver only after they are resolved.
    pub fn add_generic_param(&mut self, generic_param_id: GenericParamId) {
        self.generic_params.push(generic_param_id);
        let db = self.db.upcast();
        if let Some(name) = generic_param_id.name(db) {
            self.generic_param_by_name.insert(name, generic_param_id);
        }
    }

    /// Resolves an item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    fn resolve_path_inner<ResolvedItem: Clone>(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        path: impl AsSegments,
        item_type: NotFoundItemType,
        mut callbacks: ResolvePathInnerCallbacks<
            ResolvedItem,
            impl FnMut(
                &mut Resolver<'_>,
                &mut SemanticDiagnostics,
                &mut Peekable<std::slice::Iter<'_, ast::PathSegment>>,
            ) -> Maybe<ResolvedItem>,
            impl FnMut(
                &mut Resolver<'_>,
                &mut SemanticDiagnostics,
                &ResolvedItem,
                &ast::TerminalIdentifier,
                Option<Vec<ast::GenericArg>>,
                NotFoundItemType,
            ) -> Maybe<ResolvedItem>,
            impl FnMut(&mut SemanticDiagnostics, &ast::PathSegment) -> Maybe<()>,
            impl FnMut(
                &mut ResolvedItems,
                &dyn SemanticGroup,
                &syntax::node::ast::PathSegment,
                ResolvedItem,
            ),
        >,
    ) -> Maybe<ResolvedItem> {
        let db = self.db;
        let syntax_db = db.upcast();
        let elements_vec = path.to_segments(syntax_db);
        let mut segments = elements_vec.iter().peekable();

        // Find where the first segment lies in.
        let mut item: ResolvedItem =
            (callbacks.resolve_path_first_segment)(self, diagnostics, &mut segments)?;

        // Follow modules.
        while let Some(segment) = segments.next() {
            (callbacks.validate_segment)(diagnostics, segment)?;
            let identifier = segment.identifier_ast(syntax_db);
            let generic_args = segment.generic_args(syntax_db);

            // If this is not the last segment, set the expected type to
            // [NotFoundItemType::Identifier].
            let cur_item_type =
                if segments.peek().is_some() { NotFoundItemType::Identifier } else { item_type };
            // `?` is ok here as the rest of the segments have no meaning if the current one can't
            // be resolved.
            item = (callbacks.resolve_path_next_segment)(
                self,
                diagnostics,
                &item,
                &identifier,
                generic_args,
                cur_item_type,
            )?;
            (callbacks.mark)(&mut self.resolved_items, db, segment, item.clone());
        }
        Ok(item)
    }

    /// Resolves a concrete item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    pub fn resolve_concrete_path(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        path: impl AsSegments,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedConcreteItem> {
        self.resolve_path_inner::<ResolvedConcreteItem>(
            diagnostics,
            path,
            item_type,
            ResolvePathInnerCallbacks {
                resolved_item_type: PhantomData,
                resolve_path_first_segment: |resolver, diagnostics, segments| {
                    resolver.resolve_concrete_path_first_segment(diagnostics, segments)
                },
                resolve_path_next_segment: |resolver,
                                            diagnostics,
                                            item,
                                            identifier,
                                            generic_args_syntax,
                                            item_type| {
                    resolver.resolve_path_next_segment_concrete(
                        diagnostics,
                        item,
                        identifier,
                        generic_args_syntax,
                        item_type,
                    )
                },
                validate_segment: |_, _| Ok(()),
                mark: |resolved_items, db, segment, item| {
                    resolved_items.mark_concrete(db, segment, item.clone());
                },
            },
        )
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

        let db = self.db;
        let syntax_db = db.upcast();
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

                if let Some(resolved_item) =
                    resolve_self_segment(db, diagnostics, &identifier, self.data.trait_or_impl_ctx)
                {
                    // The first segment is "Self". Consume it and return.
                    segments.next().unwrap();
                    return resolved_item;
                }

                if let Some(local_item) = self.determine_base_item_in_local_scope(&identifier) {
                    self.resolved_items.mark_concrete(db, segments.next().unwrap(), local_item)
                } else if let Some(module_id) = self.determine_base_module(&identifier) {
                    // This item lies inside a module.
                    ResolvedConcreteItem::Module(module_id)
                } else {
                    // This identifier is a crate.
                    self.resolved_items.mark_concrete(
                        db,
                        segments.next().unwrap(),
                        ResolvedConcreteItem::Module(ModuleId::CrateRoot(
                            db.intern_crate(CrateLongId::Real(identifier.text(syntax_db))),
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
        path: impl AsSegments,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedGenericItem> {
        self.resolve_generic_path_inner(diagnostics, path, item_type, false)
    }
    /// Resolves a generic item, given a concrete item path, while ignoring the generic args.
    /// Guaranteed to result in at most one diagnostic.
    pub fn resolve_generic_path_with_args(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        path: impl AsSegments,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedGenericItem> {
        self.resolve_generic_path_inner(diagnostics, path, item_type, true)
    }

    /// Resolves a generic item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    /// If `allow_generic_args` is true a path with generic args will be processed, but the generic
    /// params will be ignored.
    fn resolve_generic_path_inner(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        path: impl AsSegments,
        item_type: NotFoundItemType,
        allow_generic_args: bool,
    ) -> Maybe<ResolvedGenericItem> {
        let validate_segment =
            |diagnostics: &mut SemanticDiagnostics, segment: &ast::PathSegment| match segment {
                ast::PathSegment::WithGenericArgs(generic_args) if !allow_generic_args => {
                    Err(diagnostics.report(generic_args, UnexpectedGenericArgs))
                }
                _ => Ok(()),
            };
        self.resolve_path_inner::<ResolvedGenericItem>(
            diagnostics,
            path,
            item_type,
            ResolvePathInnerCallbacks {
                resolved_item_type: PhantomData,
                resolve_path_first_segment: |resolver, diagnostics, segments| {
                    resolver.resolve_generic_path_first_segment(
                        diagnostics,
                        segments,
                        allow_generic_args,
                    )
                },
                resolve_path_next_segment: |resolver,
                                            diagnostics,
                                            item,
                                            identifier,
                                            _generic_args_syntax,
                                            item_type| {
                    resolver.resolve_path_next_segment_generic(
                        diagnostics,
                        item,
                        identifier,
                        item_type,
                    )
                },
                validate_segment,
                mark: |resolved_items, db, segment, item| {
                    resolved_items.mark_generic(db, segment, item.clone());
                },
            },
        )
    }

    /// Resolves the first segment of a generic path.
    /// If `allow_generic_args` is true the generic args will be ignored.
    fn resolve_generic_path_first_segment(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        segments: &mut Peekable<std::slice::Iter<'_, ast::PathSegment>>,
        allow_generic_args: bool,
    ) -> Maybe<ResolvedGenericItem> {
        if let Some(base_module) = self.try_handle_super_segments(diagnostics, segments) {
            return Ok(ResolvedGenericItem::Module(base_module?));
        }
        let db = self.db;
        let syntax_db = db.upcast();
        Ok(match segments.peek().unwrap() {
            syntax::node::ast::PathSegment::WithGenericArgs(generic_segment) => {
                if !allow_generic_args {
                    return Err(diagnostics
                        .report(&generic_segment.generic_args(syntax_db), UnexpectedGenericArgs));
                }
                let identifier = generic_segment.ident(syntax_db);
                // Identifier with generic args cannot be a local item.
                if let Some(module_id) = self.determine_base_module(&identifier) {
                    ResolvedGenericItem::Module(module_id)
                } else {
                    // Crates do not have generics.
                    return Err(diagnostics
                        .report(&generic_segment.generic_args(syntax_db), UnexpectedGenericArgs));
                }
            }
            syntax::node::ast::PathSegment::Simple(simple_segment) => {
                let identifier = simple_segment.ident(syntax_db);
                if let Some(module_id) = self.determine_base_module(&identifier) {
                    // This item lies inside a module.
                    ResolvedGenericItem::Module(module_id)
                } else {
                    // This identifier is a crate.
                    self.resolved_items.mark_generic(
                        db,
                        segments.next().unwrap(),
                        ResolvedGenericItem::Module(ModuleId::CrateRoot(
                            db.intern_crate(CrateLongId::Real(identifier.text(syntax_db))),
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
    fn resolve_path_next_segment_concrete(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        containing_item: &ResolvedConcreteItem,
        identifier: &ast::TerminalIdentifier,
        generic_args_syntax: Option<Vec<ast::GenericArg>>,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedConcreteItem> {
        let syntax_db = self.db.upcast();
        let ident = identifier.text(syntax_db);

        if identifier.text(syntax_db) == "Self" {
            return Err(diagnostics.report(identifier, SelfMustBeFirst));
        }

        match containing_item {
            ResolvedConcreteItem::Module(module_id) => {
                // Prefix "super" segments should be removed earlier. Middle "super" segments are
                // not allowed.
                if ident == "super" {
                    return Err(diagnostics.report(identifier, InvalidPath));
                }
                let inner_item_info = self
                    .db
                    .module_item_info_by_name(*module_id, ident)?
                    .ok_or_else(|| diagnostics.report(identifier, PathNotFound(item_type)))?;
                self.validate_item_visibility(
                    diagnostics,
                    *module_id,
                    identifier,
                    &inner_item_info,
                );
                let inner_generic_item =
                    ResolvedGenericItem::from_module_item(self.db, inner_item_info.item_id)?;
                Ok(self.specialize_generic_module_item(
                    diagnostics,
                    identifier,
                    inner_generic_item,
                    generic_args_syntax,
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
                // Find the relevant function in the trait.
                let long_trait_id = self.db.lookup_intern_concrete_trait(*concrete_trait_id);
                let trait_id = long_trait_id.trait_id;
                let Some(trait_function_id) = self.db.trait_function_by_name(trait_id, ident)?
                else {
                    return Err(diagnostics.report(identifier, InvalidPath));
                };

                let concrete_trait_function = self.db.intern_concrete_trait_function(
                    ConcreteTraitGenericFunctionLongId::new(
                        self.db,
                        *concrete_trait_id,
                        trait_function_id,
                    ),
                );
                let impl_lookup_context = self.impl_lookup_context();
                let inference = &mut self.data.inference_data.inference(self.db);
                let identifier_stable_ptr = identifier.stable_ptr().untyped();
                let generic_function = inference
                    .infer_trait_generic_function(
                        concrete_trait_function,
                        &impl_lookup_context,
                        Some(identifier_stable_ptr),
                    )
                    .map_err(|err_set| {
                        inference.report_on_pending_error(
                            err_set,
                            diagnostics,
                            identifier.stable_ptr().untyped(),
                        )
                    })?;

                Ok(ResolvedConcreteItem::Function(self.specialize_function(
                    diagnostics,
                    identifier.stable_ptr().untyped(),
                    generic_function,
                    generic_args_syntax.unwrap_or_default(),
                )?))
            }
            ResolvedConcreteItem::Impl(impl_id) => {
                let concrete_trait_id = self.db.impl_concrete_trait(*impl_id)?;
                let trait_id = concrete_trait_id.trait_id(self.db);
                let Some(trait_function_id) = self.db.trait_function_by_name(trait_id, ident)?
                else {
                    return Err(diagnostics.report(identifier, InvalidPath));
                };
                let generic_function_id = GenericFunctionId::Impl(ImplGenericFunctionId {
                    impl_id: *impl_id,
                    function: trait_function_id,
                });

                Ok(ResolvedConcreteItem::Function(self.specialize_function(
                    diagnostics,
                    identifier.stable_ptr().untyped(),
                    generic_function_id,
                    generic_args_syntax.unwrap_or_default(),
                )?))
            }
            ResolvedConcreteItem::Function(function_id) if ident == "Coupon" => {
                if !are_coupons_enabled(self.db, self.module_file_id) {
                    diagnostics.report(identifier, CouponsDisabled);
                }
                if matches!(
                    function_id.get_concrete(self.db).generic_function,
                    GenericFunctionId::Extern(_)
                ) {
                    return Err(diagnostics.report(identifier, CouponForExternFunctionNotAllowed));
                }
                Ok(ResolvedConcreteItem::Type(
                    self.db.intern_type(TypeLongId::Coupon(*function_id)),
                ))
            }
            _ => Err(diagnostics.report(identifier, InvalidPath)),
        }
    }

    /// Specializes a ResolvedGenericItem that came from a ModuleItem.
    fn specialize_generic_module_item(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        identifier: &syntax::node::ast::TerminalIdentifier,
        generic_item: ResolvedGenericItem,
        generic_args_syntax: Option<Vec<ast::GenericArg>>,
    ) -> Maybe<ResolvedConcreteItem> {
        Ok(match generic_item {
            ResolvedGenericItem::Constant(id) => ResolvedConcreteItem::Constant(id),
            ResolvedGenericItem::Module(module_id) => {
                if generic_args_syntax.is_some() {
                    return Err(diagnostics.report(identifier, UnexpectedGenericArgs));
                }
                ResolvedConcreteItem::Module(module_id)
            }
            ResolvedGenericItem::GenericFunction(generic_function) => {
                ResolvedConcreteItem::Function(self.specialize_function(
                    diagnostics,
                    identifier.stable_ptr().untyped(),
                    generic_function,
                    generic_args_syntax.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::GenericType(generic_type) => {
                ResolvedConcreteItem::Type(self.specialize_type(
                    diagnostics,
                    identifier.stable_ptr().untyped(),
                    generic_type,
                    generic_args_syntax.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::GenericTypeAlias(module_type_alias_id) => {
                let ty = self.db.module_type_alias_resolved_type(module_type_alias_id)?;
                let generic_params =
                    self.db.module_type_alias_generic_params(module_type_alias_id)?;
                let generic_args = self.resolve_generic_args(
                    diagnostics,
                    &generic_params,
                    generic_args_syntax.unwrap_or_default(),
                    identifier.stable_ptr().untyped(),
                )?;
                let substitution = GenericSubstitution::new(&generic_params, &generic_args);
                let ty = SubstitutionRewriter { db: self.db, substitution: &substitution }
                    .rewrite(ty)?;
                ResolvedConcreteItem::Type(ty)
            }
            ResolvedGenericItem::GenericImplAlias(impl_alias_id) => {
                let impl_id = self.db.impl_alias_resolved_impl(impl_alias_id)?;
                let generic_params = self.db.impl_alias_generic_params(impl_alias_id)?;
                let generic_args = self.resolve_generic_args(
                    diagnostics,
                    &generic_params,
                    generic_args_syntax.unwrap_or_default(),
                    identifier.stable_ptr().untyped(),
                )?;
                let substitution = GenericSubstitution::new(&generic_params, &generic_args);
                let impl_id = SubstitutionRewriter { db: self.db, substitution: &substitution }
                    .rewrite(impl_id)?;
                ResolvedConcreteItem::Impl(impl_id)
            }
            ResolvedGenericItem::Trait(trait_id) => {
                ResolvedConcreteItem::Trait(self.specialize_trait(
                    diagnostics,
                    identifier.stable_ptr().untyped(),
                    trait_id,
                    generic_args_syntax.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::Impl(impl_def_id) => {
                ResolvedConcreteItem::Impl(ImplId::Concrete(self.specialize_impl(
                    diagnostics,
                    identifier.stable_ptr().untyped(),
                    impl_def_id,
                    generic_args_syntax.unwrap_or_default(),
                )?))
            }
            ResolvedGenericItem::Variant(_) => panic!("Variant is not a module item."),
            ResolvedGenericItem::TraitFunction(_) => panic!("TraitFunction is not a module item."),
            ResolvedGenericItem::Variable(_, _) => panic!("Variable is not a module item."),
        })
    }

    /// Given the current resolved item, resolves the next segment.
    fn resolve_path_next_segment_generic(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        containing_item: &ResolvedGenericItem,
        identifier: &ast::TerminalIdentifier,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedGenericItem> {
        let syntax_db = self.db.upcast();
        let ident = identifier.text(syntax_db);
        match containing_item {
            ResolvedGenericItem::Module(module_id) => {
                let inner_item_info = self
                    .db
                    .module_item_info_by_name(*module_id, ident)?
                    .ok_or_else(|| diagnostics.report(identifier, PathNotFound(item_type)))?;
                self.validate_item_visibility(
                    diagnostics,
                    *module_id,
                    identifier,
                    &inner_item_info,
                );
                ResolvedGenericItem::from_module_item(self.db, inner_item_info.item_id)
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

    /// Determines whether the first identifier of a path is a local item.
    fn determine_base_item_in_local_scope(
        &mut self,
        identifier: &ast::TerminalIdentifier,
    ) -> Option<ResolvedConcreteItem> {
        let syntax_db = self.db.upcast();
        let ident = identifier.text(syntax_db);

        // If a generic param with this name is found, use it.
        if let Some(generic_param_id) = self.data.generic_param_by_name.get(&ident) {
            let item = match generic_param_id.kind(self.db.upcast()) {
                GenericKind::Type => ResolvedConcreteItem::Type(
                    self.db.intern_type(TypeLongId::GenericParameter(*generic_param_id)),
                ),
                GenericKind::Const => {
                    ResolvedConcreteItem::ConstGenericParameter(*generic_param_id)
                }
                GenericKind::Impl => {
                    ResolvedConcreteItem::Impl(ImplId::GenericParameter(*generic_param_id))
                }
                GenericKind::NegImpl => return None,
            };
            return Some(item);
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
        let crate_id = self.db.intern_crate(CrateLongId::Real(ident));
        if self.db.crate_config(crate_id).is_some() {
            return None;
        }
        // Last resort, use the `prelude` module as the base module.
        Some(self.prelude_submodule())
    }

    /// Returns the crate's `prelude` submodule.
    pub fn prelude_submodule(&self) -> ModuleId {
        let prelude_submodule_name = self.edition.prelude_submodule_name();
        let core_prelude_submodule = core_submodule(self.db, "prelude");
        get_submodule(self.db, core_prelude_submodule, prelude_submodule_name).unwrap_or_else(
            || {
                panic!(
                    "expected prelude submodule `{prelude_submodule_name}` not found in \
                     `core::prelude`."
                )
            },
        )
    }

    /// Specializes a trait.
    fn specialize_trait(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        stable_ptr: SyntaxStablePtrId,
        trait_id: TraitId,
        generic_args: Vec<ast::GenericArg>,
    ) -> Maybe<ConcreteTraitId> {
        // TODO(lior): Should we report diagnostic if `trait_generic_params` failed?
        let generic_params = self
            .db
            .trait_generic_params(trait_id)
            .map_err(|_| diagnostics.report_by_ptr(stable_ptr, UnknownTrait))?;

        let generic_args =
            self.resolve_generic_args(diagnostics, &generic_params, generic_args, stable_ptr)?;

        Ok(self.db.intern_concrete_trait(ConcreteTraitLongId { trait_id, generic_args }))
    }

    /// Specializes an impl.
    fn specialize_impl(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        stable_ptr: SyntaxStablePtrId,
        impl_def_id: ImplDefId,
        generic_args: Vec<ast::GenericArg>,
    ) -> Maybe<ConcreteImplId> {
        // TODO(lior): Should we report diagnostic if `impl_def_generic_params` failed?
        let generic_params = self
            .db
            .impl_def_generic_params(impl_def_id)
            .map_err(|_| diagnostics.report_by_ptr(stable_ptr, UnknownImpl))?;

        let generic_args =
            self.resolve_generic_args(diagnostics, &generic_params, generic_args, stable_ptr)?;

        Ok(self.db.intern_concrete_impl(ConcreteImplLongId { impl_def_id, generic_args }))
    }

    /// Specializes a generic function.
    pub fn specialize_function(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        stable_ptr: SyntaxStablePtrId,
        generic_function: GenericFunctionId,
        generic_args: Vec<ast::GenericArg>,
    ) -> Maybe<FunctionId> {
        // TODO(lior): Should we report diagnostic if `impl_def_generic_params` failed?
        let generic_params: Vec<_> = generic_function.generic_params(self.db)?;

        let generic_args =
            self.resolve_generic_args(diagnostics, &generic_params, generic_args, stable_ptr)?;

        Ok(self.db.intern_function(FunctionLongId {
            function: ConcreteFunction { generic_function, generic_args },
        }))
    }

    /// Specializes a generic type.
    pub fn specialize_type(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        stable_ptr: SyntaxStablePtrId,
        generic_type: GenericTypeId,
        generic_args: Vec<ast::GenericArg>,
    ) -> Maybe<TypeId> {
        let generic_params = self
            .db
            .generic_type_generic_params(generic_type)
            .map_err(|_| diagnostics.report_by_ptr(stable_ptr, UnknownType))?;

        let generic_args =
            self.resolve_generic_args(diagnostics, &generic_params, generic_args, stable_ptr)?;

        Ok(self.db.intern_type(TypeLongId::Concrete(ConcreteTypeId::new(
            self.db,
            generic_type,
            generic_args,
        ))))
    }

    pub fn impl_lookup_context(&self) -> ImplLookupContext {
        ImplLookupContext::new(self.module_file_id.0, self.generic_params.clone())
    }

    pub fn resolve_generic_args(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        generic_params: &[GenericParam],
        generic_args_syntax: Vec<ast::GenericArg>,
        stable_ptr: SyntaxStablePtrId,
    ) -> Maybe<Vec<GenericArgumentId>> {
        let syntax_db = self.db.upcast();
        let mut substitution = GenericSubstitution::default();
        let mut resolved_args = vec![];
        let mut arg_syntax_for_param =
            UnorderedHashMap::<GenericParamId, ast::GenericArgValue>::default();
        let mut last_named_arg_index = None;
        let generic_param_by_name = generic_params
            .iter()
            .enumerate()
            .filter_map(|(i, param)| Some((param.id().name(self.db.upcast())?, (i, param.id()))))
            .collect::<UnorderedHashMap<_, _>>();

        for (i, generic_arg_syntax) in generic_args_syntax.iter().enumerate() {
            match generic_arg_syntax {
                ast::GenericArg::Named(arg_syntax) => {
                    let name = arg_syntax.name(syntax_db).text(syntax_db);
                    let Some((index, generic_param_id)) = generic_param_by_name.get(&name) else {
                        return Err(diagnostics.report(arg_syntax, UnknownGenericParam { name }));
                    };
                    if let Some(prev_index) = last_named_arg_index {
                        if prev_index > index {
                            return Err(
                                diagnostics.report(arg_syntax, GenericArgOutOfOrder { name })
                            );
                        }
                    }
                    last_named_arg_index = Some(index);
                    if arg_syntax_for_param
                        .insert(*generic_param_id, arg_syntax.value(syntax_db))
                        .is_some()
                    {
                        return Err(diagnostics.report(arg_syntax, GenericArgDuplicate { name }));
                    }
                }
                ast::GenericArg::Unnamed(arg_syntax) => {
                    if last_named_arg_index.is_some() {
                        return Err(diagnostics.report(arg_syntax, PositionalGenericAfterNamed));
                    }
                    let generic_param = generic_params.get(i).ok_or_else(|| {
                        diagnostics.report(
                            arg_syntax,
                            TooManyGenericArguments {
                                expected: generic_params.len(),
                                actual: generic_args_syntax.len(),
                            },
                        )
                    })?;
                    assert_eq!(
                        arg_syntax_for_param
                            .insert(generic_param.id(), arg_syntax.value(syntax_db)),
                        None,
                        "Unexpected duplication in ordered params."
                    );
                }
            }
        }

        for generic_param in generic_params.iter() {
            let generic_param = SubstitutionRewriter { db: self.db, substitution: &substitution }
                .rewrite(*generic_param)?;
            let generic_arg = self.resolve_generic_arg(
                generic_param,
                arg_syntax_for_param
                    .get(&generic_param.id())
                    .and_then(|arg_syntax| {
                        if let ast::GenericArgValue::Expr(expr) = arg_syntax {
                            Some(expr.expr(syntax_db))
                        } else {
                            None
                        }
                    })
                    .as_ref(),
                stable_ptr,
                diagnostics,
            )?;
            resolved_args.push(generic_arg);
            substitution.0.insert(generic_param.id(), generic_arg);
        }

        Ok(resolved_args)
    }

    fn resolve_generic_arg(
        &mut self,
        generic_param: GenericParam,
        generic_arg_syntax_opt: Option<&ast::Expr>,
        stable_ptr: SyntaxStablePtrId,
        diagnostics: &mut SemanticDiagnostics,
    ) -> Result<GenericArgumentId, cairo_lang_diagnostics::DiagnosticAdded> {
        let Some(generic_arg_syntax) = generic_arg_syntax_opt else {
            let lookup_context = self.impl_lookup_context();
            let inference = &mut self.data.inference_data.inference(self.db);
            return inference
                .infer_generic_arg(&generic_param, lookup_context, Some(stable_ptr))
                .map_err(|err_set| {
                    inference.report_on_pending_error(err_set, diagnostics, stable_ptr)
                });
        };

        Ok(match generic_param {
            GenericParam::Type(_) => {
                let ty = resolve_type(self.db, diagnostics, self, generic_arg_syntax);
                GenericArgumentId::Type(ty)
            }
            GenericParam::Const(const_param) => {
                // TODO(spapini): Currently no bound checks are performed. Move literal validation
                // to inference finalization and use inference here. This will become more relevant
                // when we support constant expressions, which need inference.
                let environment = Environment::empty();
                let mut resolver =
                    Resolver::new(self.db, self.module_file_id, InferenceId::Canonical);

                for param in self.generic_params.iter() {
                    resolver.add_generic_param(*param);
                }

                let mut ctx = ComputationContext::new(
                    self.db,
                    diagnostics,
                    None,
                    resolver,
                    None,
                    environment,
                );
                let value = compute_expr_semantic(&mut ctx, generic_arg_syntax);

                let (_, const_value) = resolve_const_expr_and_evaluate(
                    self.db,
                    &mut ctx,
                    &value,
                    generic_arg_syntax.stable_ptr().untyped(),
                    const_param.ty,
                );

                match const_value {
                    ConstValue::Int(value) => GenericArgumentId::Constant(
                        self.db.intern_const_value(ConstValue::Int(value)),
                    ),
                    ConstValue::Generic(generic_param_id) => GenericArgumentId::Constant(
                        self.db.intern_const_value(ConstValue::Generic(generic_param_id)),
                    ),
                    ConstValue::Missing(err) => return Err(err),
                    _ => unreachable!("Invalid const value."),
                }
            }

            GenericParam::Impl(param) => {
                let expr_path = try_extract_matches!(generic_arg_syntax, ast::Expr::Path)
                    .ok_or_else(|| diagnostics.report(generic_arg_syntax, UnknownImpl))?;
                let resolved_impl = try_extract_matches!(
                    self.resolve_concrete_path(diagnostics, expr_path, NotFoundItemType::Impl,)?,
                    ResolvedConcreteItem::Impl
                )
                .ok_or_else(|| diagnostics.report(generic_arg_syntax, UnknownImpl))?;
                let impl_def_concrete_trait = self.db.impl_concrete_trait(resolved_impl)?;
                let expected_concrete_trait = param.concrete_trait?;
                if let Err(err_set) = self
                    .inference()
                    .conform_traits(impl_def_concrete_trait, expected_concrete_trait)
                {
                    let diag_added = diagnostics.report(
                        generic_arg_syntax,
                        TraitMismatch {
                            expected_trt: expected_concrete_trait,
                            actual_trt: impl_def_concrete_trait,
                        },
                    );
                    self.inference().consume_reported_error(err_set, diag_added);
                }
                GenericArgumentId::Impl(resolved_impl)
            }
            GenericParam::NegImpl(_) => {
                return Err(diagnostics.report(generic_arg_syntax, ArgPassedToNegativeImpl));
            }
        })
    }
    /// Should visibility checks not actually happen for lookups in this module.
    // TODO(orizi): Remove this check when performing a major Cairo update.
    pub fn ignore_visibility_checks(&self, module_id: ModuleId) -> bool {
        let owning_crate = module_id.owning_crate(self.db.upcast());
        extract_edition(self.db, owning_crate).ignore_visibility()
            || self.edition.ignore_visibility() && owning_crate == self.db.core_crate()
    }

    /// Validates that an item is visible from the current module or adds a diagnostic.
    fn validate_item_visibility(
        &self,
        diagnostics: &mut SemanticDiagnostics,
        containing_module_id: ModuleId,
        identifier: &ast::TerminalIdentifier,
        item_info: &ModuleItemInfo,
    ) {
        if self.ignore_visibility_checks(containing_module_id) {
            return;
        }
        let db = self.db.upcast();
        let user_module = self.module_file_id.0;
        if !visibility::peek_visible_in(db, item_info.visibility, containing_module_id, user_module)
        {
            diagnostics.report(identifier, ItemNotVisible { item_id: item_info.item_id });
        }
    }
}

/// Resolves the segment if it's "Self". Returns the Some(ResolvedConcreteItem) or Some(Err) if
/// segment == "Self" or None otherwise.
fn resolve_self_segment(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    identifier: &ast::TerminalIdentifier,
    trait_or_impl_ctx: TraitOrImplContext,
) -> Option<Maybe<ResolvedConcreteItem>> {
    if identifier.text(db.upcast()) != "Self" {
        return None;
    }

    Some(match trait_or_impl_ctx {
        TraitOrImplContext::None => Err(diagnostics.report(identifier, SelfNotSupportedInContext)),
        TraitOrImplContext::Trait(TraitContext { trait_id }) => {
            let concrete_trait_id =
                db.intern_concrete_trait(ConcreteTraitLongId { trait_id, generic_args: vec![] });
            Ok(ResolvedConcreteItem::Trait(concrete_trait_id))
        }
        TraitOrImplContext::Impl(ImplContext { impl_def_id }) => {
            let impl_id =
                ImplId::Concrete(db.intern_concrete_impl(ConcreteImplLongId {
                    impl_def_id,
                    generic_args: vec![],
                }));
            Ok(ResolvedConcreteItem::Impl(impl_id))
        }
    })
}

/// Extracts the edition of a crate.
fn extract_edition(db: &dyn SemanticGroup, crate_id: CrateId) -> Edition {
    db.crate_config(crate_id).map(|config| config.settings.edition).unwrap_or_default()
}

/// The callbacks to be used by `resolve_path_inner`.
struct ResolvePathInnerCallbacks<ResolvedItem, ResolveFirst, ResolveNext, Validate, Mark>
where
    ResolveFirst: FnMut(
        &mut Resolver<'_>,
        &mut SemanticDiagnostics,
        &mut Peekable<std::slice::Iter<'_, ast::PathSegment>>,
    ) -> Maybe<ResolvedItem>,
    ResolveNext: FnMut(
        &mut Resolver<'_>,
        &mut SemanticDiagnostics,
        &ResolvedItem,
        &ast::TerminalIdentifier,
        Option<Vec<ast::GenericArg>>,
        NotFoundItemType,
    ) -> Maybe<ResolvedItem>,
    Validate: FnMut(&mut SemanticDiagnostics, &ast::PathSegment) -> Maybe<()>,
    Mark: FnMut(
        &mut ResolvedItems,
        &dyn SemanticGroup,
        &syntax::node::ast::PathSegment,
        ResolvedItem,
    ),
{
    /// Type for the resolved item pointed by the path segments.
    resolved_item_type: PhantomData<ResolvedItem>,
    /// Resolves the first segment of a path.
    resolve_path_first_segment: ResolveFirst,
    /// Given the current resolved item, resolves the next segment.
    resolve_path_next_segment: ResolveNext,
    /// An additional validation to perform for each segment. If it fails, the whole resolution
    /// fails.
    validate_segment: Validate,
    mark: Mark,
}
