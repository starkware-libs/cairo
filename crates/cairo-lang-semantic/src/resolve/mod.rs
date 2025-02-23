use std::iter::Peekable;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use cairo_lang_defs::ids::{
    GenericKind, GenericParamId, GenericTypeId, ImplDefId, LanguageElementId, LookupItemId,
    ModuleFileId, ModuleId, ModuleItemId, TraitId, TraitItemId, VariantId,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::db::{CORELIB_CRATE_NAME, CrateSettings};
use cairo_lang_filesystem::ids::{CrateId, CrateLongId};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::ast::TerminalIdentifier;
use cairo_lang_syntax::node::helpers::PathSegmentEx;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern, extract_matches, require, try_extract_matches};
pub use item::{ResolvedConcreteItem, ResolvedGenericItem};
use itertools::Itertools;
use smol_str::SmolStr;
use syntax::node::TypedStablePtr;
use syntax::node::db::SyntaxGroup;
use syntax::node::helpers::QueryAttrs;

use crate::corelib::{core_submodule, get_submodule};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::{
    ComputationContext, ContextFunction, Environment, compute_expr_semantic,
    get_statement_item_by_name,
};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::conform::InferenceConform;
use crate::expr::inference::infers::InferenceEmbeddings;
use crate::expr::inference::{Inference, InferenceData, InferenceId};
use crate::items::constant::{ConstValue, ImplConstantId, resolve_const_expr_and_evaluate};
use crate::items::enm::SemanticEnumEx;
use crate::items::feature_kind::{
    FeatureConfig, FeatureKind, HasFeatureKind, extract_feature_config,
};
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::generics::generic_params_to_args;
use crate::items::imp::{
    ConcreteImplId, ConcreteImplLongId, DerefInfo, ImplImplId, ImplLongId, ImplLookupContext,
};
use crate::items::module::ModuleItemInfo;
use crate::items::trt::{
    ConcreteTraitConstantLongId, ConcreteTraitGenericFunctionLongId, ConcreteTraitId,
    ConcreteTraitImplLongId, ConcreteTraitLongId, ConcreteTraitTypeId,
};
use crate::items::{TraitOrImplContext, visibility};
use crate::substitution::{GenericSubstitution, SemanticRewriter};
use crate::types::{ConcreteEnumLongId, ImplTypeId, are_coupons_enabled, resolve_type};
use crate::{
    ConcreteFunction, ConcreteTypeId, ConcreteVariant, FunctionId, FunctionLongId,
    GenericArgumentId, GenericParam, Member, Mutability, TypeId, TypeLongId,
};

#[cfg(test)]
mod test;

mod item;

// Remove when these are added as actual keywords.
pub const SELF_TYPE_KW: &str = "Self";
pub const SUPER_KW: &str = "super";
pub const CRATE_KW: &str = "crate";
// Remove when this becomes an actual crate.
const STARKNET_CRATE_NAME: &str = "starknet";

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

/// The enriched members of a type, including direct members of structs, as well as members of
/// targets of `Deref` and `DerefMut` of the type.
#[derive(Debug, PartialEq, Eq, DebugWithDb, Clone)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct EnrichedMembers {
    /// A map from member names to their semantic representation and the number of deref operations
    /// needed to access them.
    pub members: OrderedHashMap<SmolStr, (Member, usize)>,
    /// The sequence of deref needed to access the members.
    pub deref_chain: Arc<[DerefInfo]>,
    // The number of derefs that were explored.
    pub explored_derefs: usize,
}
impl EnrichedMembers {
    /// Returns `EnrichedTypeMemberAccess` for a single member if exists.
    pub fn get_member(&self, name: &str) -> Option<EnrichedTypeMemberAccess> {
        let (member, n_derefs) = self.members.get(name)?;
        Some(EnrichedTypeMemberAccess {
            member: member.clone(),
            deref_functions: self
                .deref_chain
                .iter()
                .map(|deref_info| (deref_info.function_id, deref_info.self_mutability))
                .take(*n_derefs)
                .collect(),
        })
    }
}

/// The enriched member of a type, including the member itself and the deref functions needed to
/// access it.
pub struct EnrichedTypeMemberAccess {
    /// The member itself.
    pub member: Member,
    /// The sequence of deref functions needed to access the member.
    pub deref_functions: Vec<(FunctionId, Mutability)>,
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
    /// The enriched members per type and its mutability in the resolver context.
    pub type_enriched_members: OrderedHashMap<(TypeId, bool), EnrichedMembers>,
    /// Lookback map for resolved identifiers in path. Used in "Go to definition".
    pub resolved_items: ResolvedItems,
    /// Inference data for the resolver.
    pub inference_data: InferenceData,
    /// The trait/impl context the resolver is currently in. Used to resolve "Self::" paths.
    pub trait_or_impl_ctx: TraitOrImplContext,
    /// The configuration of allowed features.
    pub feature_config: FeatureConfig,
    /// The set of used items in the current context.
    pub used_items: OrderedHashSet<LookupItemId>,
}
impl ResolverData {
    pub fn new(module_file_id: ModuleFileId, inference_id: InferenceId) -> Self {
        Self {
            module_file_id,
            generic_param_by_name: Default::default(),
            generic_params: Default::default(),
            type_enriched_members: Default::default(),
            resolved_items: Default::default(),
            inference_data: InferenceData::new(inference_id),
            trait_or_impl_ctx: TraitOrImplContext::None,
            feature_config: Default::default(),
            used_items: Default::default(),
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
            type_enriched_members: self.type_enriched_members.clone(),
            resolved_items: self.resolved_items.clone(),
            inference_data: self.inference_data.clone_with_inference_id(db, inference_id),
            trait_or_impl_ctx: self.trait_or_impl_ctx,
            feature_config: self.feature_config.clone(),
            used_items: self.used_items.clone(),
        }
    }
}

/// Resolves paths semantically.
pub struct Resolver<'db> {
    db: &'db dyn SemanticGroup,
    pub data: ResolverData,
    pub owning_crate_id: CrateId,
    pub settings: CrateSettings,
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
impl Resolver<'_> {
    /// Extracts the allowed node from the syntax, and sets it as the allowed features of the
    /// resolver.
    pub fn set_feature_config(
        &mut self,
        element_id: &impl LanguageElementId,
        syntax: &impl QueryAttrs,
        diagnostics: &mut SemanticDiagnostics,
    ) {
        self.feature_config =
            extract_feature_config(self.db.upcast(), element_id, syntax, diagnostics);
    }
}

/// The result of resolveing an item using `use *` imports.
enum UseStarResult {
    /// A unique path was found, considering only the `use *` imports.
    UniquePathFound(ModuleItemInfo),
    /// The path is ambiguous, considering only the `use *` imports.
    AmbiguousPath(Vec<ModuleItemId>),
    /// The path was not found, considering only the `use *` imports.
    PathNotFound,
    /// Item is not visible in the current module, considering only the `use *` imports.
    ItemNotVisible(ModuleItemId, Vec<ModuleId>),
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
        let owning_crate_id = data.module_file_id.0.owning_crate(db.upcast());
        let settings = db.crate_config(owning_crate_id).map(|c| c.settings).unwrap_or_default();
        Self { owning_crate_id, settings, db, data }
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
        statement_env: Option<&mut Environment>,
        mut callbacks: ResolvePathInnerCallbacks<
            ResolvedItem,
            impl FnMut(
                &mut Resolver<'_>,
                &mut SemanticDiagnostics,
                &mut Peekable<std::slice::Iter<'_, ast::PathSegment>>,
                Option<&mut Environment>,
            ) -> Maybe<ResolvedItem>,
            impl FnMut(
                &mut Resolver<'_>,
                &mut SemanticDiagnostics,
                &ResolvedItem,
                &ast::PathSegment,
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
        let mut item: ResolvedItem = (callbacks.resolve_path_first_segment)(
            self,
            diagnostics,
            &mut segments,
            statement_env,
        )?;

        // Follow modules.
        while let Some(segment) = segments.next() {
            (callbacks.validate_segment)(diagnostics, segment)?;

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
                segment,
                cur_item_type,
            )?;
            (callbacks.mark)(&mut self.resolved_items, db, segment, item.clone());
        }
        Ok(item)
    }

    /// Resolves a concrete item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    /// Item not inside a statement.
    pub fn resolve_concrete_path(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        path: impl AsSegments,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedConcreteItem> {
        self.resolve_concrete_path_ex(diagnostics, path, item_type, None)
    }

    /// Resolves a concrete item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    pub fn resolve_concrete_path_ex(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        path: impl AsSegments,
        item_type: NotFoundItemType,
        statement_env: Option<&mut Environment>,
    ) -> Maybe<ResolvedConcreteItem> {
        self.resolve_path_inner::<ResolvedConcreteItem>(
            diagnostics,
            path,
            item_type,
            statement_env,
            ResolvePathInnerCallbacks {
                resolved_item_type: PhantomData,
                resolve_path_first_segment: |resolver, diagnostics, segments, statement_env| {
                    resolver.resolve_concrete_path_first_segment(
                        diagnostics,
                        segments,
                        statement_env,
                    )
                },
                resolve_path_next_segment: |resolver, diagnostics, item, segment, item_type| {
                    resolver.resolve_path_next_segment_concrete(
                        diagnostics,
                        item,
                        segment,
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

    /// Specializes the item found in the current segment, and checks its usability.
    fn specialize_generic_inner_item(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        module_id: ModuleId,
        identifier: &TerminalIdentifier,
        inner_item_info: ModuleItemInfo,
        segment: &ast::PathSegment,
    ) -> Maybe<ResolvedConcreteItem> {
        let generic_args_syntax = segment.generic_args(self.db.upcast());
        let segment_stable_ptr = segment.stable_ptr().untyped();
        self.validate_module_item_usability(diagnostics, module_id, identifier, &inner_item_info);
        self.data.used_items.insert(LookupItemId::ModuleItem(inner_item_info.item_id));
        let inner_generic_item =
            ResolvedGenericItem::from_module_item(self.db, inner_item_info.item_id)?;
        let mut specialized_item = self.specialize_generic_module_item(
            diagnostics,
            identifier,
            inner_generic_item,
            generic_args_syntax.clone(),
        )?;
        self.handle_same_impl_trait(
            diagnostics,
            &mut specialized_item,
            &generic_args_syntax.unwrap_or_default(),
            segment_stable_ptr,
        );
        Ok(specialized_item)
    }

    /// Resolves the first segment of a concrete path.
    fn resolve_concrete_path_first_segment(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        segments: &mut Peekable<std::slice::Iter<'_, ast::PathSegment>>,
        statement_env: Option<&mut Environment>,
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
                match self.determine_base(&identifier, statement_env) {
                    ResolvedBase::Module(module_id) => ResolvedConcreteItem::Module(module_id),
                    ResolvedBase::Crate(_) => {
                        // Crates do not have generics.
                        return Err(diagnostics.report(
                            &generic_segment.generic_args(syntax_db),
                            UnexpectedGenericArgs,
                        ));
                    }
                    ResolvedBase::StatementEnvironment(generic_item) => {
                        let segment = segments.next().unwrap();
                        self.specialize_generic_statement_arg(
                            segment,
                            diagnostics,
                            &identifier,
                            generic_item,
                            segment.generic_args(syntax_db),
                        )
                    }
                    ResolvedBase::FoundThroughGlobalUse {
                        item_info: inner_module_item,
                        containing_module: module_id,
                    } => {
                        let segment = segments.next().unwrap();
                        self.specialize_generic_inner_item(
                            diagnostics,
                            module_id,
                            &identifier,
                            inner_module_item,
                            segment,
                        )?
                    }
                    ResolvedBase::Ambiguous(module_items) => {
                        return Err(diagnostics.report(&identifier, AmbiguousPath(module_items)));
                    }
                    ResolvedBase::ItemNotVisible(module_item_id, containing_modules) => {
                        return Err(diagnostics.report(
                            &identifier,
                            ItemNotVisible(module_item_id, containing_modules),
                        ));
                    }
                }
            }
            syntax::node::ast::PathSegment::Simple(simple_segment) => {
                let identifier = simple_segment.ident(syntax_db);

                if let Some(resolved_item) =
                    resolve_self_segment(db, diagnostics, &identifier, &self.data.trait_or_impl_ctx)
                {
                    // The first segment is `Self`. Consume it and return.
                    segments.next().unwrap();
                    return resolved_item;
                }

                if let Some(local_item) = self.determine_base_item_in_local_scope(&identifier) {
                    self.resolved_items.mark_concrete(db, segments.next().unwrap(), local_item)
                } else {
                    match self.determine_base(&identifier, statement_env) {
                        // This item lies inside a module.
                        ResolvedBase::Module(module_id) => ResolvedConcreteItem::Module(module_id),
                        ResolvedBase::Crate(crate_id) => self.resolved_items.mark_concrete(
                            db,
                            segments.next().unwrap(),
                            ResolvedConcreteItem::Module(ModuleId::CrateRoot(crate_id)),
                        ),
                        ResolvedBase::StatementEnvironment(generic_item) => {
                            let segment = segments.next().unwrap();
                            self.specialize_generic_statement_arg(
                                segment,
                                diagnostics,
                                &identifier,
                                generic_item,
                                segment.generic_args(syntax_db),
                            )
                        }
                        ResolvedBase::FoundThroughGlobalUse {
                            item_info: inner_module_item,
                            containing_module: module_id,
                        } => {
                            let segment = segments.next().unwrap();
                            self.specialize_generic_inner_item(
                                diagnostics,
                                module_id,
                                &identifier,
                                inner_module_item,
                                segment,
                            )?
                        }
                        ResolvedBase::Ambiguous(module_items) => {
                            return Err(
                                diagnostics.report(&identifier, AmbiguousPath(module_items))
                            );
                        }
                        ResolvedBase::ItemNotVisible(module_item_id, containing_modules) => {
                            return Err(diagnostics.report(
                                &identifier,
                                ItemNotVisible(module_item_id, containing_modules),
                            ));
                        }
                    }
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
        statement_env: Option<&mut Environment>,
    ) -> Maybe<ResolvedGenericItem> {
        self.resolve_generic_path_inner(diagnostics, path, item_type, false, statement_env)
    }
    /// Resolves a generic item, given a concrete item path, while ignoring the generic args.
    /// Guaranteed to result in at most one diagnostic.
    pub fn resolve_generic_path_with_args(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        path: impl AsSegments,
        item_type: NotFoundItemType,
        statement_env: Option<&mut Environment>,
    ) -> Maybe<ResolvedGenericItem> {
        self.resolve_generic_path_inner(diagnostics, path, item_type, true, statement_env)
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
        statement_env: Option<&mut Environment>,
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
            statement_env,
            ResolvePathInnerCallbacks {
                resolved_item_type: PhantomData,
                resolve_path_first_segment: |resolver, diagnostics, segments, statement_env| {
                    resolver.resolve_generic_path_first_segment(
                        diagnostics,
                        segments,
                        allow_generic_args,
                        statement_env,
                    )
                },
                resolve_path_next_segment: |resolver, diagnostics, item, segment, item_type| {
                    let identifier = segment.identifier_ast(self.db.upcast());
                    resolver.resolve_path_next_segment_generic(
                        diagnostics,
                        item,
                        &identifier,
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
        statement_env: Option<&mut Environment>,
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
                match self.determine_base(&identifier, statement_env) {
                    ResolvedBase::Module(module_id) => ResolvedGenericItem::Module(module_id),
                    ResolvedBase::Crate(_) => {
                        // Crates do not have generics.
                        return Err(diagnostics.report(
                            &generic_segment.generic_args(syntax_db),
                            UnexpectedGenericArgs,
                        ));
                    }
                    ResolvedBase::StatementEnvironment(generic_item) => generic_item,
                    ResolvedBase::FoundThroughGlobalUse {
                        item_info: inner_module_item, ..
                    } => {
                        segments.next();
                        self.data
                            .used_items
                            .insert(LookupItemId::ModuleItem(inner_module_item.item_id));
                        ResolvedGenericItem::from_module_item(self.db, inner_module_item.item_id)?
                    }
                    ResolvedBase::Ambiguous(module_items) => {
                        return Err(diagnostics.report(&identifier, AmbiguousPath(module_items)));
                    }
                    ResolvedBase::ItemNotVisible(module_item_id, containing_modules) => {
                        return Err(diagnostics.report(
                            &identifier,
                            ItemNotVisible(module_item_id, containing_modules),
                        ));
                    }
                }
            }
            syntax::node::ast::PathSegment::Simple(simple_segment) => {
                let identifier = simple_segment.ident(syntax_db);
                match self.determine_base(&identifier, statement_env) {
                    // This item lies inside a module.
                    ResolvedBase::Module(module_id) => ResolvedGenericItem::Module(module_id),
                    ResolvedBase::Crate(crate_id) => self.resolved_items.mark_generic(
                        db,
                        segments.next().unwrap(),
                        ResolvedGenericItem::Module(ModuleId::CrateRoot(crate_id)),
                    ),
                    ResolvedBase::StatementEnvironment(generic_item) => {
                        segments.next();
                        generic_item
                    }
                    ResolvedBase::FoundThroughGlobalUse {
                        item_info: inner_module_item, ..
                    } => {
                        segments.next();
                        self.data
                            .used_items
                            .insert(LookupItemId::ModuleItem(inner_module_item.item_id));
                        ResolvedGenericItem::from_module_item(self.db, inner_module_item.item_id)?
                    }
                    ResolvedBase::Ambiguous(module_items) => {
                        return Err(diagnostics.report(&identifier, AmbiguousPath(module_items)));
                    }
                    ResolvedBase::ItemNotVisible(module_item_id, containing_modules) => {
                        return Err(diagnostics.report(
                            &identifier,
                            ItemNotVisible(module_item_id, containing_modules),
                        ));
                    }
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
            ast::PathSegment::Simple(simple) => simple.ident(syntax_db).text(syntax_db) == SUPER_KW,
        }) {
            module_id = match module_id {
                ModuleId::CrateRoot(_) => {
                    return Some(Err(diagnostics.report(segment, SuperUsedInRootModule)));
                }
                ModuleId::Submodule(submodule_id) => submodule_id.parent_module(self.db.upcast()),
            };
        }
        (module_id != self.module_file_id.0).then_some(Ok(module_id))
    }

    /// Resolves the inner item of a module, given the current segment of the path.
    fn resolve_module_inner_item(
        &mut self,
        module_id: &ModuleId,
        ident: SmolStr,
        diagnostics: &mut SemanticDiagnostics,
        identifier: &TerminalIdentifier,
        item_type: NotFoundItemType,
    ) -> Maybe<ModuleItemInfo> {
        match self.db.module_item_info_by_name(*module_id, ident.clone())? {
            Some(info) => Ok(info),
            None => match self.resolve_path_using_use_star(*module_id, identifier) {
                UseStarResult::UniquePathFound(item_info) => Ok(item_info),
                UseStarResult::AmbiguousPath(module_items) => {
                    Err(diagnostics.report(identifier, AmbiguousPath(module_items)))
                }
                UseStarResult::PathNotFound => {
                    Err(diagnostics.report(identifier, PathNotFound(item_type)))
                }
                UseStarResult::ItemNotVisible(module_item_id, containing_modules) => {
                    Err(diagnostics
                        .report(identifier, ItemNotVisible(module_item_id, containing_modules)))
                }
            },
        }
    }

    /// Given the current resolved item, resolves the next segment.
    fn resolve_path_next_segment_concrete(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        containing_item: &ResolvedConcreteItem,
        segment: &ast::PathSegment,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedConcreteItem> {
        let syntax_db = self.db.upcast();
        let identifier = &segment.identifier_ast(syntax_db);
        let generic_args_syntax = segment.generic_args(syntax_db);

        let ident = identifier.text(syntax_db);

        if identifier.text(syntax_db) == SELF_TYPE_KW {
            return Err(diagnostics.report(identifier, SelfMustBeFirst));
        }

        match containing_item {
            ResolvedConcreteItem::Module(module_id) => {
                // Prefix `super` segments should be removed earlier. Middle `super` segments are
                // not allowed.
                if ident == SUPER_KW {
                    return Err(diagnostics.report(identifier, InvalidPath));
                }
                let inner_item_info = self.resolve_module_inner_item(
                    module_id,
                    ident,
                    diagnostics,
                    identifier,
                    item_type,
                )?;

                self.specialize_generic_inner_item(
                    diagnostics,
                    *module_id,
                    identifier,
                    inner_item_info,
                    segment,
                )
            }
            ResolvedConcreteItem::Type(ty) => {
                if let TypeLongId::Concrete(ConcreteTypeId::Enum(concrete_enum_id)) =
                    ty.lookup_intern(self.db)
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
            ResolvedConcreteItem::SelfTrait(concrete_trait_id) => {
                let impl_id = ImplLongId::SelfImpl(*concrete_trait_id).intern(self.db);
                let Some(trait_item_id) = self
                    .db
                    .trait_item_by_name(concrete_trait_id.trait_id(self.db), ident.clone())?
                else {
                    return Err(diagnostics.report(identifier, InvalidPath));
                };
                if let Ok(Some(trait_item_info)) = self
                    .db
                    .trait_item_info_by_name(concrete_trait_id.trait_id(self.db), ident.clone())
                {
                    self.validate_feature_constraints(diagnostics, identifier, &trait_item_info);
                }
                self.data.used_items.insert(LookupItemId::TraitItem(trait_item_id));
                Ok(match trait_item_id {
                    TraitItemId::Function(trait_function_id) => {
                        ResolvedConcreteItem::Function(self.specialize_function(
                            diagnostics,
                            identifier.stable_ptr().untyped(),
                            GenericFunctionId::Impl(ImplGenericFunctionId {
                                impl_id,
                                function: trait_function_id,
                            }),
                            &generic_args_syntax.unwrap_or_default(),
                        )?)
                    }
                    TraitItemId::Type(trait_type_id) => ResolvedConcreteItem::Type(
                        TypeLongId::ImplType(ImplTypeId::new(impl_id, trait_type_id, self.db))
                            .intern(self.db),
                    ),
                    TraitItemId::Constant(trait_constant_id) => ResolvedConcreteItem::Constant(
                        ConstValue::ImplConstant(ImplConstantId::new(
                            impl_id,
                            trait_constant_id,
                            self.db,
                        ))
                        .intern(self.db),
                    ),
                    TraitItemId::Impl(trait_impl_id) => ResolvedConcreteItem::Impl(
                        ImplLongId::ImplImpl(ImplImplId::new(impl_id, trait_impl_id, self.db))
                            .intern(self.db),
                    ),
                })
            }
            ResolvedConcreteItem::Trait(concrete_trait_id) => {
                let Some(trait_item_id) = self
                    .db
                    .trait_item_by_name(concrete_trait_id.trait_id(self.db), ident.clone())?
                else {
                    return Err(diagnostics.report(identifier, InvalidPath));
                };

                if let Ok(Some(trait_item_info)) = self
                    .db
                    .trait_item_info_by_name(concrete_trait_id.trait_id(self.db), ident.clone())
                {
                    self.validate_feature_constraints(diagnostics, identifier, &trait_item_info);
                }
                self.data.used_items.insert(LookupItemId::TraitItem(trait_item_id));

                match trait_item_id {
                    TraitItemId::Function(trait_function_id) => {
                        let concrete_trait_function = ConcreteTraitGenericFunctionLongId::new(
                            self.db,
                            *concrete_trait_id,
                            trait_function_id,
                        )
                        .intern(self.db);
                        let identifier_stable_ptr = identifier.stable_ptr().untyped();
                        let impl_lookup_context = self.impl_lookup_context();
                        let generic_function =
                            GenericFunctionId::Impl(self.inference().infer_trait_generic_function(
                                concrete_trait_function,
                                &impl_lookup_context,
                                Some(identifier_stable_ptr),
                            ));

                        Ok(ResolvedConcreteItem::Function(self.specialize_function(
                            diagnostics,
                            identifier_stable_ptr,
                            generic_function,
                            &generic_args_syntax.unwrap_or_default(),
                        )?))
                    }
                    TraitItemId::Type(trait_type_id) => {
                        let concrete_trait_type =
                            ConcreteTraitTypeId::new(self.db, *concrete_trait_id, trait_type_id);

                        let impl_lookup_context = self.impl_lookup_context();
                        let identifier_stable_ptr = identifier.stable_ptr().untyped();
                        let ty = self.inference().infer_trait_type(
                            concrete_trait_type,
                            &impl_lookup_context,
                            Some(identifier_stable_ptr),
                        );
                        Ok(ResolvedConcreteItem::Type(self.inference().rewrite(ty).no_err()))
                    }
                    TraitItemId::Constant(trait_constant_id) => {
                        let concrete_trait_constant = ConcreteTraitConstantLongId::new(
                            self.db,
                            *concrete_trait_id,
                            trait_constant_id,
                        )
                        .intern(self.db);

                        let impl_lookup_context = self.impl_lookup_context();
                        let identifier_stable_ptr = identifier.stable_ptr().untyped();
                        let imp_constant_id = self.inference().infer_trait_constant(
                            concrete_trait_constant,
                            &impl_lookup_context,
                            Some(identifier_stable_ptr),
                        );
                        // Make sure the inference is solved for successful impl lookup
                        // Ignore the result of the `solve()` call - the error, if any, will be
                        // reported later.
                        self.inference().solve().ok();

                        Ok(ResolvedConcreteItem::Constant(
                            ConstValue::ImplConstant(imp_constant_id).intern(self.db),
                        ))
                    }
                    TraitItemId::Impl(trait_impl_id) => {
                        let concrete_trait_impl = ConcreteTraitImplLongId::new(
                            self.db,
                            *concrete_trait_id,
                            trait_impl_id,
                        )
                        .intern(self.db);

                        let impl_lookup_context = self.impl_lookup_context();
                        let identifier_stable_ptr = identifier.stable_ptr().untyped();
                        let impl_impl_id = self.inference().infer_trait_impl(
                            concrete_trait_impl,
                            &impl_lookup_context,
                            Some(identifier_stable_ptr),
                        );
                        // Make sure the inference is solved for successful impl lookup
                        // Ignore the result of the `solve()` call - the error, if any, will be
                        // reported later.
                        self.inference().solve().ok();

                        Ok(ResolvedConcreteItem::Impl(
                            ImplLongId::ImplImpl(impl_impl_id).intern(self.db),
                        ))
                    }
                }
            }
            ResolvedConcreteItem::Impl(impl_id) => {
                let concrete_trait_id = self.db.impl_concrete_trait(*impl_id)?;
                let trait_id = concrete_trait_id.trait_id(self.db);
                let Some(trait_item_id) = self.db.trait_item_by_name(trait_id, ident.clone())?
                else {
                    return Err(diagnostics.report(identifier, InvalidPath));
                };
                if let Ok(Some(trait_item_info)) = self
                    .db
                    .trait_item_info_by_name(concrete_trait_id.trait_id(self.db), ident.clone())
                {
                    self.validate_feature_constraints(diagnostics, identifier, &trait_item_info);
                }
                if let ImplLongId::Concrete(concrete_impl) = impl_id.lookup_intern(self.db) {
                    let impl_def_id: ImplDefId = concrete_impl.impl_def_id(self.db);

                    if let Ok(Some(impl_item_info)) =
                        self.db.impl_item_info_by_name(impl_def_id, ident.clone())
                    {
                        self.validate_feature_constraints(diagnostics, identifier, &impl_item_info);
                    }
                }
                self.data.used_items.insert(LookupItemId::TraitItem(trait_item_id));

                match trait_item_id {
                    TraitItemId::Function(trait_function_id) => {
                        let generic_function_id = GenericFunctionId::Impl(ImplGenericFunctionId {
                            impl_id: *impl_id,
                            function: trait_function_id,
                        });

                        Ok(ResolvedConcreteItem::Function(self.specialize_function(
                            diagnostics,
                            identifier.stable_ptr().untyped(),
                            generic_function_id,
                            &generic_args_syntax.unwrap_or_default(),
                        )?))
                    }
                    TraitItemId::Type(trait_type_id) => {
                        let impl_type_id = ImplTypeId::new(*impl_id, trait_type_id, self.db);
                        let ty = self
                            .inference()
                            .reduce_impl_ty(impl_type_id)
                            .unwrap_or_else(|_| TypeLongId::ImplType(impl_type_id).intern(self.db));
                        Ok(ResolvedConcreteItem::Type(ty))
                    }
                    TraitItemId::Constant(trait_constant_id) => {
                        let impl_constant_id =
                            ImplConstantId::new(*impl_id, trait_constant_id, self.db);

                        let constant =
                            self.inference().reduce_impl_constant(impl_constant_id).unwrap_or_else(
                                |_| ConstValue::ImplConstant(impl_constant_id).intern(self.db),
                            );

                        Ok(ResolvedConcreteItem::Constant(constant))
                    }
                    TraitItemId::Impl(trait_impl_id) => {
                        let impl_impl_id = ImplImplId::new(*impl_id, trait_impl_id, self.db);
                        let imp = self
                            .inference()
                            .reduce_impl_impl(impl_impl_id)
                            .unwrap_or_else(|_| ImplLongId::ImplImpl(impl_impl_id).intern(self.db));

                        Ok(ResolvedConcreteItem::Impl(imp))
                    }
                }
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
                Ok(ResolvedConcreteItem::Type(TypeLongId::Coupon(*function_id).intern(self.db)))
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
            ResolvedGenericItem::GenericConstant(id) => {
                ResolvedConcreteItem::Constant(self.db.constant_const_value(id)?)
            }
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
                    &generic_args_syntax.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::GenericType(generic_type) => {
                ResolvedConcreteItem::Type(self.specialize_type(
                    diagnostics,
                    identifier.stable_ptr().untyped(),
                    generic_type,
                    &generic_args_syntax.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::GenericTypeAlias(module_type_alias_id) => {
                let ty = self.db.module_type_alias_resolved_type(module_type_alias_id)?;
                let generic_params =
                    self.db.module_type_alias_generic_params(module_type_alias_id)?;
                let generic_args = self.resolve_generic_args(
                    diagnostics,
                    GenericSubstitution::default(),
                    &generic_params,
                    &generic_args_syntax.unwrap_or_default(),
                    identifier.stable_ptr().untyped(),
                )?;
                ResolvedConcreteItem::Type(
                    GenericSubstitution::new(&generic_params, &generic_args)
                        .substitute(self.db, ty)?,
                )
            }
            ResolvedGenericItem::GenericImplAlias(impl_alias_id) => {
                let impl_id = self.db.impl_alias_resolved_impl(impl_alias_id)?;
                let generic_params = self.db.impl_alias_generic_params(impl_alias_id)?;
                let generic_args = self.resolve_generic_args(
                    diagnostics,
                    GenericSubstitution::default(),
                    &generic_params,
                    &generic_args_syntax.unwrap_or_default(),
                    identifier.stable_ptr().untyped(),
                )?;
                ResolvedConcreteItem::Impl(
                    GenericSubstitution::new(&generic_params, &generic_args)
                        .substitute(self.db, impl_id)?,
                )
            }
            ResolvedGenericItem::Trait(trait_id) => {
                ResolvedConcreteItem::Trait(self.specialize_trait(
                    diagnostics,
                    identifier.stable_ptr().untyped(),
                    trait_id,
                    &generic_args_syntax.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::Impl(impl_def_id) => ResolvedConcreteItem::Impl(
                ImplLongId::Concrete(self.specialize_impl(
                    diagnostics,
                    identifier.stable_ptr().untyped(),
                    impl_def_id,
                    &generic_args_syntax.unwrap_or_default(),
                )?)
                .intern(self.db),
            ),
            ResolvedGenericItem::Variant(var) => {
                ResolvedConcreteItem::Variant(self.specialize_variant(
                    diagnostics,
                    identifier.stable_ptr().untyped(),
                    var.id,
                    &generic_args_syntax.unwrap_or_default(),
                )?)
            }
            ResolvedGenericItem::Variable(_) => panic!("Variable is not a module item."),
        })
    }

    /// Resolves an item using the `use *` imports.
    fn resolve_path_using_use_star(
        &mut self,
        module_id: ModuleId,
        identifier: &ast::TerminalIdentifier,
    ) -> UseStarResult {
        let mut item_info = None;
        let mut module_items_found: OrderedHashSet<ModuleItemId> = OrderedHashSet::default();
        let imported_modules = self.db.priv_module_use_star_modules(module_id);
        for (star_module_id, item_module_id) in &imported_modules.accessible {
            if let Some(inner_item_info) =
                self.resolve_item_in_imported_module(*item_module_id, identifier)
            {
                if self.is_item_visible(*item_module_id, &inner_item_info, *star_module_id)
                    && self.is_item_feature_usable(&inner_item_info)
                {
                    item_info = Some(inner_item_info.clone());
                    module_items_found.insert(inner_item_info.item_id);
                }
            }
        }
        if module_items_found.len() > 1 {
            return UseStarResult::AmbiguousPath(module_items_found.iter().cloned().collect());
        }
        match item_info {
            Some(item_info) => UseStarResult::UniquePathFound(item_info),
            None => {
                let mut containing_modules = vec![];
                for star_module_id in &imported_modules.all {
                    if let Some(inner_item_info) =
                        self.resolve_item_in_imported_module(*star_module_id, identifier)
                    {
                        item_info = Some(inner_item_info.clone());
                        module_items_found.insert(inner_item_info.item_id);
                        containing_modules.push(*star_module_id);
                    }
                }
                if let Some(item_info) = item_info {
                    if module_items_found.len() > 1 {
                        UseStarResult::AmbiguousPath(module_items_found.iter().cloned().collect())
                    } else {
                        UseStarResult::ItemNotVisible(item_info.item_id, containing_modules)
                    }
                } else {
                    UseStarResult::PathNotFound
                }
            }
        }
    }

    /// Resolves an item in an imported module.
    fn resolve_item_in_imported_module(
        &mut self,
        module_id: ModuleId,
        identifier: &ast::TerminalIdentifier,
    ) -> Option<ModuleItemInfo> {
        let inner_item_info =
            self.db.module_item_info_by_name(module_id, identifier.text(self.db.upcast()));
        if let Ok(Some(inner_item_info)) = inner_item_info {
            self.data.used_items.insert(LookupItemId::ModuleItem(inner_item_info.item_id));
            return Some(inner_item_info);
        }
        None
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
                let inner_item_info = self.resolve_module_inner_item(
                    module_id,
                    ident,
                    diagnostics,
                    identifier,
                    item_type,
                )?;

                self.validate_module_item_usability(
                    diagnostics,
                    *module_id,
                    identifier,
                    &inner_item_info,
                );
                self.data.used_items.insert(LookupItemId::ModuleItem(inner_item_info.item_id));
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
                    TypeLongId::GenericParameter(*generic_param_id).intern(self.db),
                ),
                GenericKind::Const => ResolvedConcreteItem::Constant(
                    ConstValue::Generic(*generic_param_id).intern(self.db),
                ),
                GenericKind::Impl => ResolvedConcreteItem::Impl(
                    ImplLongId::GenericParameter(*generic_param_id).intern(self.db),
                ),
                GenericKind::NegImpl => return None,
            };
            return Some(item);
        }
        // TODO(spapini): Resolve local variables.

        None
    }

    /// Determines the base module or crate for the path resolving. Looks only in non-local scope
    /// (i.e. current module, or crates).
    fn determine_base(
        &mut self,
        identifier: &ast::TerminalIdentifier,
        statement_env: Option<&mut Environment>,
    ) -> ResolvedBase {
        let syntax_db = self.db.upcast();
        let ident = identifier.text(syntax_db);
        let module_id = self.module_file_id.0;
        if let Some(env) = statement_env {
            if let Some(inner_generic_arg) = get_statement_item_by_name(env, &ident) {
                return ResolvedBase::StatementEnvironment(inner_generic_arg);
            }
        }

        // If an item with this name is found inside the current module, use the current module.
        if let Ok(Some(_)) = self.db.module_item_by_name(module_id, ident.clone()) {
            return ResolvedBase::Module(module_id);
        }

        // If the first element is `crate`, use the crate's root module as the base module.
        if ident == CRATE_KW {
            return ResolvedBase::Crate(self.owning_crate_id);
        }
        // If the first segment is a name of a crate, use the crate's root module as the base
        // module.
        if let Some(dep) = self.settings.dependencies.get(ident.as_str()) {
            let dep_crate_id =
                CrateLongId::Real { name: ident, discriminator: dep.discriminator.clone() }
                    .intern(self.db);
            let configs = self.db.crate_configs();
            if !configs.contains_key(&dep_crate_id) {
                let get_long_id = |crate_id: CrateId| crate_id.lookup_intern(self.db);
                panic!(
                    "Invalid crate dependency: {:?}\nconfigured crates: {:#?}",
                    get_long_id(dep_crate_id),
                    configs.keys().cloned().map(get_long_id).collect_vec()
                );
            }

            return ResolvedBase::Crate(dep_crate_id);
        }
        // If the first segment is `core` - and it was not overridden by a dependency - using it.
        if ident == CORELIB_CRATE_NAME {
            return ResolvedBase::Crate(CrateId::core(self.db));
        }
        // TODO(orizi): Remove when `starknet` becomes a proper crate.
        if ident == STARKNET_CRATE_NAME {
            // Making sure we don't look for it in `*` modules, to prevent cycles.
            return ResolvedBase::Module(self.prelude_submodule());
        }
        // If an item with this name is found in one of the 'use *' imports, use the module that
        match self.resolve_path_using_use_star(module_id, identifier) {
            UseStarResult::UniquePathFound(inner_module_item) => {
                return ResolvedBase::FoundThroughGlobalUse {
                    item_info: inner_module_item,
                    containing_module: module_id,
                };
            }
            UseStarResult::AmbiguousPath(module_items) => {
                return ResolvedBase::Ambiguous(module_items);
            }
            UseStarResult::PathNotFound => {}
            UseStarResult::ItemNotVisible(module_item_id, containing_modules) => {
                let prelude = self.prelude_submodule();
                if let Ok(Some(_)) = self.db.module_item_by_name(prelude, ident) {
                    return ResolvedBase::Module(prelude);
                }
                return ResolvedBase::ItemNotVisible(module_item_id, containing_modules);
            }
        }
        ResolvedBase::Module(self.prelude_submodule())
    }

    /// Returns the crate's `prelude` submodule.
    pub fn prelude_submodule(&self) -> ModuleId {
        let prelude_submodule_name = self.settings.edition.prelude_submodule_name();
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
        generic_args: &[ast::GenericArg],
    ) -> Maybe<ConcreteTraitId> {
        // TODO(lior): Should we report diagnostic if `trait_generic_params` failed?
        let generic_params = self
            .db
            .trait_generic_params(trait_id)
            .map_err(|_| diagnostics.report(stable_ptr, UnknownTrait))?;
        let generic_args = self.resolve_generic_args(
            diagnostics,
            GenericSubstitution::default(),
            &generic_params,
            generic_args,
            stable_ptr,
        )?;

        Ok(ConcreteTraitLongId { trait_id, generic_args }.intern(self.db))
    }

    /// Specializes an impl.
    fn specialize_impl(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        stable_ptr: SyntaxStablePtrId,
        impl_def_id: ImplDefId,
        generic_args: &[ast::GenericArg],
    ) -> Maybe<ConcreteImplId> {
        // TODO(lior): Should we report diagnostic if `impl_def_generic_params` failed?
        let generic_params = self
            .db
            .impl_def_generic_params(impl_def_id)
            .map_err(|_| diagnostics.report(stable_ptr, UnknownImpl))?;
        let generic_args = self.resolve_generic_args(
            diagnostics,
            GenericSubstitution::default(),
            &generic_params,
            generic_args,
            stable_ptr,
        )?;

        Ok(ConcreteImplLongId { impl_def_id, generic_args }.intern(self.db))
    }

    /// Specializes a variant.
    fn specialize_variant(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        stable_ptr: SyntaxStablePtrId,
        variant_id: VariantId,
        generic_args: &[ast::GenericArg],
    ) -> Maybe<ConcreteVariant> {
        let concrete_enum_id = ConcreteEnumLongId {
            enum_id: variant_id.enum_id(self.db.upcast()),
            generic_args: self.resolve_generic_args(
                diagnostics,
                GenericSubstitution::default(),
                &self.db.enum_generic_params(variant_id.enum_id(self.db.upcast()))?,
                generic_args,
                stable_ptr,
            )?,
        }
        .intern(self.db);
        self.db.concrete_enum_variant(
            concrete_enum_id,
            &self.db.variant_semantic(variant_id.enum_id(self.db.upcast()), variant_id)?,
        )
    }

    /// Specializes a generic function.
    pub fn specialize_function(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        stable_ptr: SyntaxStablePtrId,
        generic_function: GenericFunctionId,
        generic_args: &[ast::GenericArg],
    ) -> Maybe<FunctionId> {
        // TODO(lior): Should we report diagnostic if `impl_def_generic_params` failed?
        let generic_params: Vec<_> = generic_function.generic_params(self.db)?;
        let generic_args = self.resolve_generic_args(
            diagnostics,
            GenericSubstitution::default(),
            &generic_params,
            generic_args,
            stable_ptr,
        )?;

        Ok(FunctionLongId { function: ConcreteFunction { generic_function, generic_args } }
            .intern(self.db))
    }

    /// Specializes a generic type.
    pub fn specialize_type(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        stable_ptr: SyntaxStablePtrId,
        generic_type: GenericTypeId,
        generic_args: &[ast::GenericArg],
    ) -> Maybe<TypeId> {
        let generic_params = self
            .db
            .generic_type_generic_params(generic_type)
            .map_err(|_| diagnostics.report(stable_ptr, UnknownType))?;
        let generic_args = self.resolve_generic_args(
            diagnostics,
            GenericSubstitution::default(),
            &generic_params,
            generic_args,
            stable_ptr,
        )?;

        Ok(TypeLongId::Concrete(ConcreteTypeId::new(self.db, generic_type, generic_args))
            .intern(self.db))
    }

    pub fn impl_lookup_context(&self) -> ImplLookupContext {
        let mut lookup_context =
            ImplLookupContext::new(self.module_file_id.0, self.generic_params.clone());

        if let TraitOrImplContext::Impl(impl_def_id) = &self.trait_or_impl_ctx {
            let Ok(generic_params) = self.db.impl_def_generic_params(*impl_def_id) else {
                return lookup_context;
            };
            let generic_args = generic_params_to_args(generic_params.as_slice(), self.db);
            let impl_id: ConcreteImplId =
                ConcreteImplLongId { impl_def_id: *impl_def_id, generic_args }.intern(self.db);
            lookup_context.insert_impl(ImplLongId::Concrete(impl_id).intern(self.db));
        }
        lookup_context
    }

    /// Resolves generic arguments.
    /// For each generic argument, if the syntax is provided, it will be resolved by the inference.
    /// Otherwise, resolved by type.
    pub fn resolve_generic_args(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        mut substitution: GenericSubstitution,
        generic_params: &[GenericParam],
        generic_args_syntax: &[ast::GenericArg],
        stable_ptr: SyntaxStablePtrId,
    ) -> Maybe<Vec<GenericArgumentId>> {
        let mut resolved_args = vec![];
        let arg_syntax_per_param =
            self.get_arg_syntax_per_param(diagnostics, generic_params, generic_args_syntax)?;

        for generic_param in generic_params.iter() {
            let generic_param = substitution.substitute(self.db, generic_param.clone())?;
            let generic_arg = self.resolve_generic_arg(
                &generic_param,
                arg_syntax_per_param
                    .get(&generic_param.id())
                    .and_then(|arg_syntax| {
                        if let ast::GenericArgValue::Expr(expr) = arg_syntax {
                            Some(expr.expr(self.db.upcast()))
                        } else {
                            None
                        }
                    })
                    .as_ref(),
                stable_ptr,
                diagnostics,
            )?;
            resolved_args.push(generic_arg);
            substitution.insert(generic_param.id(), generic_arg);
        }

        Ok(resolved_args)
    }

    /// Returns a map of generic param id -> its assigned arg syntax.
    fn get_arg_syntax_per_param(
        &self,
        diagnostics: &mut SemanticDiagnostics,
        generic_params: &[GenericParam],
        generic_args_syntax: &[ast::GenericArg],
    ) -> Maybe<UnorderedHashMap<GenericParamId, ast::GenericArgValue>> {
        let syntax_db = self.db.upcast();
        let mut arg_syntax_per_param =
            UnorderedHashMap::<GenericParamId, ast::GenericArgValue>::default();
        let mut last_named_arg_index = None;
        let generic_param_by_name = generic_params
            .iter()
            .enumerate()
            .filter_map(|(i, param)| Some((param.id().name(self.db.upcast())?, (i, param.id()))))
            .collect::<UnorderedHashMap<_, _>>();
        for (idx, generic_arg_syntax) in generic_args_syntax.iter().enumerate() {
            match generic_arg_syntax {
                ast::GenericArg::Named(arg_syntax) => {
                    let name = arg_syntax.name(syntax_db).text(syntax_db);
                    let Some((index, generic_param_id)) = generic_param_by_name.get(&name) else {
                        return Err(diagnostics.report(arg_syntax, UnknownGenericParam(name)));
                    };
                    if let Some(prev_index) = last_named_arg_index {
                        if prev_index > index {
                            return Err(diagnostics.report(arg_syntax, GenericArgOutOfOrder(name)));
                        }
                    }
                    last_named_arg_index = Some(index);
                    if arg_syntax_per_param
                        .insert(*generic_param_id, arg_syntax.value(syntax_db))
                        .is_some()
                    {
                        return Err(diagnostics.report(arg_syntax, GenericArgDuplicate(name)));
                    }
                }
                ast::GenericArg::Unnamed(arg_syntax) => {
                    if last_named_arg_index.is_some() {
                        return Err(diagnostics.report(arg_syntax, PositionalGenericAfterNamed));
                    }
                    let generic_param = generic_params.get(idx).ok_or_else(|| {
                        diagnostics.report(
                            arg_syntax,
                            TooManyGenericArguments {
                                expected: generic_params.len(),
                                actual: generic_args_syntax.len(),
                            },
                        )
                    })?;
                    assert_eq!(
                        arg_syntax_per_param
                            .insert(generic_param.id(), arg_syntax.value(syntax_db)),
                        None,
                        "Unexpected duplication in ordered params."
                    );
                }
            }
        }
        Ok(arg_syntax_per_param)
    }

    /// Resolves a generic argument.
    /// If no syntax Expr is provided, inference will be used.
    /// If a syntax Expr is provided, it will be resolved by type.
    fn resolve_generic_arg(
        &mut self,
        generic_param: &GenericParam,
        generic_arg_syntax_opt: Option<&ast::Expr>,
        stable_ptr: SyntaxStablePtrId,
        diagnostics: &mut SemanticDiagnostics,
    ) -> Result<GenericArgumentId, cairo_lang_diagnostics::DiagnosticAdded> {
        let Some(generic_arg_syntax) = generic_arg_syntax_opt else {
            let lookup_context = self.impl_lookup_context();
            let inference = &mut self.data.inference_data.inference(self.db);
            return inference
                .infer_generic_arg(generic_param, lookup_context, Some(stable_ptr))
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

                // Using the resolver's data in the constant computation context, so impl vars are
                // added to the correct inference.
                let mut resolver_data =
                    ResolverData::new(self.module_file_id, self.inference_data.inference_id);
                std::mem::swap(&mut self.data, &mut resolver_data);

                let mut ctx = ComputationContext::new(
                    self.db,
                    diagnostics,
                    Resolver::with_data(self.db, resolver_data),
                    None,
                    environment,
                    ContextFunction::Global,
                );
                let value = compute_expr_semantic(&mut ctx, generic_arg_syntax);

                let const_value = resolve_const_expr_and_evaluate(
                    self.db,
                    &mut ctx,
                    &value,
                    generic_arg_syntax.stable_ptr().untyped(),
                    const_param.ty,
                    false,
                );

                // Update `self` data with const_eval_resolver's data.
                std::mem::swap(&mut ctx.resolver.data, &mut self.data);

                GenericArgumentId::Constant(const_value.intern(self.db))
            }

            GenericParam::Impl(param) => {
                let expr_path = try_extract_matches!(generic_arg_syntax, ast::Expr::Path)
                    .ok_or_else(|| diagnostics.report(generic_arg_syntax, UnknownImpl))?;
                let resolved_impl = match self.resolve_concrete_path(
                    diagnostics,
                    expr_path,
                    NotFoundItemType::Impl,
                )? {
                    ResolvedConcreteItem::Impl(resolved_impl) => resolved_impl,
                    ResolvedConcreteItem::SelfTrait(concrete_trait_id) => {
                        ImplLongId::SelfImpl(concrete_trait_id).intern(self.db)
                    }
                    _ => return Err(diagnostics.report(generic_arg_syntax, UnknownImpl)),
                };
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
                } else {
                    for (trait_ty, ty1) in param.type_constraints.iter() {
                        let ty0 = TypeLongId::ImplType(ImplTypeId::new(
                            resolved_impl,
                            *trait_ty,
                            self.db,
                        ))
                        .intern(self.db);
                        let _ = self.inference().conform_ty(ty0, *ty1).map_err(|err_set| {
                            self.inference().report_on_pending_error(
                                err_set,
                                diagnostics,
                                stable_ptr,
                            )
                        });
                    }
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
        let module_crate = module_id.owning_crate(self.db.upcast());
        let module_edition =
            self.db.crate_config(module_crate).map(|c| c.settings.edition).unwrap_or_default();
        module_edition.ignore_visibility()
            || self.settings.edition.ignore_visibility() && module_crate == self.db.core_crate()
    }

    /// Validates whether a given item is allowed based on its feature kind.
    /// This function checks if the item's feature kind is allowed in the current
    /// configuration. If the item uses an unstable, deprecated, or internal feature
    /// that is not permitted, a corresponding diagnostic error is reported.
    pub fn validate_feature_constraints<T: HasFeatureKind>(
        &self,
        diagnostics: &mut SemanticDiagnostics,
        identifier: &ast::TerminalIdentifier,
        item_info: &T,
    ) {
        match &item_info.feature_kind() {
            FeatureKind::Unstable { feature, note }
                if !self.data.feature_config.allowed_features.contains(feature) =>
            {
                diagnostics.report(
                    identifier,
                    UnstableFeature { feature_name: feature.clone(), note: note.clone() },
                );
            }
            FeatureKind::Deprecated { feature, note }
                if !self.data.feature_config.allow_deprecated
                    && !self.data.feature_config.allowed_features.contains(feature) =>
            {
                diagnostics.report(
                    identifier,
                    DeprecatedFeature { feature_name: feature.clone(), note: note.clone() },
                );
            }
            FeatureKind::Internal { feature, note }
                if !self.data.feature_config.allowed_features.contains(feature) =>
            {
                diagnostics.report(
                    identifier,
                    InternalFeature { feature_name: feature.clone(), note: note.clone() },
                );
            }
            _ => {}
        }
    }

    /// Validates that an item is usable from the current module or adds a diagnostic.
    /// This includes visibility checks and feature checks.
    fn validate_module_item_usability(
        &self,
        diagnostics: &mut SemanticDiagnostics,
        containing_module_id: ModuleId,
        identifier: &ast::TerminalIdentifier,
        item_info: &ModuleItemInfo,
    ) {
        if !self.is_item_visible(containing_module_id, item_info, self.module_file_id.0) {
            diagnostics.report(identifier, ItemNotVisible(item_info.item_id, vec![]));
        }

        self.validate_feature_constraints(diagnostics, identifier, item_info);
    }

    /// Checks if an item is visible from the current module.
    fn is_item_visible(
        &self,
        containing_module_id: ModuleId,
        item_info: &ModuleItemInfo,
        user_module: ModuleId,
    ) -> bool {
        let db = self.db.upcast();
        self.ignore_visibility_checks(containing_module_id)
            || visibility::peek_visible_in(
                db,
                item_info.visibility,
                containing_module_id,
                user_module,
            )
    }

    /// Checks if an item uses a feature that is not allowed.
    fn is_item_feature_usable(&self, item_info: &ModuleItemInfo) -> bool {
        match &item_info.feature_kind {
            FeatureKind::Unstable { feature, .. }
            | FeatureKind::Deprecated { feature, .. }
            | FeatureKind::Internal { feature, .. } => {
                self.data.feature_config.allowed_features.contains(feature)
            }
            _ => true,
        }
    }

    // TODO(yuval): on a breaking version change, consider changing warnings to errors.
    /// Warns about the use of a trait in a path inside the same trait or an impl of it, and the use
    /// of an impl in a path inside the same impl, additionally, converts a `Self` equivalent
    /// trait to be resolved as `Self`.
    /// That is, warns about using the actual path equivalent to `Self`, where `Self` can be used.
    fn handle_same_impl_trait(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        specialized_item: &mut ResolvedConcreteItem,
        generic_args_syntax_slice: &[ast::GenericArg],
        segment_stable_ptr: SyntaxStablePtrId,
    ) {
        match *specialized_item {
            ResolvedConcreteItem::Trait(current_segment_concrete_trait) => {
                match self.trait_or_impl_ctx {
                    TraitOrImplContext::None => {}
                    TraitOrImplContext::Trait(ctx_trait) => {
                        if self
                            .warn_trait_in_same_trait(
                                diagnostics,
                                current_segment_concrete_trait.trait_id(self.db),
                                generic_args_syntax_slice,
                                ctx_trait,
                                segment_stable_ptr,
                            )
                            .is_err()
                        {
                            *specialized_item =
                                ResolvedConcreteItem::SelfTrait(current_segment_concrete_trait);
                        }
                    }
                    TraitOrImplContext::Impl(ctx_impl_def_id) => {
                        self.warn_trait_in_its_impl(
                            diagnostics,
                            current_segment_concrete_trait,
                            ctx_impl_def_id,
                            segment_stable_ptr,
                        )
                        .ok();
                    }
                };
            }
            ResolvedConcreteItem::Impl(current_segment_impl_id) => {
                if let TraitOrImplContext::Impl(ctx_impl) = self.trait_or_impl_ctx {
                    // A ResolvedConcreteItem::Impl returned by
                    // `specialize_generic_module_item` must be ImplLongId::Concrete.
                    let current_segment_concrete_impl_id = extract_matches!(
                        current_segment_impl_id.lookup_intern(self.db),
                        ImplLongId::Concrete
                    );
                    self.warn_impl_in_same_impl(
                        diagnostics,
                        current_segment_concrete_impl_id.impl_def_id(self.db),
                        generic_args_syntax_slice,
                        ctx_impl,
                        segment_stable_ptr,
                    )
                    .ok();
                }
            }
            _ => {}
        };
    }

    /// Raises a warning diagnostic (and returns an error) if the segment describes an impl in the
    /// context of that impl (that is, could be expressed as `Self`).
    fn warn_impl_in_same_impl(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        current_segment_impl_def_id: ImplDefId,
        current_segment_generic_args: &[ast::GenericArg],
        ctx_impl: ImplDefId,
        segment_stable_ptr: SyntaxStablePtrId,
    ) -> Maybe<()> {
        if current_segment_impl_def_id != ctx_impl {
            return Ok(());
        }

        let generic_params = self.db.impl_def_generic_params(ctx_impl)?;
        self.compare_segment_args_to_params(
            diagnostics,
            current_segment_generic_args,
            generic_params,
            segment_stable_ptr,
            ImplInImplMustBeExplicit,
            ImplItemForbiddenInTheImpl,
        )
    }

    /// Raises a warning diagnostic (and returns an error) if the segment is of a concrete trait in
    /// the context of an impl of that concrete trait (that is, could be expressed as `Self`).
    fn warn_trait_in_its_impl(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        current_segment_concrete_trait_id: ConcreteTraitId,
        impl_ctx: ImplDefId,
        segment_stable_ptr: SyntaxStablePtrId,
    ) -> Maybe<()> {
        let ctx_impl_trait = self.db.impl_def_trait(impl_ctx)?;
        if current_segment_concrete_trait_id.trait_id(self.db) != ctx_impl_trait {
            return Ok(());
        }

        let ctx_impl_concrete_trait = self.db.impl_def_concrete_trait(impl_ctx)?;
        if ctx_impl_concrete_trait.generic_args(self.db)
            == current_segment_concrete_trait_id.generic_args(self.db)
        {
            return Err(diagnostics.report(segment_stable_ptr, TraitItemForbiddenInItsImpl));
        }
        Ok(())
    }

    /// Raises a warning diagnostic (and returns an error) if the segment describes a trait in the
    /// context of that trait (that is, could be expressed as `Self`).
    fn warn_trait_in_same_trait(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        current_segment_trait_id: TraitId,
        current_segment_generic_args: &[ast::GenericArg],
        ctx_trait: TraitId,
        segment_stable_ptr: SyntaxStablePtrId,
    ) -> Maybe<()> {
        if current_segment_trait_id != ctx_trait {
            return Ok(());
        }

        let generic_params = self.db.trait_generic_params(ctx_trait)?;
        self.compare_segment_args_to_params(
            diagnostics,
            current_segment_generic_args,
            generic_params,
            segment_stable_ptr,
            TraitInTraitMustBeExplicit,
            TraitItemForbiddenInTheTrait,
        )
    }

    /// Check if the given generic arguments exactly match the given generic parameters.
    /// This is used to check if a path segment is forbidden in the context of the item referred to
    /// by the segment.
    ///
    /// Fails if not all arguments are explicitly specified or if they are all specified and exactly
    /// the same as the parameters.
    fn compare_segment_args_to_params(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        current_segment_generic_args: &[ast::GenericArg],
        generic_params: Vec<GenericParam>,
        segment_stable_ptr: SyntaxStablePtrId,
        must_be_explicit_error: SemanticDiagnosticKind,
        item_forbidden_in_itself_explicit_error: SemanticDiagnosticKind,
    ) -> Maybe<()> {
        // This assumes the current segment item and the context items are equal. In this specific
        // case we disallow implicit arguments.
        if current_segment_generic_args.len() < generic_params.len() {
            return Err(diagnostics.report(segment_stable_ptr, must_be_explicit_error));
        }
        let resolved_args = self.resolve_generic_args(
            diagnostics,
            GenericSubstitution::default(),
            &generic_params,
            current_segment_generic_args,
            segment_stable_ptr,
        )?;

        if generic_params
            .iter()
            .zip_eq(resolved_args.iter())
            .all(|(gparam, garg)| gparam.as_arg(self.db) == *garg)
        {
            return Err(
                diagnostics.report(segment_stable_ptr, item_forbidden_in_itself_explicit_error)
            );
        }
        Ok(())
    }

    /// Specializes a ResolvedGenericItem that came from a Statement Environment.
    fn specialize_generic_statement_arg(
        &mut self,
        segment: &ast::PathSegment,
        diagnostics: &mut SemanticDiagnostics,
        identifier: &ast::TerminalIdentifier,
        inner_generic_item: ResolvedGenericItem,
        generic_args_syntax: Option<Vec<ast::GenericArg>>,
    ) -> ResolvedConcreteItem {
        let segment_stable_ptr = segment.stable_ptr().untyped();
        let mut specialized_item = self
            .specialize_generic_module_item(
                diagnostics,
                identifier,
                inner_generic_item.clone(),
                generic_args_syntax.clone(),
            )
            .unwrap();
        self.handle_same_impl_trait(
            diagnostics,
            &mut specialized_item,
            &generic_args_syntax.unwrap_or_default(),
            segment_stable_ptr,
        );
        specialized_item
    }
}

/// Resolves the segment if it's `Self`. Returns the Some(ResolvedConcreteItem) or Some(Err) if
/// segment == `Self` or None otherwise.
fn resolve_self_segment(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    identifier: &ast::TerminalIdentifier,
    trait_or_impl_ctx: &TraitOrImplContext,
) -> Option<Maybe<ResolvedConcreteItem>> {
    require(identifier.text(db.upcast()) == SELF_TYPE_KW)?;
    Some(resolve_actual_self_segment(db, diagnostics, identifier, trait_or_impl_ctx))
}

/// Resolves the `Self` segment given that it's actually `Self`.
fn resolve_actual_self_segment(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    identifier: &ast::TerminalIdentifier,
    trait_or_impl_ctx: &TraitOrImplContext,
) -> Maybe<ResolvedConcreteItem> {
    match trait_or_impl_ctx {
        TraitOrImplContext::None => Err(diagnostics.report(identifier, SelfNotSupportedInContext)),
        TraitOrImplContext::Trait(trait_id) => {
            let generic_parameters = db.trait_generic_params(*trait_id)?;
            let concrete_trait_id = ConcreteTraitLongId {
                trait_id: *trait_id,
                generic_args: generic_params_to_args(&generic_parameters, db),
            }
            .intern(db);
            Ok(ResolvedConcreteItem::SelfTrait(concrete_trait_id))
        }
        TraitOrImplContext::Impl(impl_def_id) => {
            let generic_parameters = db.impl_def_generic_params(*impl_def_id)?;
            let impl_id = ImplLongId::Concrete(
                ConcreteImplLongId {
                    impl_def_id: *impl_def_id,
                    generic_args: generic_params_to_args(&generic_parameters, db),
                }
                .intern(db),
            );
            Ok(ResolvedConcreteItem::Impl(impl_id.intern(db)))
        }
    }
}

/// The base module or crate for the path resolving.
enum ResolvedBase {
    /// The base module is a module.
    Module(ModuleId),
    /// The base module is a crate.
    Crate(CrateId),
    /// The base module to address is the statement
    StatementEnvironment(ResolvedGenericItem),
    /// The item is imported using global use.
    FoundThroughGlobalUse { item_info: ModuleItemInfo, containing_module: ModuleId },
    /// The base module is ambiguous.
    Ambiguous(Vec<ModuleItemId>),
    /// The base module is inaccessible.
    ItemNotVisible(ModuleItemId, Vec<ModuleId>),
}

/// The callbacks to be used by `resolve_path_inner`.
struct ResolvePathInnerCallbacks<ResolvedItem, ResolveFirst, ResolveNext, Validate, Mark>
where
    ResolveFirst: FnMut(
        &mut Resolver<'_>,
        &mut SemanticDiagnostics,
        &mut Peekable<std::slice::Iter<'_, ast::PathSegment>>,
        Option<&mut Environment>,
    ) -> Maybe<ResolvedItem>,
    ResolveNext: FnMut(
        &mut Resolver<'_>,
        &mut SemanticDiagnostics,
        &ResolvedItem,
        &ast::PathSegment,
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
