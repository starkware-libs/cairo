use std::iter::Peekable;
use std::ops::{Deref, DerefMut, Neg};

use cairo_lang_defs::ids::{
    GenericKind, GenericParamId, GenericTypeId, ImplDefId, LanguageElementId, ModuleFileId,
    ModuleId, TraitId,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::ids::CrateLongId;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::ast::Expr;
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

use crate::corelib::core_module;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::expr::inference::conform::InferenceConform;
use crate::expr::inference::infers::InferenceEmbeddings;
use crate::expr::inference::{Inference, InferenceData, InferenceId};
use crate::items::enm::SemanticEnumEx;
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::imp::{ConcreteImplId, ConcreteImplLongId, ImplId, ImplLookupContext};
use crate::items::trt::{ConcreteTraitGenericFunctionLongId, ConcreteTraitId, ConcreteTraitLongId};
use crate::literals::LiteralLongId;
use crate::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use crate::types::resolve_type;
use crate::{
    ConcreteFunction, ConcreteTypeId, FunctionId, FunctionLongId, GenericArgumentId, GenericParam,
    TypeId, TypeLongId,
};

#[cfg(test)]
mod test;

mod item;
pub mod scope;

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
    // Current module in which to resolve the path.
    pub module_file_id: ModuleFileId,
    // Generic parameters accessible to the resolver.
    pub generic_params: OrderedHashMap<SmolStr, GenericParamId>,
    // Lookback map for resolved identifiers in path. Used in "Go to definition".
    pub resolved_items: ResolvedItems,
    /// Inference data for the resolver.
    pub inference_data: InferenceData,
}
impl ResolverData {
    pub fn new(module_file_id: ModuleFileId, inference_id: InferenceId) -> Self {
        Self {
            module_file_id,
            generic_params: Default::default(),
            resolved_items: Default::default(),
            inference_data: InferenceData::new(inference_id),
        }
    }
    pub fn clone_with_inference_id(
        &self,
        db: &dyn SemanticGroup,
        inference_id: InferenceId,
    ) -> Self {
        Self {
            module_file_id: self.module_file_id,
            generic_params: self.generic_params.clone(),
            resolved_items: self.resolved_items.clone(),
            inference_data: self.inference_data.clone_with_inference_id(db, inference_id),
        }
    }
}

/// Resolves paths semantically.
pub struct Resolver<'db> {
    db: &'db dyn SemanticGroup,
    pub data: ResolverData,
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
        Self {
            db,
            data: ResolverData {
                module_file_id,
                generic_params: Default::default(),
                resolved_items: Default::default(),
                inference_data: InferenceData::new(inference_id),
            },
        }
    }

    pub fn with_data(db: &'db dyn SemanticGroup, data: ResolverData) -> Self {
        Self { db, data }
    }

    pub fn inference(&mut self) -> Inference<'_> {
        self.data.inference_data.inference(self.db)
    }

    /// Adds a generic param to an existing resolver.
    /// This is required since a resolver needs to exist before resolving the generic params,
    /// and thus, they are added to the Resolver only after they are resolved.
    pub fn add_generic_param(&mut self, generic_param_id: GenericParamId) {
        let db = self.db.upcast();
        self.generic_params.insert(generic_param_id.name(db), generic_param_id);
    }

    /// Resolves a concrete item, given a path.
    /// Guaranteed to result in at most one diagnostic.
    pub fn resolve_concrete_path(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        path: impl AsSegments,
        item_type: NotFoundItemType,
    ) -> Maybe<ResolvedConcreteItem> {
        let db = self.db;
        let syntax_db = db.upcast();
        let elements_vec = path.to_segments(syntax_db);
        let mut segments = elements_vec.iter().peekable();

        // Find where the first segment lies in.
        let mut item = self.resolve_concrete_path_first_segment(diagnostics, &mut segments)?;

        // Follow modules.
        while segments.peek().is_some() {
            let segment = segments.next().unwrap();
            let identifier = segment.identifier_ast(syntax_db);
            let generic_args = segment.generic_args(syntax_db);

            // If this is not the last segment, set the expected type to
            // [NotFoundItemType::Identifier].
            let cur_item_type =
                if segments.peek().is_some() { NotFoundItemType::Identifier } else { item_type };
            item = self.resolve_next_concrete(
                diagnostics,
                &item,
                &identifier,
                generic_args,
                cur_item_type,
            )?;
            self.resolved_items.mark_concrete(db, segment, item.clone());
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
        let db = self.db;
        let syntax_db = db.upcast();
        let elements_vec = path.to_segments(syntax_db);
        let mut segments = elements_vec.iter().peekable();

        // Find where the first segment lies in.
        let mut item = self.resolve_generic_path_first_segment(
            diagnostics,
            &mut segments,
            allow_generic_args,
        )?;

        // Follow modules.
        while segments.peek().is_some() {
            let segment = segments.next().unwrap();
            let identifier = match segment {
                syntax::node::ast::PathSegment::WithGenericArgs(segment) => {
                    if !allow_generic_args {
                        return Err(diagnostics
                            .report(&segment.generic_args(syntax_db), UnexpectedGenericArgs));
                    }
                    segment.ident(syntax_db)
                }
                syntax::node::ast::PathSegment::Simple(segment) => segment.ident(syntax_db),
            };

            // If this is not the last segment, set the expected type to
            // [NotFoundItemType::Identifier].
            let cur_item_type =
                if segments.peek().is_some() { NotFoundItemType::Identifier } else { item_type };
            item = self.resolve_next_generic(diagnostics, &item, &identifier, cur_item_type)?;
            self.resolved_items.mark_generic(db, segment, item.clone());
        }
        Ok(item)
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
    fn resolve_next_concrete(
        &mut self,
        diagnostics: &mut SemanticDiagnostics,
        item: &ResolvedConcreteItem,
        identifier: &ast::TerminalIdentifier,
        generic_args_syntax: Option<Vec<ast::GenericArg>>,
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
                let generic_item = ResolvedGenericItem::from_module_item(self.db, module_item)?;
                Ok(self.specialize_generic_module_item(
                    diagnostics,
                    identifier,
                    generic_item,
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
                let generic_function = self
                    .data
                    .inference_data
                    .inference(self.db)
                    .infer_trait_generic_function(
                        concrete_trait_function,
                        &impl_lookup_context,
                        Some(identifier.stable_ptr().untyped()),
                    )
                    .map_err(|err| err.report(diagnostics, identifier.stable_ptr().untyped()))?;

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
            ResolvedGenericItem::GenericTypeAlias(type_alias_id) => {
                // Check for cycles in this type alias definition.
                // TODO(orizi): Handle this without using `priv_type_alias_semantic_data`.
                self.db.priv_type_alias_semantic_data(type_alias_id)?.check_no_cycle()?;

                let ty = self.db.type_alias_resolved_type(type_alias_id)?;
                let generic_params = self.db.type_alias_generic_params(type_alias_id)?;
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
                // Check for cycles in this type alias definition.
                // TODO(orizi): Handle this without using `priv_impl_alias_semantic_data`.
                self.db.priv_impl_alias_semantic_data(impl_alias_id)?.check_no_cycle()?;

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
                ResolvedGenericItem::from_module_item(self.db, module_item)
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
        if let Some(generic_param_id) = self.data.generic_params.get(&ident) {
            let item = match generic_param_id.kind(self.db.upcast()) {
                GenericKind::Type => ResolvedConcreteItem::Type(
                    self.db.intern_type(TypeLongId::GenericParameter(*generic_param_id)),
                ),
                GenericKind::Const => todo!("Add a variant to ConstId."),
                GenericKind::Impl => {
                    ResolvedConcreteItem::Impl(ImplId::GenericParameter(*generic_param_id))
                }
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
        // TODO(spapini): Use a better interface to check if the crate exists (not using `dir`).
        if self.db.crate_root_dir(crate_id).is_some() {
            return None;
        }

        // Last resort, use the `core` crate root module as the base module.
        Some(core_module(self.db))
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
        // Check for cycles in this type alias definition.
        // TODO(orizi): Handle this without using `priv_impl_declaration_data`.
        self.db.priv_impl_declaration_data(impl_def_id)?.check_no_cycle()?;

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
        ImplLookupContext::new(
            self.module_file_id.0,
            self.generic_params.values().copied().collect(),
        )
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
            .map(|(i, param)| (param.id().name(self.db.upcast()), (i, param.id())))
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
                    if arg_syntax_for_param
                        .insert(generic_param.id(), arg_syntax.value(syntax_db))
                        .is_some()
                    {
                        return Err(diagnostics.report(
                            arg_syntax,
                            GenericArgDuplicate { name: generic_param.id().name(self.db.upcast()) },
                        ));
                    }
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
            return self
                .data
                .inference_data
                .inference(self.db)
                .infer_generic_arg(&generic_param, lookup_context, Some(stable_ptr))
                .map_err(|err| err.report(diagnostics, stable_ptr));
        };

        Ok(match generic_param {
            GenericParam::Type(_) => {
                let ty = resolve_type(self.db, diagnostics, self, generic_arg_syntax);
                GenericArgumentId::Type(ty)
            }
            GenericParam::Const(_) => {
                // TODO(spapini): Currently no bound checks are performed. Move literal validation
                // to inference finalization and use inference here. This will become more relevant
                // when we support constant expressions, which need inference.
                // TODO(mkaput): This is a dumb heuristic, the expr here should be const-evaluated.
                let syntax_db = self.db.upcast();

                let value = match generic_arg_syntax {
                    Expr::Literal(literal) => literal.numeric_value(syntax_db).unwrap_or_default(),

                    Expr::ShortString(literal) => {
                        literal.numeric_value(self.db.upcast()).unwrap_or_default()
                    }

                    // TODO(yuval): support string const generic arguments?
                    Expr::Unary(unary) => {
                        if !matches!(unary.op(syntax_db), ast::UnaryOperator::Minus(_)) {
                            return Err(diagnostics.report(generic_arg_syntax, UnknownLiteral));
                        }

                        if let Expr::Literal(literal) = unary.expr(syntax_db) {
                            literal.numeric_value(syntax_db).unwrap_or_default().neg()
                        } else {
                            return Err(diagnostics.report(generic_arg_syntax, UnknownLiteral));
                        }
                    }

                    _ => {
                        return Err(diagnostics.report(generic_arg_syntax, UnknownLiteral));
                    }
                };

                let literal = LiteralLongId { value };
                GenericArgumentId::Literal(self.db.intern_literal(literal))
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
                if self
                    .data
                    .inference_data
                    .inference(self.db)
                    .conform_traits(impl_def_concrete_trait, expected_concrete_trait)
                    .is_err()
                {
                    diagnostics.report(generic_arg_syntax, TraitMismatch);
                }
                GenericArgumentId::Impl(resolved_impl)
            }
        })
    }
}
