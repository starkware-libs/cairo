use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    FunctionTitleId, LanguageElementId, LookupItemId, ModuleItemId, NamedLanguageElementId,
    NamedLanguageElementLongId, TopLevelLanguageElementId, TraitConstantId, TraitConstantLongId,
    TraitFunctionId, TraitFunctionLongId, TraitId, TraitImplId, TraitImplLongId, TraitItemId,
    TraitTypeId, TraitTypeLongId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe, ToMaybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::OptionWrappedGenericParamListHelper;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern, define_short_id};
use smol_str::SmolStr;

use super::TraitOrImplContext;
use super::feature_kind::FeatureKind;
use super::function_with_body::{FunctionBodyData, get_implicit_precedence, get_inline_config};
use super::functions::{
    FunctionDeclarationData, GenericFunctionId, ImplGenericFunctionId, ImplicitPrecedence,
    InlineConfiguration,
};
use super::generics::{
    GenericParamsData, fmt_generic_args, generic_params_to_args, semantic_generic_params,
    semantic_generic_params_ex,
};
use super::imp::{GenericsHeadFilter, ImplLongId, TraitFilter};
use crate::db::{SemanticGroup, get_resolver_data_options};
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::{ComputationContext, ContextFunction, Environment, compute_root_expr};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::items::feature_kind::HasFeatureKind;
use crate::resolve::{ResolvedConcreteItem, Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter};
use crate::types::resolve_type;
use crate::{
    Arenas, FunctionBody, FunctionLongId, GenericArgumentId, GenericParam, Mutability,
    SemanticDiagnostic, TypeId, semantic, semantic_object_for_id,
};

#[cfg(test)]
#[path = "trt_test.rs"]
mod test;

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteTraitLongId {
    pub trait_id: TraitId,
    pub generic_args: Vec<GenericArgumentId>,
}
impl DebugWithDb<dyn SemanticGroup> for ConcreteTraitLongId {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        write!(f, "{}", self.trait_id.full_path(db.upcast()))?;
        fmt_generic_args(&self.generic_args, f, db)
    }
}

define_short_id!(
    ConcreteTraitId,
    ConcreteTraitLongId,
    SemanticGroup,
    lookup_intern_concrete_trait,
    intern_concrete_trait
);
semantic_object_for_id!(
    ConcreteTraitId,
    lookup_intern_concrete_trait,
    intern_concrete_trait,
    ConcreteTraitLongId
);
impl ConcreteTraitId {
    pub fn trait_id(&self, db: &dyn SemanticGroup) -> TraitId {
        self.lookup_intern(db).trait_id
    }
    pub fn generic_args(&self, db: &dyn SemanticGroup) -> Vec<GenericArgumentId> {
        self.lookup_intern(db).generic_args
    }
    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        self.trait_id(db).name(db.upcast())
    }
    pub fn full_path(&self, db: &dyn SemanticGroup) -> String {
        format!("{:?}", self.debug(db.elongate()))
    }
    pub fn filter(&self, db: &dyn SemanticGroup) -> TraitFilter {
        let generics_filter = match self.generic_args(db).first() {
            Some(first_generic) => match first_generic.head(db) {
                Some(head) => GenericsHeadFilter::FirstGenericFilter(head),
                None => GenericsHeadFilter::NoFilter,
            },
            None => GenericsHeadFilter::NoGenerics,
        };
        TraitFilter { trait_id: self.trait_id(db), generics_filter }
    }

    /// Returns true if the `trait` does not depend on any generics.
    pub fn is_fully_concrete(&self, db: &dyn SemanticGroup) -> bool {
        self.lookup_intern(db)
            .generic_args
            .iter()
            .all(|generic_argument_id| generic_argument_id.is_fully_concrete(db))
    }
    /// Returns true if the `trait` does not depend on impl or type variables.
    pub fn is_var_free(&self, db: &dyn SemanticGroup) -> bool {
        self.lookup_intern(db)
            .generic_args
            .iter()
            .all(|generic_argument_id| generic_argument_id.is_var_free(db))
    }
}

/// The ID of a generic function in a concrete trait.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteTraitGenericFunctionLongId {
    // Note the members are private to prevent direct call to the constructor.
    concrete_trait: ConcreteTraitId,
    trait_function: TraitFunctionId,
}
impl ConcreteTraitGenericFunctionLongId {
    pub fn new(
        db: &dyn SemanticGroup,
        concrete_trait: ConcreteTraitId,
        trait_function: TraitFunctionId,
    ) -> Self {
        assert_eq!(
            concrete_trait.trait_id(db),
            trait_function.trait_id(db.upcast()),
            "Concrete trait and trait function must belong to the same generic trait."
        );
        Self { concrete_trait, trait_function }
    }
}
define_short_id!(
    ConcreteTraitGenericFunctionId,
    ConcreteTraitGenericFunctionLongId,
    SemanticGroup,
    lookup_intern_concrete_trait_function,
    intern_concrete_trait_function
);
semantic_object_for_id!(
    ConcreteTraitGenericFunctionId,
    lookup_intern_concrete_trait_function,
    intern_concrete_trait_function,
    ConcreteTraitGenericFunctionLongId
);
impl ConcreteTraitGenericFunctionId {
    pub fn new(
        db: &dyn SemanticGroup,
        concrete_trait: ConcreteTraitId,
        trait_function: TraitFunctionId,
    ) -> Self {
        ConcreteTraitGenericFunctionLongId::new(db, concrete_trait, trait_function).intern(db)
    }

    pub fn trait_function(&self, db: &dyn SemanticGroup) -> TraitFunctionId {
        self.lookup_intern(db).trait_function
    }

    pub fn concrete_trait(&self, db: &dyn SemanticGroup) -> ConcreteTraitId {
        self.lookup_intern(db).concrete_trait
    }
}

/// The ID of a type item in a concrete trait.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteTraitTypeLongId {
    // Note the members are private to prevent direct call to the constructor.
    concrete_trait: ConcreteTraitId,
    trait_type: TraitTypeId,
}
impl ConcreteTraitTypeLongId {
    pub fn new(
        db: &dyn SemanticGroup,
        concrete_trait: ConcreteTraitId,
        trait_type: TraitTypeId,
    ) -> Self {
        assert_eq!(
            concrete_trait.trait_id(db),
            trait_type.trait_id(db.upcast()),
            "Concrete trait and trait type must belong to the same generic trait."
        );
        Self { concrete_trait, trait_type }
    }
}
define_short_id!(
    ConcreteTraitTypeId,
    ConcreteTraitTypeLongId,
    SemanticGroup,
    lookup_intern_concrete_trait_type,
    intern_concrete_trait_type
);
semantic_object_for_id!(
    ConcreteTraitTypeId,
    lookup_intern_concrete_trait_type,
    intern_concrete_trait_type,
    ConcreteTraitTypeLongId
);
impl ConcreteTraitTypeId {
    pub fn new(
        db: &dyn SemanticGroup,
        concrete_trait: ConcreteTraitId,
        trait_type: TraitTypeId,
    ) -> Self {
        ConcreteTraitTypeLongId::new(db, concrete_trait, trait_type).intern(db)
    }

    pub fn trait_type(&self, db: &dyn SemanticGroup) -> TraitTypeId {
        self.lookup_intern(db).trait_type
    }

    pub fn concrete_trait(&self, db: &dyn SemanticGroup) -> ConcreteTraitId {
        self.lookup_intern(db).concrete_trait
    }
}

/// The ID of a constant item in a concrete trait.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteTraitConstantLongId {
    // Note the members are private to prevent direct call to the constructor.
    concrete_trait: ConcreteTraitId,
    trait_constant: TraitConstantId,
}
impl ConcreteTraitConstantLongId {
    pub fn new(
        db: &dyn SemanticGroup,
        concrete_trait: ConcreteTraitId,
        trait_constant: TraitConstantId,
    ) -> Self {
        assert_eq!(
            concrete_trait.trait_id(db),
            trait_constant.trait_id(db.upcast()),
            "Concrete trait and trait constant must belong to the same generic trait."
        );
        Self { concrete_trait, trait_constant }
    }
}
define_short_id!(
    ConcreteTraitConstantId,
    ConcreteTraitConstantLongId,
    SemanticGroup,
    lookup_intern_concrete_trait_constant,
    intern_concrete_trait_constant
);
semantic_object_for_id!(
    ConcreteTraitConstantId,
    lookup_intern_concrete_trait_constant,
    intern_concrete_trait_constant,
    ConcreteTraitConstantLongId
);
impl ConcreteTraitConstantId {
    pub fn new(
        db: &dyn SemanticGroup,
        concrete_trait: ConcreteTraitId,
        trait_constant: TraitConstantId,
    ) -> Self {
        ConcreteTraitConstantLongId::new(db, concrete_trait, trait_constant).intern(db)
    }

    pub fn trait_constant(&self, db: &dyn SemanticGroup) -> TraitConstantId {
        self.lookup_intern(db).trait_constant
    }

    pub fn concrete_trait(&self, db: &dyn SemanticGroup) -> ConcreteTraitId {
        self.lookup_intern(db).concrete_trait
    }
}

/// The ID of a impl item in a concrete trait.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteTraitImplLongId {
    // Note the members are private to prevent direct call to the constructor.
    concrete_trait: ConcreteTraitId,
    trait_impl: TraitImplId,
}
impl ConcreteTraitImplLongId {
    pub fn new(
        db: &dyn SemanticGroup,
        concrete_trait: ConcreteTraitId,
        trait_impl: TraitImplId,
    ) -> Self {
        assert_eq!(
            concrete_trait.trait_id(db),
            trait_impl.trait_id(db.upcast()),
            "Concrete trait and trait impl must belong to the same generic trait."
        );
        Self { concrete_trait, trait_impl }
    }
}
define_short_id!(
    ConcreteTraitImplId,
    ConcreteTraitImplLongId,
    SemanticGroup,
    lookup_intern_concrete_trait_impl,
    intern_concrete_trait_impl
);
semantic_object_for_id!(
    ConcreteTraitImplId,
    lookup_intern_concrete_trait_impl,
    intern_concrete_trait_impl,
    ConcreteTraitImplLongId
);
impl ConcreteTraitImplId {
    pub fn new(
        db: &dyn SemanticGroup,
        concrete_trait: ConcreteTraitId,
        trait_impl: TraitImplId,
    ) -> Self {
        ConcreteTraitImplLongId::new(db, concrete_trait, trait_impl).intern(db)
    }

    pub fn trait_impl(&self, db: &dyn SemanticGroup) -> TraitImplId {
        self.lookup_intern(db).trait_impl
    }

    pub fn concrete_trait(&self, db: &dyn SemanticGroup) -> ConcreteTraitId {
        self.lookup_intern(db).concrete_trait
    }
}

// === Trait Declaration ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TraitDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<GenericParam>,
    attributes: Vec<Attribute>,
    resolver_data: Arc<ResolverData>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::trait_semantic_declaration_diagnostics].
pub fn trait_semantic_declaration_diagnostics(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_trait_declaration_data(trait_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::trait_generic_params].
pub fn trait_generic_params(db: &dyn SemanticGroup, trait_id: TraitId) -> Maybe<Vec<GenericParam>> {
    Ok(db.trait_generic_params_data(trait_id, false)?.generic_params)
}
/// Cycle handling for [crate::db::SemanticGroup::trait_generic_params].
pub fn trait_generic_params_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    trait_id: &TraitId,
) -> Maybe<Vec<GenericParam>> {
    // Forwarding cycle handling to `priv_generic_param_data` handler.
    Ok(db.trait_generic_params_data(*trait_id, true)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::trait_generic_params_data].
pub fn trait_generic_params_data(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
    in_cycle: bool,
) -> Maybe<GenericParamsData> {
    let syntax_db: &dyn SyntaxGroup = db.upcast();
    let module_file_id = trait_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_ast = db.module_trait_by_id(trait_id)?.to_maybe()?;

    // Generic params.
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id)));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    resolver.set_feature_config(&trait_id, &trait_ast, &mut diagnostics);
    let generic_params = semantic_generic_params_ex(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &trait_ast.generic_params(syntax_db),
        in_cycle,
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, trait_ast.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}
/// Cycle handling for [crate::db::SemanticGroup::trait_generic_params_data].
pub fn trait_generic_params_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &salsa::Cycle,
    trait_id: &TraitId,
    _in_cycle: &bool,
) -> Maybe<GenericParamsData> {
    // Forwarding cycle handling to `priv_generic_param_data` handler.
    trait_generic_params_data(db, *trait_id, true)
}
/// Query implementation of [crate::db::SemanticGroup::trait_attributes].
pub fn trait_attributes(db: &dyn SemanticGroup, trait_id: TraitId) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_trait_declaration_data(trait_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::trait_resolver_data].
pub fn trait_resolver_data(db: &dyn SemanticGroup, trait_id: TraitId) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_trait_declaration_data(trait_id)?.resolver_data)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_trait_declaration_data].
pub fn priv_trait_declaration_data(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<TraitDeclarationData> {
    let syntax_db: &dyn SyntaxGroup = db.upcast();
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let trait_ast = db.module_trait_by_id(trait_id)?.to_maybe()?;

    // Generic params.
    let generic_params_data = db.trait_generic_params_data(trait_id, false)?;
    let generic_params = generic_params_data.generic_params;
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id)));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);

    let attributes = trait_ast.attributes(syntax_db).structurize(syntax_db);

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, trait_ast.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();

    let mut resolver_data = resolver.data;
    resolver_data.trait_or_impl_ctx = TraitOrImplContext::Trait(trait_id);
    Ok(TraitDeclarationData {
        diagnostics: diagnostics.build(),
        generic_params,
        attributes,
        resolver_data: Arc::new(resolver_data),
    })
}

// === Trait Definition ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TraitDefinitionData {
    /// The diagnostics here are "flat" - that is, only the diagnostics found on the trait level
    /// itself, and don't include the diagnostics of its items. The reason it's this way is that
    /// computing the items' diagnostics require a query about their trait, forming a cycle of
    /// queries. Adding the items' diagnostics only after the whole computation breaks this cycle.
    diagnostics: Diagnostics<SemanticDiagnostic>,

    // AST maps.
    function_asts: OrderedHashMap<TraitFunctionId, ast::TraitItemFunction>,
    item_type_asts: OrderedHashMap<TraitTypeId, ast::TraitItemType>,
    item_constant_asts: OrderedHashMap<TraitConstantId, ast::TraitItemConstant>,
    item_impl_asts: OrderedHashMap<TraitImplId, ast::TraitItemImpl>,

    /// Mapping of item names to their IDs. All the IDs should appear in one of the AST maps above.
    item_id_by_name: Arc<OrderedHashMap<SmolStr, TraitItemInfo>>,
}

impl TraitDefinitionData {
    /// Retrieves trait item information by its name.
    pub fn get_trait_item_info(&self, item_name: &SmolStr) -> Option<TraitItemInfo> {
        self.item_id_by_name.get(item_name).cloned()
    }
}
/// Stores metadata for a trait item, including its ID and feature kind.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraitItemInfo {
    /// The unique identifier of the trait item.
    pub id: TraitItemId,
    /// The feature kind associated with this trait item.
    pub feature_kind: FeatureKind,
}

impl HasFeatureKind for TraitItemInfo {
    /// Returns the feature kind of this trait item.
    fn feature_kind(&self) -> &FeatureKind {
        &self.feature_kind
    }
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::trait_semantic_definition_diagnostics].
pub fn trait_semantic_definition_diagnostics(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Diagnostics<SemanticDiagnostic> {
    let mut diagnostics = DiagnosticsBuilder::default();

    let Ok(data) = db.priv_trait_definition_data(trait_id) else {
        return Diagnostics::default();
    };

    // The diagnostics from `priv_trait_definition_data` are only the diagnostics from the trait
    // level. They should be enriched with the items' diagnostics.
    diagnostics.extend(data.diagnostics);
    for trait_function_id in data.function_asts.keys() {
        diagnostics.extend(db.trait_function_declaration_diagnostics(*trait_function_id));
        diagnostics.extend(db.trait_function_body_diagnostics(*trait_function_id));
    }
    for trait_type_id in data.item_type_asts.keys() {
        diagnostics.extend(db.trait_type_diagnostics(*trait_type_id));
    }
    for trait_constant in data.item_constant_asts.keys() {
        diagnostics.extend(db.trait_constant_diagnostics(*trait_constant));
    }
    for trait_impl in data.item_impl_asts.keys() {
        diagnostics.extend(db.trait_impl_diagnostics(*trait_impl));
    }

    diagnostics.build()
}

/// Query implementation of [crate::db::SemanticGroup::trait_required_item_names].
pub fn trait_required_item_names(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<OrderedHashSet<SmolStr>> {
    let mut required_items = OrderedHashSet::<_>::default();
    for (item_name, item_id) in db.priv_trait_definition_data(trait_id)?.item_id_by_name.iter() {
        if match item_id.id {
            TraitItemId::Function(id) => {
                let body = id.stable_ptr(db.upcast()).lookup(db.upcast()).body(db.upcast());
                matches!(body, ast::MaybeTraitFunctionBody::None(_))
            }
            TraitItemId::Type(_) | TraitItemId::Constant(_) => true,
            TraitItemId::Impl(_) => false,
        } {
            required_items.insert(item_name.clone());
        }
    }
    Ok(required_items)
}

/// Query implementation of [crate::db::SemanticGroup::trait_item_by_name].
pub fn trait_item_by_name(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
    name: SmolStr,
) -> Maybe<Option<TraitItemId>> {
    Ok(db.priv_trait_definition_data(trait_id)?.item_id_by_name.get(&name).map(|info| info.id))
}

/// Query implementation of [crate::db::SemanticGroup::trait_item_info_by_name].
pub fn trait_item_info_by_name(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
    name: SmolStr,
) -> Maybe<Option<TraitItemInfo>> {
    let trait_definition_data = db.priv_trait_definition_data(trait_id)?;
    Ok(trait_definition_data.get_trait_item_info(&name))
}

/// Query implementation of [SemanticGroup::trait_all_used_items].
pub fn trait_all_used_items(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<Arc<OrderedHashSet<LookupItemId>>> {
    let mut all_used_items = db.trait_resolver_data(trait_id)?.used_items.clone();
    let data = db.priv_trait_definition_data(trait_id)?;
    for item in data.item_id_by_name.values() {
        for resolver_data in get_resolver_data_options(LookupItemId::TraitItem(item.id), db) {
            all_used_items.extend(resolver_data.used_items.iter().cloned());
        }
    }
    Ok(all_used_items.into())
}

/// Query implementation of [crate::db::SemanticGroup::trait_functions].
pub fn trait_functions(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<OrderedHashMap<SmolStr, TraitFunctionId>> {
    Ok(db
        .priv_trait_definition_data(trait_id)?
        .function_asts
        .keys()
        .map(|function_id| {
            let function_long_id = function_id.lookup_intern(db);
            (function_long_id.name(db.upcast()), *function_id)
        })
        .collect())
}

/// Query implementation of [crate::db::SemanticGroup::trait_function_by_name].
pub fn trait_function_by_name(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
    name: SmolStr,
) -> Maybe<Option<TraitFunctionId>> {
    Ok(db.trait_functions(trait_id)?.get(&name).copied())
}

/// Query implementation of [crate::db::SemanticGroup::trait_types].
pub fn trait_types(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<OrderedHashMap<SmolStr, TraitTypeId>> {
    Ok(db
        .priv_trait_definition_data(trait_id)?
        .item_type_asts
        .keys()
        .map(|type_id| {
            let type_long_id = type_id.lookup_intern(db);
            (type_long_id.name(db.upcast()), *type_id)
        })
        .collect())
}

/// Query implementation of [crate::db::SemanticGroup::trait_type_by_name].
pub fn trait_type_by_name(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
    name: SmolStr,
) -> Maybe<Option<TraitTypeId>> {
    Ok(db.trait_types(trait_id)?.get(&name).copied())
}

/// Query implementation of [crate::db::SemanticGroup::trait_constants].
pub fn trait_constants(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<OrderedHashMap<SmolStr, TraitConstantId>> {
    Ok(db
        .priv_trait_definition_data(trait_id)?
        .item_constant_asts
        .keys()
        .map(|constant_id| {
            let constant_long_id = constant_id.lookup_intern(db);
            (constant_long_id.name(db.upcast()), *constant_id)
        })
        .collect())
}

/// Query implementation of [crate::db::SemanticGroup::trait_constant_by_name].
pub fn trait_constant_by_name(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
    name: SmolStr,
) -> Maybe<Option<TraitConstantId>> {
    Ok(db.trait_constants(trait_id)?.get(&name).copied())
}

/// Query implementation of [crate::db::SemanticGroup::trait_impls].
pub fn trait_impls(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<OrderedHashMap<SmolStr, TraitImplId>> {
    Ok(db
        .priv_trait_definition_data(trait_id)?
        .item_impl_asts
        .keys()
        .map(|impl_id| {
            let impl_long_id = impl_id.lookup_intern(db);
            (impl_long_id.name(db.upcast()), *impl_id)
        })
        .collect())
}

/// Query implementation of [crate::db::SemanticGroup::trait_impl_by_name].
pub fn trait_impl_by_name(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
    name: SmolStr,
) -> Maybe<Option<TraitImplId>> {
    Ok(db.trait_impls(trait_id)?.get(&name).copied())
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_trait_definition_data].
pub fn priv_trait_definition_data(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<TraitDefinitionData> {
    let syntax_db: &dyn SyntaxGroup = db.upcast();

    let module_file_id = trait_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();

    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let trait_ast = db.module_trait_by_id(trait_id)?.to_maybe()?;

    let mut function_asts = OrderedHashMap::default();
    let mut item_type_asts = OrderedHashMap::default();
    let mut item_constant_asts = OrderedHashMap::default();
    let mut item_impl_asts = OrderedHashMap::default();
    let mut item_id_by_name: OrderedHashMap<SmolStr, TraitItemInfo> = OrderedHashMap::default();

    if let ast::MaybeTraitBody::Some(body) = trait_ast.body(syntax_db) {
        for item in body.items(syntax_db).elements(syntax_db) {
            match item {
                ast::TraitItem::Function(func) => {
                    let trait_func_id =
                        TraitFunctionLongId(module_file_id, func.stable_ptr()).intern(db);
                    let name_node = func.declaration(syntax_db).name(syntax_db);
                    let name = name_node.text(syntax_db);
                    let attributes = func.attributes(syntax_db);
                    let feature_kind =
                        FeatureKind::from_ast(db.upcast(), &mut diagnostics, &attributes);
                    if item_id_by_name
                        .insert(
                            name.clone(),
                            TraitItemInfo {
                                id: TraitItemId::Function(trait_func_id),
                                feature_kind,
                            },
                        )
                        .is_some()
                    {
                        diagnostics.report(
                            &name_node,
                            SemanticDiagnosticKind::NameDefinedMultipleTimes(name),
                        );
                    }
                    function_asts.insert(trait_func_id, func);
                }
                ast::TraitItem::Type(ty) => {
                    let trait_type_id = TraitTypeLongId(module_file_id, ty.stable_ptr()).intern(db);
                    let name_node = ty.name(syntax_db);
                    let name = name_node.text(syntax_db);
                    let attributes = ty.attributes(syntax_db);
                    let feature_kind =
                        FeatureKind::from_ast(db.upcast(), &mut diagnostics, &attributes);
                    if item_id_by_name
                        .insert(
                            name.clone(),
                            TraitItemInfo { id: TraitItemId::Type(trait_type_id), feature_kind },
                        )
                        .is_some()
                    {
                        diagnostics.report(
                            &name_node,
                            SemanticDiagnosticKind::NameDefinedMultipleTimes(name),
                        );
                    }
                    item_type_asts.insert(trait_type_id, ty);
                }
                ast::TraitItem::Constant(constant) => {
                    let trait_constant =
                        TraitConstantLongId(module_file_id, constant.stable_ptr()).intern(db);

                    let name_node = constant.name(syntax_db);
                    let name = name_node.text(syntax_db);
                    let attributes = constant.attributes(syntax_db);
                    let feature_kind =
                        FeatureKind::from_ast(db.upcast(), &mut diagnostics, &attributes);
                    if item_id_by_name
                        .insert(
                            name.clone(),
                            TraitItemInfo {
                                id: TraitItemId::Constant(trait_constant),
                                feature_kind,
                            },
                        )
                        .is_some()
                    {
                        diagnostics.report(
                            &name_node,
                            SemanticDiagnosticKind::NameDefinedMultipleTimes(name),
                        );
                    }
                    item_constant_asts.insert(trait_constant, constant);
                }
                ast::TraitItem::Impl(imp) => {
                    let trait_impl = TraitImplLongId(module_file_id, imp.stable_ptr()).intern(db);

                    let name_node = imp.name(syntax_db);
                    let name = name_node.text(syntax_db);
                    let attributes = imp.attributes(syntax_db);
                    let feature_kind =
                        FeatureKind::from_ast(db.upcast(), &mut diagnostics, &attributes);
                    if item_id_by_name
                        .insert(
                            name.clone(),
                            TraitItemInfo { id: TraitItemId::Impl(trait_impl), feature_kind },
                        )
                        .is_some()
                    {
                        diagnostics.report(
                            &name_node,
                            SemanticDiagnosticKind::NameDefinedMultipleTimes(name),
                        );
                    }
                    item_impl_asts.insert(trait_impl, imp);
                }
                ast::TraitItem::Missing(_) => {}
            }
        }
    }

    Ok(TraitDefinitionData {
        diagnostics: diagnostics.build(),
        function_asts,
        item_type_asts,
        item_constant_asts,
        item_impl_asts,
        item_id_by_name: Arc::new(item_id_by_name),
    })
}

// === Trait item type ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TraitItemTypeData {
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub generic_params: Vec<semantic::GenericParam>,
    pub attributes: Vec<Attribute>,
    pub resolver_data: Arc<ResolverData>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::trait_type_diagnostics].
pub fn trait_type_diagnostics(
    db: &dyn SemanticGroup,
    trait_type_id: TraitTypeId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_trait_type_data(trait_type_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::trait_type_generic_params].
pub fn trait_type_generic_params(
    db: &dyn SemanticGroup,
    trait_type_id: TraitTypeId,
) -> Maybe<Vec<GenericParam>> {
    Ok(db.priv_trait_type_generic_params_data(trait_type_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::trait_type_attributes].
pub fn trait_type_attributes(
    db: &dyn SemanticGroup,
    trait_type_id: TraitTypeId,
) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_trait_type_data(trait_type_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::trait_type_resolver_data].
pub fn trait_type_resolver_data(
    db: &dyn SemanticGroup,
    trait_type_id: TraitTypeId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_trait_type_data(trait_type_id)?.resolver_data)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_trait_type_generic_params_data].
pub fn priv_trait_type_generic_params_data(
    db: &dyn SemanticGroup,
    trait_type_id: TraitTypeId,
) -> Maybe<GenericParamsData> {
    let syntax_db = db.upcast();
    let module_file_id = trait_type_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_type_id.trait_id(db.upcast());
    let data = db.priv_trait_definition_data(trait_id)?;
    let trait_type_ast = &data.item_type_asts[&trait_type_id];
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::TraitItem(TraitItemId::Type(trait_type_id)));
    let parent_resolver_data = db.trait_resolver_data(trait_id)?;
    let mut resolver =
        Resolver::with_data(db, parent_resolver_data.clone_with_inference_id(db, inference_id));
    for trait_generic_param in db.trait_generic_params(trait_id)? {
        resolver.add_generic_param(trait_generic_param.id());
    }
    let generic_params_node = trait_type_ast.generic_params(syntax_db);
    let type_generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &generic_params_node,
    );
    let type_generic_params = resolver.inference().rewrite(type_generic_params).no_err();

    // TODO(yuval): support generics in impls (including validation), then remove this.
    // Generic parameters are not yet supported, make sure there are none.
    if !generic_params_node.is_empty(syntax_db) {
        diagnostics.report(
            &generic_params_node,
            GenericsNotSupportedInItem { scope: "Trait".into(), item_kind: "type".into() },
        );
    }

    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData {
        diagnostics: diagnostics.build(),
        generic_params: type_generic_params,
        resolver_data,
    })
}

/// Query implementation of [crate::db::SemanticGroup::priv_trait_type_data].
pub fn priv_trait_type_data(
    db: &dyn SemanticGroup,
    trait_type_id: TraitTypeId,
) -> Maybe<TraitItemTypeData> {
    let syntax_db = db.upcast();
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_type_id.trait_id(db.upcast());
    let data = db.priv_trait_definition_data(trait_id)?;
    let type_syntax = &data.item_type_asts[&trait_type_id];

    let type_generic_params_data = db.priv_trait_type_generic_params_data(trait_type_id)?;
    let type_generic_params = type_generic_params_data.generic_params;
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::TraitItem(
        TraitItemId::Type(trait_type_id),
    ));
    let resolver = Resolver::with_data(
        db,
        (*type_generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(type_generic_params_data.diagnostics);

    let attributes = type_syntax.attributes(syntax_db).structurize(syntax_db);
    let resolver_data = Arc::new(resolver.data);

    Ok(TraitItemTypeData {
        diagnostics: diagnostics.build(),
        generic_params: type_generic_params,
        attributes,
        resolver_data,
    })
}

// === Trait item constant ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TraitItemConstantData {
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub ty: TypeId,
    pub attributes: Vec<Attribute>,
    pub resolver_data: Arc<ResolverData>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::trait_constant_diagnostics].
pub fn trait_constant_diagnostics(
    db: &dyn SemanticGroup,
    trait_constant: TraitConstantId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_trait_constant_data(trait_constant).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::trait_constant_resolver_data].
pub fn trait_constant_resolver_data(
    db: &dyn SemanticGroup,
    trait_constant: TraitConstantId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_trait_constant_data(trait_constant)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::trait_constant_attributes].
pub fn trait_constant_attributes(
    db: &dyn SemanticGroup,
    trait_constant: TraitConstantId,
) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_trait_constant_data(trait_constant)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::trait_constant_type].
pub fn trait_constant_type(
    db: &dyn SemanticGroup,
    trait_constant_id: TraitConstantId,
) -> Maybe<TypeId> {
    Ok(db.priv_trait_constant_data(trait_constant_id)?.ty)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_trait_constant_data].
pub fn priv_trait_constant_data(
    db: &dyn SemanticGroup,
    trait_constant: TraitConstantId,
) -> Maybe<TraitItemConstantData> {
    let syntax_db = db.upcast();
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_constant.trait_id(db.upcast());
    let data = db.priv_trait_definition_data(trait_id)?;
    let constant_syntax = &data.item_constant_asts[&trait_constant];
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::TraitItem(
        TraitItemId::Constant(trait_constant),
    ));
    let parent_resolver_data = db.trait_resolver_data(trait_id)?;
    let mut resolver =
        Resolver::with_data(db, parent_resolver_data.clone_with_inference_id(db, inference_id));

    let ty = resolve_type(
        db,
        &mut diagnostics,
        &mut resolver,
        &constant_syntax.type_clause(syntax_db).ty(syntax_db),
    );
    let attributes = constant_syntax.attributes(syntax_db).structurize(syntax_db);
    let resolver_data = Arc::new(resolver.data);

    Ok(TraitItemConstantData { diagnostics: diagnostics.build(), ty, attributes, resolver_data })
}

/// Query implementation of [crate::db::SemanticGroup::concrete_trait_constant_type].
pub fn concrete_trait_constant_type(
    db: &dyn SemanticGroup,
    concrete_trait_constant_id: ConcreteTraitConstantId,
) -> Maybe<TypeId> {
    let concrete_trait_id = concrete_trait_constant_id.concrete_trait(db);
    let substitution = GenericSubstitution::new(
        &db.trait_generic_params(concrete_trait_id.trait_id(db))?,
        &concrete_trait_id.generic_args(db),
    );
    let generic_ty = db.trait_constant_type(concrete_trait_constant_id.trait_constant(db))?;
    substitution.substitute(db, generic_ty)
}

// === Trait item impl ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TraitItemImplData {
    pub diagnostics: Diagnostics<SemanticDiagnostic>,
    pub concrete_trait: Maybe<ConcreteTraitId>,
    pub attributes: Vec<Attribute>,
    pub resolver_data: Arc<ResolverData>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::trait_impl_diagnostics].
pub fn trait_impl_diagnostics(
    db: &dyn SemanticGroup,
    trait_impl: TraitImplId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_trait_impl_data(trait_impl).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::trait_impl_resolver_data].
pub fn trait_impl_resolver_data(
    db: &dyn SemanticGroup,
    trait_impl: TraitImplId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_trait_impl_data(trait_impl)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::trait_impl_attributes].
pub fn trait_impl_attributes(
    db: &dyn SemanticGroup,
    trait_impl: TraitImplId,
) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_trait_impl_data(trait_impl)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::trait_impl_concrete_trait].
pub fn trait_impl_concrete_trait(
    db: &dyn SemanticGroup,
    trait_impl_id: TraitImplId,
) -> Maybe<ConcreteTraitId> {
    db.priv_trait_impl_data(trait_impl_id)?.concrete_trait
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_trait_impl_data].
pub fn priv_trait_impl_data(
    db: &dyn SemanticGroup,
    trait_impl: TraitImplId,
) -> Maybe<TraitItemImplData> {
    let syntax_db = db.upcast();
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_impl.trait_id(db.upcast());
    let data = db.priv_trait_definition_data(trait_id)?;
    let impl_syntax = &data.item_impl_asts[&trait_impl];
    let trait_path = impl_syntax.trait_path(syntax_db);
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::TraitItem(TraitItemId::Impl(trait_impl)));
    let parent_resolver_data = db.trait_resolver_data(trait_id)?;
    let mut resolver =
        Resolver::with_data(db, parent_resolver_data.clone_with_inference_id(db, inference_id));
    let concrete_trait = resolver
        .resolve_concrete_path(&mut diagnostics, &trait_path, NotFoundItemType::Trait)
        .and_then(|resolved_item: crate::resolve::ResolvedConcreteItem| match resolved_item {
            ResolvedConcreteItem::Trait(id) | ResolvedConcreteItem::SelfTrait(id) => Ok(id),
            _ => Err(diagnostics.report(&trait_path, SemanticDiagnosticKind::UnknownTrait)),
        });
    let attributes = impl_syntax.attributes(syntax_db).structurize(syntax_db);
    let resolver_data = Arc::new(resolver.data);

    Ok(TraitItemImplData {
        diagnostics: diagnostics.build(),
        concrete_trait,
        attributes,
        resolver_data,
    })
}

/// Query implementation of [crate::db::SemanticGroup::concrete_trait_impl_concrete_trait].
pub fn concrete_trait_impl_concrete_trait(
    db: &dyn SemanticGroup,
    concrete_trait_impl_id: ConcreteTraitImplId,
) -> Maybe<ConcreteTraitId> {
    let concrete_trait_id = concrete_trait_impl_id.concrete_trait(db);
    GenericSubstitution::new(
        &db.trait_generic_params(concrete_trait_id.trait_id(db))?,
        &concrete_trait_id.generic_args(db),
    )
    .substitute(db, db.trait_impl_concrete_trait(concrete_trait_impl_id.trait_impl(db))?)
}

// === Trait function Declaration ===

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::trait_function_declaration_diagnostics].
pub fn trait_function_declaration_diagnostics(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_trait_function_declaration_data(trait_function_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::trait_function_signature].
pub fn trait_function_signature(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<semantic::Signature> {
    Ok(db.priv_trait_function_declaration_data(trait_function_id)?.signature)
}

/// Query implementation of [crate::db::SemanticGroup::trait_function_generic_params].
pub fn trait_function_generic_params(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<Vec<GenericParam>> {
    Ok(db.priv_trait_function_generic_params_data(trait_function_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::priv_trait_function_generic_params_data].
pub fn priv_trait_function_generic_params_data(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<GenericParamsData> {
    let syntax_db = db.upcast();
    let module_file_id = trait_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_function_id.trait_id(db.upcast());
    let data = db.priv_trait_definition_data(trait_id)?;
    let function_syntax = &data.function_asts[&trait_function_id];
    let declaration = function_syntax.declaration(syntax_db);
    let inference_id = InferenceId::LookupItemGenerics(LookupItemId::TraitItem(
        TraitItemId::Function(trait_function_id),
    ));
    let parent_resolver_data = db.trait_resolver_data(trait_id)?;
    let mut resolver =
        Resolver::with_data(db, parent_resolver_data.clone_with_inference_id(db, inference_id));
    let trait_generic_params = db.trait_generic_params(trait_id)?;
    for generic_param in trait_generic_params {
        resolver.add_generic_param(generic_param.id());
    }
    let function_generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &declaration.generic_params(syntax_db),
    );
    let function_generic_params = resolver.inference().rewrite(function_generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData {
        diagnostics: diagnostics.build(),
        generic_params: function_generic_params,
        resolver_data,
    })
}

/// Query implementation of [crate::db::SemanticGroup::trait_function_attributes].
pub fn trait_function_attributes(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_trait_function_declaration_data(trait_function_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::trait_function_resolver_data].
pub fn trait_function_resolver_data(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_trait_function_declaration_data(trait_function_id)?.resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::trait_function_declaration_inline_config].
pub fn trait_function_declaration_inline_config(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<InlineConfiguration> {
    Ok(db.priv_trait_function_declaration_data(trait_function_id)?.inline_config)
}

/// Query implementation of [SemanticGroup::trait_function_declaration_implicit_precedence].
pub fn trait_function_declaration_implicit_precedence(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<ImplicitPrecedence> {
    Ok(db.priv_trait_function_declaration_data(trait_function_id)?.implicit_precedence)
}

/// Query implementation of [crate::db::SemanticGroup::trait_function_declaration_implicits].
pub fn trait_function_declaration_implicits(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<Vec<TypeId>> {
    Ok(db.priv_trait_function_declaration_data(trait_function_id)?.signature.implicits)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_trait_function_declaration_data].
pub fn priv_trait_function_declaration_data(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<FunctionDeclarationData> {
    let syntax_db = db.upcast();
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_function_id.trait_id(db.upcast());
    let data = db.priv_trait_definition_data(trait_id)?;
    let function_syntax = &data.function_asts[&trait_function_id];
    let declaration_syntax = function_syntax.declaration(syntax_db);
    let function_generic_params_data =
        db.priv_trait_function_generic_params_data(trait_function_id)?;
    let function_generic_params = function_generic_params_data.generic_params;
    let lookup_item_id = LookupItemId::TraitItem(TraitItemId::Function(trait_function_id));
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let mut resolver = Resolver::with_data(
        db,
        (*function_generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(function_generic_params_data.diagnostics);
    resolver.set_feature_config(&trait_function_id, function_syntax, &mut diagnostics);
    let mut environment = Environment::empty();
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &declaration_syntax,
        FunctionTitleId::Trait(trait_function_id),
        &mut environment,
    );

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, function_syntax.stable_ptr().untyped());
    let signature = inference.rewrite(signature).no_err();
    let function_generic_params = inference.rewrite(function_generic_params).no_err();

    validate_trait_function_signature(
        db,
        &mut diagnostics,
        trait_id,
        trait_function_id,
        &signature,
        &declaration_syntax.signature(syntax_db),
    );

    let attributes = function_syntax.attributes(syntax_db).structurize(syntax_db);

    let inline_config = get_inline_config(db, &mut diagnostics, &attributes)?;
    let (implicit_precedence, _) =
        get_implicit_precedence(&mut diagnostics, &mut resolver, &attributes);
    let resolver_data = Arc::new(resolver.data);

    Ok(FunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature,
        generic_params: function_generic_params,
        environment,
        attributes,
        resolver_data,
        inline_config,
        implicit_precedence,
    })
}

fn validate_trait_function_signature(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    trait_id: TraitId,
    function_id: TraitFunctionId,
    sig: &semantic::Signature,
    sig_syntax: &ast::FunctionSignature,
) {
    let syntax_db = db.upcast();
    for (idx, param) in sig.params.iter().enumerate() {
        if param.mutability == Mutability::Mutable {
            diagnostics.report(
                &sig_syntax.parameters(syntax_db).elements(syntax_db)[idx].modifiers(syntax_db),
                crate::diagnostic::SemanticDiagnosticKind::TraitParamMutable {
                    trait_id,
                    function_id,
                },
            );
        }
    }
}

// === Concrete Trait Function ===

/// Query implementation of [crate::db::SemanticGroup::concrete_trait_function_generic_params].
pub fn concrete_trait_function_generic_params(
    db: &dyn SemanticGroup,
    concrete_trait_function_id: ConcreteTraitGenericFunctionId,
) -> Maybe<Vec<GenericParam>> {
    let concrete_trait_id = concrete_trait_function_id.concrete_trait(db);
    GenericSubstitution::new(
        &db.trait_generic_params(concrete_trait_id.trait_id(db))?,
        &concrete_trait_id.generic_args(db),
    )
    .substitute(
        db,
        db.trait_function_generic_params(concrete_trait_function_id.trait_function(db))?,
    )
}

/// Query implementation of [crate::db::SemanticGroup::concrete_trait_function_signature].
pub fn concrete_trait_function_signature(
    db: &dyn SemanticGroup,
    concrete_trait_function_id: ConcreteTraitGenericFunctionId,
) -> Maybe<semantic::Signature> {
    let concrete_trait_id = concrete_trait_function_id.concrete_trait(db);
    GenericSubstitution::new(
        &db.trait_generic_params(concrete_trait_id.trait_id(db))?,
        &concrete_trait_id.generic_args(db),
    )
    .substitute(db, db.trait_function_signature(concrete_trait_function_id.trait_function(db))?)
}

// === Body ===

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::trait_function_body_diagnostics].
pub fn trait_function_body_diagnostics(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_trait_function_body_data(trait_function_id)
        .map(|data| match data {
            Some(data) => data.diagnostics,
            None => Diagnostics::<SemanticDiagnostic>::new(),
        })
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::trait_function_body].
pub fn trait_function_body(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<Option<Arc<FunctionBody>>> {
    Ok(match db.priv_trait_function_body_data(trait_function_id)? {
        Some(body_data) => Some(body_data.body),
        None => None,
    })
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_trait_function_body_data].
pub fn priv_trait_function_body_data(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<Option<FunctionBodyData>> {
    let defs_db = db.upcast();
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_function_id.trait_id(defs_db);
    let data = db.priv_trait_definition_data(trait_id)?;
    let function_syntax = &data.function_asts[&trait_function_id];
    // Compute declaration semantic.
    let trait_function_declaration_data =
        db.priv_trait_function_declaration_data(trait_function_id)?;
    let parent_resolver_data = trait_function_declaration_data.resolver_data;
    let inference_id = InferenceId::LookupItemDefinition(LookupItemId::TraitItem(
        TraitItemId::Function(trait_function_id),
    ));
    let mut resolver =
        Resolver::with_data(db, (*parent_resolver_data).clone_with_inference_id(db, inference_id));
    resolver.trait_or_impl_ctx = TraitOrImplContext::Trait(trait_id);
    let environment = trait_function_declaration_data.environment;

    let function_id = (|| {
        let generic_parameters = db.trait_generic_params(trait_id)?;
        let concrete_trait = ConcreteTraitLongId {
            trait_id,
            generic_args: generic_params_to_args(&generic_parameters, db),
        }
        .intern(db);
        let generic_function = GenericFunctionId::Impl(ImplGenericFunctionId {
            impl_id: ImplLongId::SelfImpl(concrete_trait).intern(db),
            function: trait_function_id,
        });

        Ok(FunctionLongId::from_generic(db, generic_function)?.intern(db))
    })();
    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(
        db,
        &mut diagnostics,
        resolver,
        Some(&trait_function_declaration_data.signature),
        environment,
        ContextFunction::Function(function_id),
    );
    let function_body = match function_syntax.body(db.upcast()) {
        ast::MaybeTraitFunctionBody::Some(expr_block) => expr_block,
        ast::MaybeTraitFunctionBody::None(_) => return Ok(None),
    };
    let return_type = trait_function_declaration_data.signature.return_type;
    let body_expr = compute_root_expr(&mut ctx, &function_body, return_type)?;
    let ComputationContext { arenas: Arenas { exprs, patterns, statements }, resolver, .. } = ctx;

    let expr_lookup: UnorderedHashMap<_, _> =
        exprs.iter().map(|(expr_id, expr)| (expr.stable_ptr(), expr_id)).collect();
    let pattern_lookup: UnorderedHashMap<_, _> =
        patterns.iter().map(|(pattern_id, pattern)| (pattern.stable_ptr(), pattern_id)).collect();
    let resolver_data = Arc::new(resolver.data);
    Ok(Some(FunctionBodyData {
        diagnostics: diagnostics.build(),
        expr_lookup,
        pattern_lookup,
        resolver_data,
        body: Arc::new(FunctionBody { arenas: Arenas { exprs, patterns, statements }, body_expr }),
    }))
}
