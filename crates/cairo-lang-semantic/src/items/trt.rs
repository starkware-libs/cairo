use std::fmt::Write;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    FunctionTitleId, GenericParamId, GenericParamLongId, LanguageElementId, LookupItemId,
    ModuleItemId, NamedLanguageElementId, NamedLanguageElementLongId, TopLevelLanguageElementId,
    TraitConstantId, TraitConstantLongId, TraitFunctionId, TraitFunctionLongId, TraitId,
    TraitImplId, TraitImplLongId, TraitItemId, TraitTypeId, TraitTypeLongId, UseId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe, MaybeAsRef};
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::helpers::OptionWrappedGenericParamListHelper;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{Intern, define_short_id};
use salsa::Database;

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
use crate::db::get_resolver_data_options;
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::compute::{ComputationContext, ContextFunction, Environment, compute_root_expr};
use crate::expr::fmt::CountingWriter;
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::items::feature_kind::HasFeatureKind;
use crate::resolve::{ResolvedConcreteItem, Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter};
use crate::types::resolve_type;
use crate::{
    FunctionBody, FunctionLongId, GenericArgumentId, GenericParam, Mutability, SemanticDiagnostic,
    TypeId, semantic, semantic_object_for_id,
};

#[cfg(test)]
#[path = "trt_test.rs"]
mod test;

#[derive(Clone, Debug, Hash, PartialEq, Eq, SemanticObject)]
pub struct ConcreteTraitLongId<'db> {
    pub trait_id: TraitId<'db>,
    pub generic_args: Vec<GenericArgumentId<'db>>,
}
impl<'db> DebugWithDb<'db> for ConcreteTraitLongId<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        let mut f = CountingWriter::new(f);
        write!(f, "{}", self.trait_id.full_path(db))?;
        fmt_generic_args(&self.generic_args, &mut f, db)
    }
}

define_short_id!(ConcreteTraitId, ConcreteTraitLongId<'db>);
semantic_object_for_id!(ConcreteTraitId, ConcreteTraitLongId<'a>);
impl<'db> ConcreteTraitId<'db> {
    pub fn trait_id(&self, db: &'db dyn Database) -> TraitId<'db> {
        self.long(db).trait_id
    }
    pub fn generic_args(&self, db: &'db dyn Database) -> &'db [GenericArgumentId<'db>] {
        &self.long(db).generic_args
    }
    pub fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        self.trait_id(db).name(db)
    }
    pub fn full_path(&self, db: &dyn Database) -> String {
        format!("{:?}", self.debug(db))
    }
    pub fn filter(&self, db: &'db dyn Database) -> TraitFilter<'db> {
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
    pub fn is_fully_concrete(&self, db: &dyn Database) -> bool {
        self.long(db)
            .generic_args
            .iter()
            .all(|generic_argument_id| generic_argument_id.is_fully_concrete(db))
    }
    /// Returns true if the `trait` does not depend on impl or type variables.
    pub fn is_var_free(&self, db: &dyn Database) -> bool {
        self.long(db)
            .generic_args
            .iter()
            .all(|generic_argument_id| generic_argument_id.is_var_free(db))
    }
}

/// The ID of a generic function in a concrete trait.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn Database)]
pub struct ConcreteTraitGenericFunctionLongId<'db> {
    // Note the members are private to prevent direct call to the constructor.
    concrete_trait: ConcreteTraitId<'db>,
    trait_function: TraitFunctionId<'db>,
}
impl<'db> ConcreteTraitGenericFunctionLongId<'db> {
    pub fn new(
        db: &dyn Database,
        concrete_trait: ConcreteTraitId<'db>,
        trait_function: TraitFunctionId<'db>,
    ) -> Self {
        assert_eq!(
            concrete_trait.trait_id(db),
            trait_function.trait_id(db),
            "Concrete trait and trait function must belong to the same generic trait."
        );
        Self { concrete_trait, trait_function }
    }
}
define_short_id!(ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId<'db>);
semantic_object_for_id!(ConcreteTraitGenericFunctionId, ConcreteTraitGenericFunctionLongId<'a>);
impl<'db> ConcreteTraitGenericFunctionId<'db> {
    pub fn new_from_data(
        db: &'db dyn Database,
        concrete_trait: ConcreteTraitId<'db>,
        trait_function: TraitFunctionId<'db>,
    ) -> Self {
        ConcreteTraitGenericFunctionLongId::new(db, concrete_trait, trait_function).intern(db)
    }

    pub fn trait_function(&self, db: &'db dyn Database) -> TraitFunctionId<'db> {
        self.long(db).trait_function
    }

    pub fn concrete_trait(&self, db: &'db dyn Database) -> ConcreteTraitId<'db> {
        self.long(db).concrete_trait
    }
}

/// The ID of a type item in a concrete trait.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn Database)]
pub struct ConcreteTraitTypeLongId<'db> {
    // Note the members are private to prevent direct call to the constructor.
    concrete_trait: ConcreteTraitId<'db>,
    trait_type: TraitTypeId<'db>,
}
impl<'db> ConcreteTraitTypeLongId<'db> {
    pub fn new(
        db: &dyn Database,
        concrete_trait: ConcreteTraitId<'db>,
        trait_type: TraitTypeId<'db>,
    ) -> Self {
        assert_eq!(
            concrete_trait.trait_id(db),
            trait_type.trait_id(db),
            "Concrete trait and trait type must belong to the same generic trait."
        );
        Self { concrete_trait, trait_type }
    }
}
define_short_id!(ConcreteTraitTypeId, ConcreteTraitTypeLongId<'db>);
semantic_object_for_id!(ConcreteTraitTypeId, ConcreteTraitTypeLongId<'a>);
impl<'db> ConcreteTraitTypeId<'db> {
    pub fn new_from_data(
        db: &'db dyn Database,
        concrete_trait: ConcreteTraitId<'db>,
        trait_type: TraitTypeId<'db>,
    ) -> Self {
        ConcreteTraitTypeLongId::new(db, concrete_trait, trait_type).intern(db)
    }

    pub fn trait_type(&self, db: &'db dyn Database) -> TraitTypeId<'db> {
        self.long(db).trait_type
    }

    pub fn concrete_trait(&self, db: &'db dyn Database) -> ConcreteTraitId<'db> {
        self.long(db).concrete_trait
    }
}

/// The ID of a constant item in a concrete trait.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn Database)]
pub struct ConcreteTraitConstantLongId<'db> {
    // Note the members are private to prevent direct call to the constructor.
    concrete_trait: ConcreteTraitId<'db>,
    trait_constant: TraitConstantId<'db>,
}
impl<'db> ConcreteTraitConstantLongId<'db> {
    pub fn new(
        db: &'db dyn Database,
        concrete_trait: ConcreteTraitId<'db>,
        trait_constant: TraitConstantId<'db>,
    ) -> Self {
        assert_eq!(
            concrete_trait.trait_id(db),
            trait_constant.trait_id(db),
            "Concrete trait and trait constant must belong to the same generic trait."
        );
        Self { concrete_trait, trait_constant }
    }
}
define_short_id!(ConcreteTraitConstantId, ConcreteTraitConstantLongId<'db>);
semantic_object_for_id!(ConcreteTraitConstantId, ConcreteTraitConstantLongId<'a>);
impl<'db> ConcreteTraitConstantId<'db> {
    pub fn new_from_data(
        db: &'db dyn Database,
        concrete_trait: ConcreteTraitId<'db>,
        trait_constant: TraitConstantId<'db>,
    ) -> Self {
        ConcreteTraitConstantLongId::new(db, concrete_trait, trait_constant).intern(db)
    }

    pub fn trait_constant(&self, db: &'db dyn Database) -> TraitConstantId<'db> {
        self.long(db).trait_constant
    }

    pub fn concrete_trait(&self, db: &'db dyn Database) -> ConcreteTraitId<'db> {
        self.long(db).concrete_trait
    }
}

/// The ID of a impl item in a concrete trait.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb, SemanticObject)]
#[debug_db(dyn Database)]
pub struct ConcreteTraitImplLongId<'db> {
    // Note the members are private to prevent direct call to the constructor.
    concrete_trait: ConcreteTraitId<'db>,
    trait_impl: TraitImplId<'db>,
}
impl<'db> ConcreteTraitImplLongId<'db> {
    pub fn new_from_data(
        db: &dyn Database,
        concrete_trait: ConcreteTraitId<'db>,
        trait_impl: TraitImplId<'db>,
    ) -> Self {
        assert_eq!(
            concrete_trait.trait_id(db),
            trait_impl.trait_id(db),
            "Concrete trait and trait impl must belong to the same generic trait."
        );
        Self { concrete_trait, trait_impl }
    }
}
define_short_id!(ConcreteTraitImplId, ConcreteTraitImplLongId<'db>);
semantic_object_for_id!(ConcreteTraitImplId, ConcreteTraitImplLongId<'a>);
impl<'db> ConcreteTraitImplId<'db> {
    pub fn new_from_data(
        db: &'db dyn Database,
        concrete_trait: ConcreteTraitId<'db>,
        trait_impl: TraitImplId<'db>,
    ) -> Self {
        ConcreteTraitImplLongId::new_from_data(db, concrete_trait, trait_impl).intern(db)
    }

    pub fn trait_impl(&self, db: &'db dyn Database) -> TraitImplId<'db> {
        self.long(db).trait_impl
    }

    pub fn concrete_trait(&self, db: &'db dyn Database) -> ConcreteTraitId<'db> {
        self.long(db).concrete_trait
    }
}

// === Trait Declaration ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
struct TraitDeclarationData<'db> {
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    attributes: Vec<Attribute<'db>>,
    resolver_data: Arc<ResolverData<'db>>,
}

/// Implementation of [PrivTraitSemantic::trait_generic_params_data].
fn trait_generic_params_data<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
    in_cycle: bool,
) -> Maybe<GenericParamsData<'db>> {
    let module_id = trait_id.module_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_ast = db.module_trait_by_id(trait_id)?;

    // Generic params.
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id)));
    let mut resolver = Resolver::new(db, module_id, inference_id);
    resolver.set_feature_config(&trait_id, &trait_ast, &mut diagnostics);
    let generic_params = semantic_generic_params_ex(
        db,
        &mut diagnostics,
        &mut resolver,
        module_id,
        &trait_ast.generic_params(db),
        in_cycle,
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, trait_ast.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Query implementation of [TraitSemantic::trait_generic_params_data].
#[salsa::tracked(cycle_result=trait_generic_params_data_cycle, returns(ref))]
fn trait_generic_params_data_tracked<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
    in_cycle: bool,
) -> Maybe<GenericParamsData<'db>> {
    trait_generic_params_data(db, trait_id, in_cycle)
}

/// Cycle handling for [PrivTraitSemantic::trait_generic_params_data].
fn trait_generic_params_data_cycle<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
    _in_cycle: bool,
) -> Maybe<GenericParamsData<'db>> {
    // Forwarding cycle handling to `priv_generic_param_data` handler.
    trait_generic_params_data(db, trait_id, true)
}

/// Query implementation of [TraitSemantic::trait_generic_params_ids].
#[salsa::tracked(returns(ref))]
fn trait_generic_params_ids<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Maybe<Vec<GenericParamId<'db>>> {
    let module_id = trait_id.module_id(db);
    let trait_ast = db.module_trait_by_id(trait_id)?;

    let generic_params = &trait_ast.generic_params(db);

    let syntax_db = db;
    Ok(match generic_params {
        syntax::node::ast::OptionWrappedGenericParamList::Empty(_) => vec![],
        syntax::node::ast::OptionWrappedGenericParamList::WrappedGenericParamList(syntax) => syntax
            .generic_params(syntax_db)
            .elements(syntax_db)
            .map(|param_syntax| {
                GenericParamLongId(module_id, param_syntax.stable_ptr(syntax_db)).intern(db)
            })
            .collect(),
    })
}

/// Query implementation of [TraitSemantic::priv_trait_declaration_data].
#[salsa::tracked(returns(ref))]
fn priv_trait_declaration_data<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Maybe<TraitDeclarationData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let trait_ast = db.module_trait_by_id(trait_id)?;

    // Generic params.
    let generic_params_data = db.trait_generic_params_data(trait_id, false)?;
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id)));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics.clone());

    let attributes = trait_ast.attributes(db).structurize(db);

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, trait_ast.stable_ptr(db).untyped());

    let mut resolver_data = resolver.data;
    resolver_data.trait_or_impl_ctx = TraitOrImplContext::Trait(trait_id);
    Ok(TraitDeclarationData {
        diagnostics: diagnostics.build(),
        attributes,
        resolver_data: Arc::new(resolver_data),
    })
}

// === Trait Definition ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
struct TraitDefinitionData<'db> {
    /// The diagnostics here are "flat" - that is, only the diagnostics found on the trait level
    /// itself, and don't include the diagnostics of its items. The reason it's this way is that
    /// computing the items' diagnostics require a query about their trait, forming a cycle of
    /// queries. Adding the items' diagnostics only after the whole computation breaks this cycle.
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,

    // AST maps.
    function_asts: OrderedHashMap<TraitFunctionId<'db>, ast::TraitItemFunction<'db>>,
    item_type_asts: OrderedHashMap<TraitTypeId<'db>, ast::TraitItemType<'db>>,
    item_constant_asts: OrderedHashMap<TraitConstantId<'db>, ast::TraitItemConstant<'db>>,
    item_impl_asts: OrderedHashMap<TraitImplId<'db>, ast::TraitItemImpl<'db>>,

    /// Mapping of item names to their IDs. All the IDs should appear in one of the AST maps above.
    item_id_by_name: OrderedHashMap<SmolStrId<'db>, TraitItemInfo<'db>>,
}

/// Stores metadata for a trait item, including its ID and feature kind.
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update)]
pub struct TraitItemInfo<'db> {
    /// The unique identifier of the trait item.
    pub id: TraitItemId<'db>,
    /// The feature kind associated with this trait item.
    pub feature_kind: FeatureKind<'db>,
}

impl<'db> HasFeatureKind<'db> for TraitItemInfo<'db> {
    /// Returns the feature kind of this trait item.
    fn feature_kind(&self) -> &FeatureKind<'db> {
        &self.feature_kind
    }
}

// --- Selectors ---

/// Implementation of [TraitSemantic::trait_semantic_definition_diagnostics].
fn trait_semantic_definition_diagnostics<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    let mut diagnostics = DiagnosticsBuilder::default();

    let Ok(data) = db.priv_trait_definition_data(trait_id) else {
        return Diagnostics::default();
    };

    // The diagnostics from `priv_trait_definition_data` are only the diagnostics from the trait
    // level. They should be enriched with the items' diagnostics.
    diagnostics.extend(data.diagnostics.clone());
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

/// Query implementation of [TraitSemantic::trait_semantic_definition_diagnostics].
#[salsa::tracked]
fn trait_semantic_definition_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    trait_semantic_definition_diagnostics(db, trait_id)
}

/// Query implementation of [TraitSemantic::trait_required_item_names].
#[salsa::tracked(returns(ref))]
fn trait_required_item_names<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Maybe<OrderedHashSet<SmolStrId<'db>>> {
    let mut required_items = OrderedHashSet::<_>::default();
    for (item_name, item_id) in db.priv_trait_definition_data(trait_id)?.item_id_by_name.iter() {
        if match item_id.id {
            TraitItemId::Function(id) => {
                let body = id.stable_ptr(db).lookup(db).body(db);
                matches!(body, ast::MaybeTraitFunctionBody::None(_))
            }
            TraitItemId::Type(_) | TraitItemId::Constant(_) => true,
            TraitItemId::Impl(_) => false,
        } {
            required_items.insert(*item_name);
        }
    }
    Ok(required_items)
}

/// Query implementation of [TraitSemantic::trait_all_used_uses].
#[salsa::tracked(returns(ref))]
fn trait_all_used_uses<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Maybe<OrderedHashSet<UseId<'db>>> {
    let mut all_used_uses = db.trait_resolver_data(trait_id)?.used_uses.clone();
    let data = db.priv_trait_definition_data(trait_id)?;
    for item in data.item_id_by_name.values() {
        for resolver_data in get_resolver_data_options(LookupItemId::TraitItem(item.id), db) {
            all_used_uses.extend(resolver_data.used_uses.iter().cloned());
        }
    }
    Ok(all_used_uses)
}

/// Query implementation of [TraitSemantic::trait_functions].
#[salsa::tracked(returns(ref))]
fn trait_functions<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Maybe<OrderedHashMap<SmolStrId<'db>, TraitFunctionId<'db>>> {
    Ok(db
        .priv_trait_definition_data(trait_id)?
        .function_asts
        .keys()
        .map(|function_id| {
            let function_long_id = function_id.long(db);
            (function_long_id.name(db), *function_id)
        })
        .collect())
}

/// Query implementation of [TraitSemantic::trait_types].
#[salsa::tracked(returns(ref))]
fn trait_types<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Maybe<OrderedHashMap<SmolStrId<'db>, TraitTypeId<'db>>> {
    Ok(db
        .priv_trait_definition_data(trait_id)?
        .item_type_asts
        .keys()
        .map(|type_id| {
            let type_long_id = type_id.long(db);
            (type_long_id.name(db), *type_id)
        })
        .collect())
}

/// Query implementation of [TraitSemantic::trait_constants].
#[salsa::tracked(returns(ref))]
fn trait_constants<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Maybe<OrderedHashMap<SmolStrId<'db>, TraitConstantId<'db>>> {
    Ok(db
        .priv_trait_definition_data(trait_id)?
        .item_constant_asts
        .keys()
        .map(|constant_id| {
            let constant_long_id = constant_id.long(db);
            (constant_long_id.name(db), *constant_id)
        })
        .collect())
}

/// Query implementation of [TraitSemantic::trait_impls].
#[salsa::tracked(returns(ref))]
fn trait_impls<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Maybe<OrderedHashMap<SmolStrId<'db>, TraitImplId<'db>>> {
    Ok(db
        .priv_trait_definition_data(trait_id)?
        .item_impl_asts
        .keys()
        .map(|impl_id| {
            let impl_long_id = impl_id.long(db);
            (impl_long_id.name(db), *impl_id)
        })
        .collect())
}

// --- Computation ---

/// Query implementation of [PrivTraitSemantic::priv_trait_definition_data].
#[salsa::tracked(returns(ref))]
fn priv_trait_definition_data<'db>(
    db: &'db dyn Database,
    trait_id: TraitId<'db>,
) -> Maybe<TraitDefinitionData<'db>> {
    let module_id = trait_id.module_id(db);
    let mut diagnostics = SemanticDiagnostics::default();

    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let trait_ast = db.module_trait_by_id(trait_id)?;

    let mut function_asts = OrderedHashMap::default();
    let mut item_type_asts = OrderedHashMap::default();
    let mut item_constant_asts = OrderedHashMap::default();
    let mut item_impl_asts = OrderedHashMap::default();
    let mut item_id_by_name: OrderedHashMap<SmolStrId<'db>, TraitItemInfo<'db>> =
        OrderedHashMap::default();

    if let ast::MaybeTraitBody::Some(body) = trait_ast.body(db) {
        for item in body.items(db).elements(db) {
            match item {
                ast::TraitItem::Function(func) => {
                    let trait_func_id =
                        TraitFunctionLongId(module_id, func.stable_ptr(db)).intern(db);
                    let name_node = func.declaration(db).name(db);
                    let name = name_node.text(db);
                    let attributes = func.attributes(db);
                    let feature_kind = FeatureKind::from_ast(db, &mut diagnostics, &attributes);
                    if item_id_by_name
                        .insert(
                            name,
                            TraitItemInfo {
                                id: TraitItemId::Function(trait_func_id),
                                feature_kind,
                            },
                        )
                        .is_some()
                    {
                        diagnostics.report(
                            name_node.stable_ptr(db),
                            SemanticDiagnosticKind::NameDefinedMultipleTimes(name),
                        );
                    }
                    function_asts.insert(trait_func_id, func);
                }
                ast::TraitItem::Type(ty) => {
                    let trait_type_id = TraitTypeLongId(module_id, ty.stable_ptr(db)).intern(db);
                    let name_node = ty.name(db);
                    let name = name_node.text(db);
                    let attributes = ty.attributes(db);
                    let feature_kind = FeatureKind::from_ast(db, &mut diagnostics, &attributes);
                    if item_id_by_name
                        .insert(
                            name,
                            TraitItemInfo { id: TraitItemId::Type(trait_type_id), feature_kind },
                        )
                        .is_some()
                    {
                        diagnostics.report(
                            name_node.stable_ptr(db),
                            SemanticDiagnosticKind::NameDefinedMultipleTimes(name),
                        );
                    }
                    item_type_asts.insert(trait_type_id, ty);
                }
                ast::TraitItem::Constant(constant) => {
                    let trait_constant =
                        TraitConstantLongId(module_id, constant.stable_ptr(db)).intern(db);

                    let name_node = constant.name(db);
                    let name = name_node.text(db);
                    let attributes = constant.attributes(db);
                    let feature_kind = FeatureKind::from_ast(db, &mut diagnostics, &attributes);
                    if item_id_by_name
                        .insert(
                            name,
                            TraitItemInfo {
                                id: TraitItemId::Constant(trait_constant),
                                feature_kind,
                            },
                        )
                        .is_some()
                    {
                        diagnostics.report(
                            name_node.stable_ptr(db),
                            SemanticDiagnosticKind::NameDefinedMultipleTimes(name),
                        );
                    }
                    item_constant_asts.insert(trait_constant, constant);
                }
                ast::TraitItem::Impl(imp) => {
                    let trait_impl = TraitImplLongId(module_id, imp.stable_ptr(db)).intern(db);

                    let name_node = imp.name(db);
                    let name = name_node.text(db);
                    let attributes = imp.attributes(db);
                    let feature_kind = FeatureKind::from_ast(db, &mut diagnostics, &attributes);
                    if item_id_by_name
                        .insert(
                            name,
                            TraitItemInfo { id: TraitItemId::Impl(trait_impl), feature_kind },
                        )
                        .is_some()
                    {
                        diagnostics.report(
                            name_node.stable_ptr(db),
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
        item_id_by_name,
    })
}

// === Trait item type ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
struct TraitItemTypeData<'db> {
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    pub attributes: Vec<Attribute<'db>>,
    pub resolver_data: Arc<ResolverData<'db>>,
}

/// Query implementation of [PrivTraitSemantic::priv_trait_type_generic_params_data].
#[salsa::tracked(returns(ref))]
fn priv_trait_type_generic_params_data<'db>(
    db: &'db dyn Database,
    trait_type_id: TraitTypeId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_id = trait_type_id.module_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_type_id.trait_id(db);
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
    let generic_params_node = trait_type_ast.generic_params(db);
    let type_generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_id,
        &generic_params_node,
    );
    let type_generic_params = resolver.inference().rewrite(type_generic_params).no_err();

    // TODO(yuval): support generics in impls (including validation), then remove this.
    // Generic parameters are not yet supported, make sure there are none.
    if !generic_params_node.is_empty(db) {
        diagnostics.report(
            generic_params_node.stable_ptr(db),
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

/// Query implementation of [PrivTraitSemantic::priv_trait_type_data].
#[salsa::tracked(returns(ref))]
fn priv_trait_type_data<'db>(
    db: &'db dyn Database,
    trait_type_id: TraitTypeId<'db>,
) -> Maybe<TraitItemTypeData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_type_id.trait_id(db);
    let data = db.priv_trait_definition_data(trait_id)?;
    let type_syntax = &data.item_type_asts[&trait_type_id];

    let type_generic_params_data = db.priv_trait_type_generic_params_data(trait_type_id)?;
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::TraitItem(
        TraitItemId::Type(trait_type_id),
    ));
    let resolver = Resolver::with_data(
        db,
        (*type_generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(type_generic_params_data.diagnostics.clone());

    let attributes = type_syntax.attributes(db).structurize(db);
    let resolver_data = Arc::new(resolver.data);

    Ok(TraitItemTypeData { diagnostics: diagnostics.build(), attributes, resolver_data })
}

// === Trait item constant ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
struct TraitItemConstantData<'db> {
    pub diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    pub ty: TypeId<'db>,
    pub attributes: Vec<Attribute<'db>>,
    pub resolver_data: Arc<ResolverData<'db>>,
}

// --- Computation ---

/// Query implementation of [PrivTraitSemantic::priv_trait_constant_data].
#[salsa::tracked(returns(ref))]
fn priv_trait_constant_data<'db>(
    db: &'db dyn Database,
    trait_constant: TraitConstantId<'db>,
) -> Maybe<TraitItemConstantData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_constant.trait_id(db);
    let data = db.priv_trait_definition_data(trait_id)?;
    let constant_syntax = &data.item_constant_asts[&trait_constant];
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::TraitItem(
        TraitItemId::Constant(trait_constant),
    ));
    let parent_resolver_data = db.trait_resolver_data(trait_id)?;
    let mut resolver =
        Resolver::with_data(db, parent_resolver_data.clone_with_inference_id(db, inference_id));

    let ty =
        resolve_type(db, &mut diagnostics, &mut resolver, &constant_syntax.type_clause(db).ty(db));
    let attributes = constant_syntax.attributes(db).structurize(db);
    let resolver_data = Arc::new(resolver.data);

    Ok(TraitItemConstantData { diagnostics: diagnostics.build(), ty, attributes, resolver_data })
}

/// Implementation of [TraitSemantic::concrete_trait_constant_type].
fn concrete_trait_constant_type<'db>(
    db: &'db dyn Database,
    concrete_trait_constant_id: ConcreteTraitConstantId<'db>,
) -> Maybe<TypeId<'db>> {
    let concrete_trait_id = concrete_trait_constant_id.concrete_trait(db);
    let substitution = GenericSubstitution::new(
        db.trait_generic_params(concrete_trait_id.trait_id(db))?,
        concrete_trait_id.generic_args(db),
    );
    let generic_ty = db.trait_constant_type(concrete_trait_constant_id.trait_constant(db))?;
    substitution.substitute(db, generic_ty)
}

/// Query implementation of [TraitSemantic::concrete_trait_constant_type].
#[salsa::tracked]
fn concrete_trait_constant_type_tracked<'db>(
    db: &'db dyn Database,
    concrete_trait_constant_id: ConcreteTraitConstantId<'db>,
) -> Maybe<TypeId<'db>> {
    concrete_trait_constant_type(db, concrete_trait_constant_id)
}

// === Trait item impl ===

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
struct TraitItemImplData<'db> {
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    concrete_trait: Maybe<ConcreteTraitId<'db>>,
    attributes: Vec<Attribute<'db>>,
    resolver_data: Arc<ResolverData<'db>>,
}

/// Query implementation of [PrivTraitSemantic::priv_trait_impl_data].
#[salsa::tracked(returns(ref))]
fn priv_trait_impl_data<'db>(
    db: &'db dyn Database,
    trait_impl: TraitImplId<'db>,
) -> Maybe<TraitItemImplData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_impl.trait_id(db);
    let data = db.priv_trait_definition_data(trait_id)?;
    let impl_syntax = &data.item_impl_asts[&trait_impl];
    let trait_path = impl_syntax.trait_path(db);
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::TraitItem(TraitItemId::Impl(trait_impl)));
    let parent_resolver_data = db.trait_resolver_data(trait_id)?;
    let mut resolver =
        Resolver::with_data(db, parent_resolver_data.clone_with_inference_id(db, inference_id));
    let concrete_trait = resolver
        .resolve_concrete_path(&mut diagnostics, &trait_path, NotFoundItemType::Trait)
        .and_then(|resolved_item: crate::resolve::ResolvedConcreteItem<'_>| match resolved_item {
            ResolvedConcreteItem::Trait(id) | ResolvedConcreteItem::SelfTrait(id) => Ok(id),
            _ => {
                Err(diagnostics
                    .report(trait_path.stable_ptr(db), SemanticDiagnosticKind::UnknownTrait))
            }
        });
    let attributes = impl_syntax.attributes(db).structurize(db);
    let resolver_data = Arc::new(resolver.data);

    Ok(TraitItemImplData {
        diagnostics: diagnostics.build(),
        concrete_trait,
        attributes,
        resolver_data,
    })
}

/// Implementation of [TraitSemantic::concrete_trait_impl_concrete_trait].
fn concrete_trait_impl_concrete_trait<'db>(
    db: &'db dyn Database,
    concrete_trait_impl_id: ConcreteTraitImplId<'db>,
) -> Maybe<ConcreteTraitId<'db>> {
    let concrete_trait_id = concrete_trait_impl_id.concrete_trait(db);
    GenericSubstitution::new(
        db.trait_generic_params(concrete_trait_id.trait_id(db))?,
        concrete_trait_id.generic_args(db),
    )
    .substitute(db, db.trait_impl_concrete_trait(concrete_trait_impl_id.trait_impl(db))?)
}

/// Query implementation of [TraitSemantic::concrete_trait_impl_concrete_trait].
#[salsa::tracked]
fn concrete_trait_impl_concrete_trait_tracked<'db>(
    db: &'db dyn Database,
    concrete_trait_impl_id: ConcreteTraitImplId<'db>,
) -> Maybe<ConcreteTraitId<'db>> {
    concrete_trait_impl_concrete_trait(db, concrete_trait_impl_id)
}

// === Trait function Declaration ===

/// Query implementation of [TraitSemantic::priv_trait_function_generic_params_data].
#[salsa::tracked(returns(ref))]
fn priv_trait_function_generic_params_data<'db>(
    db: &'db dyn Database,
    trait_function_id: TraitFunctionId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_id = trait_function_id.module_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_function_id.trait_id(db);
    let data = db.priv_trait_definition_data(trait_id)?;
    let function_syntax = &data.function_asts[&trait_function_id];
    let declaration = function_syntax.declaration(db);
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
        module_id,
        &declaration.generic_params(db),
    );
    let function_generic_params = resolver.inference().rewrite(function_generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData {
        diagnostics: diagnostics.build(),
        generic_params: function_generic_params,
        resolver_data,
    })
}

/// Query implementation of [PrivTraitSemantic::priv_trait_function_declaration_data].
#[salsa::tracked(returns(ref))]
fn priv_trait_function_declaration_data<'db>(
    db: &'db dyn Database,
    trait_function_id: TraitFunctionId<'db>,
) -> Maybe<FunctionDeclarationData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_function_id.trait_id(db);
    let data = db.priv_trait_definition_data(trait_id)?;
    let function_syntax = &data.function_asts[&trait_function_id];
    let declaration_syntax = function_syntax.declaration(db);
    let function_generic_params_data =
        db.priv_trait_function_generic_params_data(trait_function_id)?;
    let lookup_item_id = LookupItemId::TraitItem(TraitItemId::Function(trait_function_id));
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let mut resolver = Resolver::with_data(
        db,
        (*function_generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(function_generic_params_data.diagnostics.clone());
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
    inference.finalize(&mut diagnostics, function_syntax.stable_ptr(db).untyped());
    let signature = inference.rewrite(signature).no_err();

    validate_trait_function_signature(
        db,
        &mut diagnostics,
        trait_id,
        trait_function_id,
        &signature,
    );

    let attributes = function_syntax.attributes(db).structurize(db);

    let inline_config = get_inline_config(db, &mut diagnostics, &attributes)?;
    let (implicit_precedence, _) =
        get_implicit_precedence(db, &mut diagnostics, &mut resolver, &attributes);
    let resolver_data = Arc::new(resolver.data);

    Ok(FunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature,
        environment,
        attributes,
        resolver_data,
        inline_config,
        implicit_precedence,
    })
}

fn validate_trait_function_signature<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    trait_id: TraitId<'db>,
    function_id: TraitFunctionId<'db>,
    sig: &semantic::Signature<'db>,
) {
    for param in &sig.params {
        if param.mutability == Mutability::Mutable {
            diagnostics.report(
                param.stable_ptr(db).lookup(db).modifiers(db).stable_ptr(db),
                crate::diagnostic::SemanticDiagnosticKind::TraitParamMutable {
                    trait_id,
                    function_id,
                },
            );
        }
    }
}

// === Concrete Trait Function ===

/// Implementation of [TraitSemantic::concrete_trait_function_generic_params].
fn concrete_trait_function_generic_params<'db>(
    db: &'db dyn Database,
    concrete_trait_function_id: ConcreteTraitGenericFunctionId<'db>,
) -> Maybe<Vec<GenericParam<'db>>> {
    let concrete_trait_id = concrete_trait_function_id.concrete_trait(db);
    GenericSubstitution::new(
        db.trait_generic_params(concrete_trait_id.trait_id(db))?,
        concrete_trait_id.generic_args(db),
    )
    .substitute(
        db,
        db.trait_function_generic_params(concrete_trait_function_id.trait_function(db))?.to_vec(),
    )
}

/// Query implementation of [TraitSemantic::concrete_trait_function_generic_params].
#[salsa::tracked(returns(ref))]
fn concrete_trait_function_generic_params_tracked<'db>(
    db: &'db dyn Database,
    concrete_trait_function_id: ConcreteTraitGenericFunctionId<'db>,
) -> Maybe<Vec<GenericParam<'db>>> {
    concrete_trait_function_generic_params(db, concrete_trait_function_id)
}

/// Query implementation of [TraitSemantic::concrete_trait_function_signature].
#[salsa::tracked(returns(ref))]
fn concrete_trait_function_signature<'db>(
    db: &'db dyn Database,
    concrete_trait_function_id: ConcreteTraitGenericFunctionId<'db>,
) -> Maybe<semantic::Signature<'db>> {
    let concrete_trait_id = concrete_trait_function_id.concrete_trait(db);
    GenericSubstitution::new(
        db.trait_generic_params(concrete_trait_id.trait_id(db))?,
        concrete_trait_id.generic_args(db),
    )
    .substitute(
        db,
        db.trait_function_signature(concrete_trait_function_id.trait_function(db))?.clone(),
    )
}

// === Body ===

/// Query implementation of [TraitSemantic::priv_trait_function_body_data].
#[salsa::tracked(returns(ref))]
fn priv_trait_function_body_data<'db>(
    db: &'db dyn Database,
    trait_function_id: TraitFunctionId<'db>,
) -> Maybe<Option<FunctionBodyData<'db>>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let trait_id = trait_function_id.trait_id(db);
    let data = db.priv_trait_definition_data(trait_id)?;
    let function_syntax = &data.function_asts[&trait_function_id];
    // Compute declaration semantic.
    let trait_function_declaration_data =
        db.priv_trait_function_declaration_data(trait_function_id)?;
    let parent_resolver_data = &trait_function_declaration_data.resolver_data;
    let inference_id = InferenceId::LookupItemDefinition(LookupItemId::TraitItem(
        TraitItemId::Function(trait_function_id),
    ));
    let mut resolver =
        Resolver::with_data(db, (*parent_resolver_data).clone_with_inference_id(db, inference_id));
    resolver.trait_or_impl_ctx = TraitOrImplContext::Trait(trait_id);
    let environment = trait_function_declaration_data.environment.clone();

    let function_id: Result<crate::FunctionId<'db>, cairo_lang_diagnostics::DiagnosticAdded> =
        (|| {
            let generic_parameters = db.trait_generic_params(trait_id)?;
            let concrete_trait = ConcreteTraitLongId {
                trait_id,
                generic_args: generic_params_to_args(generic_parameters, db),
            }
            .intern(db);
            let generic_function = GenericFunctionId::Impl(ImplGenericFunctionId {
                impl_id: ImplLongId::SelfImpl(concrete_trait).intern(db),
                function: trait_function_id,
            });

            Ok(FunctionLongId::from_generic(db, generic_function)?.intern(db))
        })();
    // Compute body semantic expr.
    let mut ctx: ComputationContext<'db, '_> = ComputationContext::new(
        db,
        &mut diagnostics,
        &mut resolver,
        Some(&trait_function_declaration_data.signature),
        environment,
        ContextFunction::Function(function_id),
    );
    let function_body = match function_syntax.body(db) {
        ast::MaybeTraitFunctionBody::Some(expr_block) => expr_block,
        ast::MaybeTraitFunctionBody::None(_) => return Ok(None),
    };
    let return_type = trait_function_declaration_data.signature.return_type;
    let body_expr = compute_root_expr(&mut ctx, &function_body, return_type)?;
    let ComputationContext { arenas, .. } = ctx;

    let expr_lookup: UnorderedHashMap<_, _> =
        arenas.exprs.iter().map(|(id, expr)| (expr.stable_ptr(), id)).collect();
    let pattern_lookup: UnorderedHashMap<_, _> =
        arenas.patterns.iter().map(|(id, pattern)| (pattern.stable_ptr(), id)).collect();
    let resolver_data = Arc::new(resolver.data);
    Ok(Some(FunctionBodyData {
        diagnostics: diagnostics.build(),
        expr_lookup,
        pattern_lookup,
        resolver_data,
        body: FunctionBody { arenas, body_expr },
    }))
}

/// Trait for trait-related semantic queries.
pub trait TraitSemantic<'db>: Database {
    /// Returns the semantic declaration diagnostics of a trait.
    fn trait_semantic_declaration_diagnostics(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        self.priv_trait_declaration_data(trait_id)
            .map(|data| data.diagnostics.clone())
            .unwrap_or_default()
    }
    /// Returns the generic parameters of a trait.
    fn trait_generic_params(&'db self, trait_id: TraitId<'db>) -> Maybe<&'db [GenericParam<'db>]> {
        Ok(&self.trait_generic_params_data(trait_id, false)?.generic_params)
    }
    /// Returns the ids of the generic parameters of a trait.
    fn trait_generic_params_ids(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<&'db [GenericParamId<'db>]> {
        Ok(trait_generic_params_ids(self.as_dyn_database(), trait_id).maybe_as_ref()?)
    }
    /// Returns the attributes of a trait.
    fn trait_attributes(&'db self, trait_id: TraitId<'db>) -> Maybe<&'db [Attribute<'db>]> {
        Ok(&self.priv_trait_declaration_data(trait_id)?.attributes)
    }
    /// Returns the resolution resolved_items of a trait.
    fn trait_resolver_data(&'db self, trait_id: TraitId<'db>) -> Maybe<Arc<ResolverData<'db>>> {
        Ok(self.priv_trait_declaration_data(trait_id)?.resolver_data.clone())
    }
    /// Returns the semantic definition diagnostics of a trait.
    fn trait_semantic_definition_diagnostics(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        trait_semantic_definition_diagnostics_tracked(self.as_dyn_database(), trait_id)
    }
    /// Returns the names of all the non default implemented items of a trait.
    fn trait_required_item_names(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<&'db OrderedHashSet<SmolStrId<'db>>> {
        trait_required_item_names(self.as_dyn_database(), trait_id).maybe_as_ref()
    }
    /// Returns the item of the trait, by the given `name`, if exists.
    fn trait_item_by_name(
        &'db self,
        trait_id: TraitId<'db>,
        name: SmolStrId<'db>,
    ) -> Maybe<Option<TraitItemId<'db>>> {
        Ok(self
            .priv_trait_definition_data(trait_id)?
            .item_id_by_name
            .get(&name)
            .map(|info| info.id))
    }
    /// Returns the metadata for a trait item, by the given `name`, if exists.
    fn trait_item_info_by_name(
        &'db self,
        trait_id: TraitId<'db>,
        name: SmolStrId<'db>,
    ) -> Maybe<Option<TraitItemInfo<'db>>> {
        Ok(self.priv_trait_definition_data(trait_id)?.item_id_by_name.get(&name).cloned())
    }
    /// Returns all the items used within the trait.
    fn trait_all_used_uses(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<&'db OrderedHashSet<UseId<'db>>> {
        trait_all_used_uses(self.as_dyn_database(), trait_id).maybe_as_ref()
    }
    /// Returns the functions of a trait.
    fn trait_functions(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<&'db OrderedHashMap<SmolStrId<'db>, TraitFunctionId<'db>>> {
        trait_functions(self.as_dyn_database(), trait_id).maybe_as_ref()
    }
    /// Returns the function with the given name of the given trait, if exists.
    fn trait_function_by_name(
        &'db self,
        trait_id: TraitId<'db>,
        name: SmolStrId<'db>,
    ) -> Maybe<Option<TraitFunctionId<'db>>> {
        Ok(self.trait_functions(trait_id)?.get(&name).copied())
    }
    /// Returns the types of a trait.
    fn trait_types(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<&'db OrderedHashMap<SmolStrId<'db>, TraitTypeId<'db>>> {
        trait_types(self.as_dyn_database(), trait_id).maybe_as_ref()
    }
    /// Returns the item type with the given name of the given trait, if exists.
    fn trait_type_by_name(
        &'db self,
        trait_id: TraitId<'db>,
        name: SmolStrId<'db>,
    ) -> Maybe<Option<TraitTypeId<'db>>> {
        Ok(self.trait_types(trait_id)?.get(&name).copied())
    }
    /// Returns the constants of a trait.
    fn trait_constants(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<&'db OrderedHashMap<SmolStrId<'db>, TraitConstantId<'db>>> {
        trait_constants(self.as_dyn_database(), trait_id).maybe_as_ref()
    }
    /// Returns the item constants with the given name of the given trait, if exists.
    fn trait_constant_by_name(
        &'db self,
        trait_id: TraitId<'db>,
        name: SmolStrId<'db>,
    ) -> Maybe<Option<TraitConstantId<'db>>> {
        Ok(self.trait_constants(trait_id)?.get(&name).copied())
    }
    /// Returns the constants of a trait.
    fn trait_impls(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<&'db OrderedHashMap<SmolStrId<'db>, TraitImplId<'db>>> {
        trait_impls(self.as_dyn_database(), trait_id).maybe_as_ref()
    }
    /// Returns the item impls with the given name of the given trait, if exists.
    fn trait_impl_by_name(
        &'db self,
        trait_id: TraitId<'db>,
        name: SmolStrId<'db>,
    ) -> Maybe<Option<TraitImplId<'db>>> {
        Ok(self.trait_impls(trait_id)?.get(&name).copied())
    }

    // Trait type.
    // ================
    /// Returns the generic params of a trait type.
    fn trait_type_generic_params(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<&'db [GenericParam<'db>]> {
        Ok(&self.priv_trait_type_generic_params_data(trait_type_id)?.generic_params)
    }
    /// Returns the attributes of a trait type.
    fn trait_type_attributes(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<&'db [Attribute<'db>]> {
        Ok(&self.priv_trait_type_data(trait_type_id).maybe_as_ref()?.attributes)
    }
    /// Returns the resolution resolved_items of a trait type.
    fn trait_type_resolver_data(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        Ok(self.priv_trait_type_data(trait_type_id)?.resolver_data.clone())
    }

    // Trait constants.
    // ================
    /// Returns the attributes of a trait constants.
    fn trait_constant_attributes(
        &'db self,
        id: TraitConstantId<'db>,
    ) -> Maybe<Vec<Attribute<'db>>> {
        Ok(self.priv_trait_constant_data(id)?.attributes.clone())
    }
    /// Returns the type of a trait constant.
    fn trait_constant_type(&'db self, id: TraitConstantId<'db>) -> Maybe<TypeId<'db>> {
        Ok(self.priv_trait_constant_data(id)?.ty)
    }
    /// Returns the resolution resolved_items of a trait constants.
    fn trait_constant_resolver_data(
        &'db self,
        id: TraitConstantId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        Ok(self.priv_trait_constant_data(id)?.resolver_data.clone())
    }
    /// Returns the type of a trait constant.
    fn concrete_trait_constant_type(
        &'db self,
        concrete_trait_constant_id: ConcreteTraitConstantId<'db>,
    ) -> Maybe<TypeId<'db>> {
        concrete_trait_constant_type_tracked(self.as_dyn_database(), concrete_trait_constant_id)
    }

    // Trait impls.
    // ================
    /// Returns the concrete trait of a trait impl.
    fn trait_impl_concrete_trait(&'db self, id: TraitImplId<'db>) -> Maybe<ConcreteTraitId<'db>> {
        self.priv_trait_impl_data(id)?.concrete_trait
    }
    /// Returns the resolution resolved_items of a trait impls.
    fn trait_impl_resolver_data(&'db self, id: TraitImplId<'db>) -> Maybe<Arc<ResolverData<'db>>> {
        Ok(self.priv_trait_impl_data(id)?.resolver_data.clone())
    }
    /// Returns the concrete trait of a concrete trait impl.
    fn concrete_trait_impl_concrete_trait(
        &'db self,
        concrete_trait_impl_id: ConcreteTraitImplId<'db>,
    ) -> Maybe<ConcreteTraitId<'db>> {
        concrete_trait_impl_concrete_trait_tracked(self.as_dyn_database(), concrete_trait_impl_id)
    }

    // Trait function.
    // ================
    /// Returns the semantic diagnostics of a trait function.
    fn trait_function_declaration_diagnostics(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        self.priv_trait_function_declaration_data(trait_function_id)
            .map(|data| data.diagnostics.clone())
            .unwrap_or_default()
    }
    /// Returns the signature of a trait function.
    fn trait_function_signature(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<&'db semantic::Signature<'db>> {
        Ok(&self.priv_trait_function_declaration_data(trait_function_id)?.signature)
    }
    /// Returns the generic params of a trait function.
    fn trait_function_generic_params(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<&'db [GenericParam<'db>]> {
        Ok(&self.priv_trait_function_generic_params_data(trait_function_id)?.generic_params)
    }
    /// Returns the attributes of a trait function.
    fn trait_function_attributes(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<&'db [Attribute<'db>]> {
        Ok(&self.priv_trait_function_declaration_data(trait_function_id)?.attributes)
    }
    /// Returns the resolver data of a trait function.
    fn trait_function_resolver_data(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        Ok(self.priv_trait_function_declaration_data(trait_function_id)?.resolver_data.clone())
    }
    /// Returns the inline configuration of a trait function's declaration.
    fn trait_function_declaration_inline_config(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<InlineConfiguration<'db>> {
        Ok(self.priv_trait_function_declaration_data(trait_function_id)?.inline_config.clone())
    }
    /// Returns the implicits precedence of a trait function.
    fn trait_function_declaration_implicit_precedence(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<&'db ImplicitPrecedence<'db>> {
        Ok(&self.priv_trait_function_declaration_data(trait_function_id)?.implicit_precedence)
    }
    /// Returns the semantic diagnostics of a trait function definition (declaration + body).
    fn trait_function_body_diagnostics(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        self.priv_trait_function_body_data(trait_function_id)
            .ok()
            .flatten()
            .map(|data| data.diagnostics.clone())
            .unwrap_or_default()
    }
    /// Returns the body of a trait function, if any.
    fn trait_function_body(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Option<&'db FunctionBody<'db>>> {
        Ok(self.priv_trait_function_body_data(trait_function_id)?.map(|data| &data.body))
    }
    /// Returns the resolution resolved_items of a trait function.
    fn trait_function_body_resolver_data(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Option<Arc<ResolverData<'db>>>> {
        self.priv_trait_function_body_data(trait_function_id)
            .map(|x| x.map(|y| y.resolver_data.clone()))
    }
    /// Private query to compute data about a trait function definition (declaration + body)
    fn priv_trait_function_body_data(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<Option<&'db FunctionBodyData<'db>>> {
        priv_trait_function_body_data(self.as_dyn_database(), trait_function_id)
            .maybe_as_ref()
            .map(Option::as_ref)
    }

    // Concrete Trait function.
    // ========================
    /// Returns the generic params of a concrete trait function.
    fn concrete_trait_function_generic_params(
        &'db self,
        concrete_trait_function_id: ConcreteTraitGenericFunctionId<'db>,
    ) -> Maybe<&'db [GenericParam<'db>]> {
        Ok(concrete_trait_function_generic_params_tracked(
            self.as_dyn_database(),
            concrete_trait_function_id,
        )
        .maybe_as_ref()?)
    }
    /// Returns the signature of a concrete trait function.
    fn concrete_trait_function_signature(
        &'db self,
        concrete_trait_function_id: ConcreteTraitGenericFunctionId<'db>,
    ) -> Maybe<&'db semantic::Signature<'db>> {
        concrete_trait_function_signature(self.as_dyn_database(), concrete_trait_function_id)
            .maybe_as_ref()
    }
}
impl<'db, T: Database + ?Sized> TraitSemantic<'db> for T {}

/// Trait for private trait-related semantic queries.
trait PrivTraitSemantic<'db>: Database {
    /// Returns the generic parameters data of a trait.
    fn trait_generic_params_data(
        &'db self,
        trait_id: TraitId<'db>,
        in_cycle: bool,
    ) -> Maybe<&'db GenericParamsData<'db>> {
        trait_generic_params_data_tracked(self.as_dyn_database(), trait_id, in_cycle).maybe_as_ref()
    }
    /// Private query to compute declaration data about a trait.
    fn priv_trait_declaration_data(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<&'db TraitDeclarationData<'db>> {
        priv_trait_declaration_data(self.as_dyn_database(), trait_id).maybe_as_ref()
    }
    /// Private query to compute definition data about a trait.
    fn priv_trait_definition_data(
        &'db self,
        trait_id: TraitId<'db>,
    ) -> Maybe<&'db TraitDefinitionData<'db>> {
        priv_trait_definition_data(self.as_dyn_database(), trait_id).maybe_as_ref()
    }
    /// Returns the semantic diagnostics of a trait type.
    fn trait_type_diagnostics(
        &'db self,
        id: TraitTypeId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        self.priv_trait_type_data(id).map(|data| data.diagnostics.clone()).unwrap_or_default()
    }
    /// Returns the semantic diagnostics of a trait impls.
    fn trait_impl_diagnostics(
        &'db self,
        id: TraitImplId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        self.priv_trait_impl_data(id).map(|data| data.diagnostics.clone()).unwrap_or_default()
    }
    /// Private query to compute the generic params data of a trait type.
    fn priv_trait_type_generic_params_data(
        &'db self,
        trait_type_id: TraitTypeId<'db>,
    ) -> Maybe<&'db GenericParamsData<'db>> {
        priv_trait_type_generic_params_data(self.as_dyn_database(), trait_type_id).maybe_as_ref()
    }
    /// Private query to compute data about a trait type.
    fn priv_trait_type_data(
        &'db self,
        type_id: TraitTypeId<'db>,
    ) -> Maybe<&'db TraitItemTypeData<'db>> {
        priv_trait_type_data(self.as_dyn_database(), type_id).maybe_as_ref()
    }
    /// Returns the semantic diagnostics of a trait type.
    fn trait_constant_diagnostics(
        &'db self,
        id: TraitConstantId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        self.priv_trait_constant_data(id).map(|data| data.diagnostics.clone()).unwrap_or_default()
    }
    /// Private query to compute data about a trait constant.
    fn priv_trait_constant_data(
        &'db self,
        trait_constant: TraitConstantId<'db>,
    ) -> Maybe<&'db TraitItemConstantData<'db>> {
        priv_trait_constant_data(self.as_dyn_database(), trait_constant).maybe_as_ref()
    }
    /// Private query to compute data about a trait impl.
    fn priv_trait_impl_data(
        &'db self,
        trait_impl: TraitImplId<'db>,
    ) -> Maybe<&'db TraitItemImplData<'db>> {
        priv_trait_impl_data(self.as_dyn_database(), trait_impl).maybe_as_ref()
    }
    /// Returns the generic params data of a trait function.
    fn priv_trait_function_generic_params_data(
        &'db self,
        trait_function_id: TraitFunctionId<'db>,
    ) -> Maybe<&'db GenericParamsData<'db>> {
        priv_trait_function_generic_params_data(self.as_dyn_database(), trait_function_id)
            .maybe_as_ref()
    }
    /// Private query to compute data about a trait function declaration.
    fn priv_trait_function_declaration_data(
        &'db self,
        function_id: TraitFunctionId<'db>,
    ) -> Maybe<&'db FunctionDeclarationData<'db>> {
        priv_trait_function_declaration_data(self.as_dyn_database(), function_id).maybe_as_ref()
    }
}
impl<'db, T: Database + ?Sized> PrivTraitSemantic<'db> for T {}
