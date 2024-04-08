use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{
    FunctionTitleId, LanguageElementId, LookupItemId, ModuleItemId, NamedLanguageElementId,
    NamedLanguageElementLongId, TopLevelLanguageElementId, TraitContext, TraitFunctionId,
    TraitFunctionLongId, TraitId, TraitItemId, TraitOrImplContext, TraitTypeId, TraitTypeLongId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe, ToMaybe};
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::OptionWrappedGenericParamListHelper;
use cairo_lang_syntax::node::{ast, Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::define_short_id;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::chain;
use smol_str::SmolStr;

use super::function_with_body::{get_implicit_precedence, get_inline_config, FunctionBodyData};
use super::functions::{FunctionDeclarationData, ImplicitPrecedence, InlineConfiguration};
use super::generics::{semantic_generic_params, GenericParamsData};
use super::imp::{GenericsHeadFilter, TraitFilter};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::{self, *};
use crate::diagnostic::{report_unsupported_trait_item, SemanticDiagnostics};
use crate::expr::compute::{compute_root_expr, ComputationContext, Environment};
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::expr::inference::InferenceId;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter, SubstitutionRewriter};
use crate::{
    semantic, semantic_object_for_id, FunctionBody, GenericArgumentId, GenericParam, Mutability,
    SemanticDiagnostic, TypeId,
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
        if !self.generic_args.is_empty() {
            write!(f, "::<")?;
            for (i, arg) in self.generic_args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{:?}", arg.debug(db))?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

define_short_id!(ConcreteTraitId, ConcreteTraitLongId, SemanticGroup, lookup_intern_concrete_trait);
semantic_object_for_id!(
    ConcreteTraitId,
    lookup_intern_concrete_trait,
    intern_concrete_trait,
    ConcreteTraitLongId
);
impl ConcreteTraitId {
    pub fn trait_id(&self, db: &dyn SemanticGroup) -> TraitId {
        db.lookup_intern_concrete_trait(*self).trait_id
    }
    pub fn generic_args(&self, db: &dyn SemanticGroup) -> Vec<GenericArgumentId> {
        db.lookup_intern_concrete_trait(*self).generic_args
    }
    pub fn name(&self, db: &dyn SemanticGroup) -> SmolStr {
        self.trait_id(db).name(db.upcast())
    }
    pub fn full_path(&self, db: &dyn SemanticGroup) -> String {
        self.trait_id(db).full_path(db.upcast())
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
            "Concrete trait a trait function must belong to the same generic trait."
        );
        Self { concrete_trait, trait_function }
    }
}
define_short_id!(
    ConcreteTraitGenericFunctionId,
    ConcreteTraitGenericFunctionLongId,
    SemanticGroup,
    lookup_intern_concrete_trait_function
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
        db.intern_concrete_trait_function(ConcreteTraitGenericFunctionLongId::new(
            db,
            concrete_trait,
            trait_function,
        ))
    }

    pub fn trait_function(&self, db: &dyn SemanticGroup) -> TraitFunctionId {
        db.lookup_intern_concrete_trait_function(*self).trait_function
    }

    pub fn concrete_trait(&self, db: &dyn SemanticGroup) -> ConcreteTraitId {
        db.lookup_intern_concrete_trait_function(*self).concrete_trait
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
    lookup_intern_concrete_trait_type
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
        db.intern_concrete_trait_type(ConcreteTraitTypeLongId::new(db, concrete_trait, trait_type))
    }

    pub fn trait_type(&self, db: &dyn SemanticGroup) -> TraitTypeId {
        db.lookup_intern_concrete_trait_type(*self).trait_type
    }

    pub fn concrete_trait(&self, db: &dyn SemanticGroup) -> ConcreteTraitId {
        db.lookup_intern_concrete_trait_type(*self).concrete_trait
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
    Ok(db.trait_generic_params_data(trait_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::trait_generic_params_data].
pub fn trait_generic_params_data(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<GenericParamsData> {
    let syntax_db: &dyn SyntaxGroup = db.upcast();
    let module_file_id = trait_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let trait_ast = db.module_trait_by_id(trait_id)?.to_maybe()?;

    // Generic params.
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id)));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &trait_ast.generic_params(syntax_db),
    )?;

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, trait_ast.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
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
    let module_file_id = trait_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let trait_ast = db.module_trait_by_id(trait_id)?.to_maybe()?;

    // Generic params.
    let generic_params_data = db.trait_generic_params_data(trait_id)?;
    let generic_params = generic_params_data.generic_params;
    let inference_id =
        InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id)));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.diagnostics.extend(generic_params_data.diagnostics);

    let attributes = trait_ast.attributes(syntax_db).structurize(syntax_db);

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, trait_ast.stable_ptr().untyped());

    let generic_params = inference.rewrite(generic_params).no_err();

    let mut resolver_data = resolver.data;
    resolver_data.trait_or_impl_ctx = TraitOrImplContext::Trait(TraitContext { trait_id });
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

    /// Mapping of item names to their IDs. All the IDs should appear in one of the AST maps above.
    item_id_by_name: Arc<OrderedHashMap<SmolStr, TraitItemId>>,
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
    }
    for trait_type_id in data.item_type_asts.keys() {
        diagnostics.extend(db.trait_type_diagnostics(*trait_type_id));
    }

    diagnostics.build()
}

/// Query implementation of [crate::db::SemanticGroup::trait_item_names].
pub fn trait_item_names(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<OrderedHashSet<SmolStr>> {
    let trait_functions = db.trait_functions(trait_id)?;
    let trait_types = db.trait_types(trait_id)?;
    Ok(chain!(trait_functions.keys(), trait_types.keys()).cloned().collect())
}

/// Query implementation of [crate::db::SemanticGroup::trait_item_by_name].
pub fn trait_item_by_name(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
    name: SmolStr,
) -> Maybe<Option<TraitItemId>> {
    Ok(db.priv_trait_definition_data(trait_id)?.item_id_by_name.get(&name).cloned())
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
            let function_long_id = db.lookup_intern_trait_function(*function_id);
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
            let type_long_id = db.lookup_intern_trait_type(*type_id);
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

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_trait_definition_data].
pub fn priv_trait_definition_data(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<TraitDefinitionData> {
    let syntax_db: &dyn SyntaxGroup = db.upcast();

    let module_file_id = trait_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);

    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let trait_ast = db.module_trait_by_id(trait_id)?.to_maybe()?;

    let mut function_asts = OrderedHashMap::default();
    let mut item_type_asts = OrderedHashMap::default();
    let mut item_id_by_name = OrderedHashMap::default();

    if let ast::MaybeTraitBody::Some(body) = trait_ast.body(syntax_db) {
        for item in body.items(syntax_db).elements(syntax_db) {
            match item {
                ast::TraitItem::Function(func) => {
                    let trait_func_id = db.intern_trait_function(TraitFunctionLongId(
                        module_file_id,
                        func.stable_ptr(),
                    ));
                    let name_node = func.declaration(syntax_db).name(syntax_db);
                    let name = name_node.text(syntax_db);
                    if item_id_by_name
                        .insert(name.clone(), TraitItemId::Function(trait_func_id))
                        .is_some()
                    {
                        diagnostics.report_by_ptr(
                            name_node.stable_ptr().untyped(),
                            SemanticDiagnosticKind::NameDefinedMultipleTimes { name },
                        );
                    }
                    function_asts.insert(trait_func_id, func);
                }
                ast::TraitItem::Type(ty) => {
                    let trait_type_id =
                        db.intern_trait_type(TraitTypeLongId(module_file_id, ty.stable_ptr()));
                    let name_node = ty.name(syntax_db);
                    let name = name_node.text(syntax_db);
                    if item_id_by_name
                        .insert(name.clone(), TraitItemId::Type(trait_type_id))
                        .is_some()
                    {
                        diagnostics.report_by_ptr(
                            name_node.stable_ptr().untyped(),
                            SemanticDiagnosticKind::NameDefinedMultipleTimes { name },
                        );
                    }
                    item_type_asts.insert(trait_type_id, ty);
                }
                ast::TraitItem::Constant(constant) => report_unsupported_trait_item(
                    &mut diagnostics,
                    constant.const_kw(syntax_db),
                    "Constant",
                ),
                ast::TraitItem::Impl(imp) => {
                    report_unsupported_trait_item(&mut diagnostics, imp.impl_kw(syntax_db), "Impl")
                }
                ast::TraitItem::Missing(_) => {}
            }
        }
    }

    Ok(TraitDefinitionData {
        diagnostics: diagnostics.build(),
        function_asts,
        item_type_asts,
        item_id_by_name: item_id_by_name.into(),
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
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
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
    )?;
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
    let module_file_id = trait_type_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
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
    diagnostics.diagnostics.extend(type_generic_params_data.diagnostics);

    let attributes = type_syntax.attributes(syntax_db).structurize(syntax_db);
    let resolver_data = Arc::new(resolver.data);

    Ok(TraitItemTypeData {
        diagnostics: diagnostics.build(),
        generic_params: type_generic_params,
        attributes,
        resolver_data,
    })
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
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
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
    )?;
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
    let module_file_id = trait_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let trait_id = trait_function_id.trait_id(db.upcast());
    let data = db.priv_trait_definition_data(trait_id)?;
    let function_syntax = &data.function_asts[&trait_function_id];
    let declaration = function_syntax.declaration(syntax_db);
    let function_generic_params_data =
        db.priv_trait_function_generic_params_data(trait_function_id)?;
    let function_generic_params = function_generic_params_data.generic_params;
    let lookup_item_id = LookupItemId::TraitItem(TraitItemId::Function(trait_function_id));
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let mut resolver = Resolver::with_data(
        db,
        (*function_generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.diagnostics.extend(function_generic_params_data.diagnostics);

    let signature_syntax = declaration.signature(syntax_db);
    let mut environment = Environment::from_lookup_item_id(db, lookup_item_id, &mut diagnostics);
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        FunctionTitleId::Trait(trait_function_id),
        &mut environment,
    );

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, function_syntax.stable_ptr().untyped());

    validate_trait_function_signature(
        db,
        &mut diagnostics,
        trait_id,
        trait_function_id,
        &signature,
        &signature_syntax,
    );
    // Validate trait function body is empty.
    if matches!(function_syntax.body(syntax_db), ast::MaybeTraitFunctionBody::Some(_)) {
        diagnostics.report(
            &function_syntax.body(syntax_db),
            TraitFunctionWithBody { trait_id, function_id: trait_function_id },
        );
    }

    let attributes = function_syntax.attributes(syntax_db).structurize(syntax_db);
    let resolver_data = Arc::new(resolver.data);

    let inline_config = get_inline_config(db, &mut diagnostics, &attributes)?;
    let (implicit_precedence, _) = get_implicit_precedence(db, &mut diagnostics, &attributes)?;

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
    let substitution = GenericSubstitution::new(
        &db.trait_generic_params(concrete_trait_id.trait_id(db))?,
        &concrete_trait_id.generic_args(db),
    );
    let generic_params =
        db.trait_function_generic_params(concrete_trait_function_id.trait_function(db))?;
    let mut rewriter = SubstitutionRewriter { db, substitution: &substitution };
    rewriter.rewrite(generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::concrete_trait_function_signature].
pub fn concrete_trait_function_signature(
    db: &dyn SemanticGroup,
    concrete_trait_function_id: ConcreteTraitGenericFunctionId,
) -> Maybe<semantic::Signature> {
    let concrete_trait_id = concrete_trait_function_id.concrete_trait(db);
    let substitution = GenericSubstitution::new(
        &db.trait_generic_params(concrete_trait_id.trait_id(db))?,
        &concrete_trait_id.generic_args(db),
    );
    let generic_signature =
        db.trait_function_signature(concrete_trait_function_id.trait_function(db))?;
    SubstitutionRewriter { db, substitution: &substitution }.rewrite(generic_signature)
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
    let module_file_id = trait_function_id.module_file_id(defs_db);
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast())?);
    let trait_id = trait_function_id.trait_id(defs_db);
    let data = db.priv_trait_definition_data(trait_id)?;
    let function_syntax = &data.function_asts[&trait_function_id];
    // Compute declaration semantic.
    let trait_function_declaration_data =
        db.priv_trait_function_declaration_data(trait_function_id)?;
    let parent_resolver_data = db.trait_resolver_data(trait_id)?;
    let inference_id = InferenceId::LookupItemDefinition(LookupItemId::TraitItem(
        TraitItemId::Function(trait_function_id),
    ));
    let resolver =
        Resolver::with_data(db, (*parent_resolver_data).clone_with_inference_id(db, inference_id));
    let environment = trait_function_declaration_data.environment;

    // Compute body semantic expr.
    let mut ctx = ComputationContext::new(
        db,
        &mut diagnostics,
        None,
        resolver,
        Some(&trait_function_declaration_data.signature),
        environment,
    );
    let function_body = match function_syntax.body(db.upcast()) {
        ast::MaybeTraitFunctionBody::Some(expr_block) => expr_block,
        ast::MaybeTraitFunctionBody::None(_) => return Ok(None),
    };
    let return_type = trait_function_declaration_data.signature.return_type;
    let body_expr = compute_root_expr(&mut ctx, &function_body, return_type)?;
    let ComputationContext { exprs, patterns, statements, resolver, .. } = ctx;

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
        body: Arc::new(FunctionBody { exprs, patterns, statements, body_expr }),
    }))
}
