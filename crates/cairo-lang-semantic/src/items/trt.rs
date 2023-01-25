use std::sync::Arc;

use cairo_lang_defs::ids::{
    FunctionSignatureId, GenericParamId, LanguageElementId, TraitFunctionId, TraitFunctionLongId,
    TraitId,
};
use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::define_short_id;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{chain, Itertools};
use smol_str::SmolStr;

use super::attribute::{ast_attributes_to_semantic, Attribute};
use super::generics::semantic_generic_params;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::Environment;
use crate::resolve_path::{ResolvedLookback, Resolver};
use crate::{semantic, GenericArgumentId, Mutability, SemanticDiagnostic};

#[cfg(test)]
#[path = "trt_test.rs"]
mod test;

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteTraitLongId {
    pub trait_id: TraitId,
    pub generic_args: Vec<GenericArgumentId>,
}
define_short_id!(ConcreteTraitId, ConcreteTraitLongId, SemanticGroup, lookup_intern_concrete_trait);
impl ConcreteTraitId {
    pub fn trait_id(&self, db: &dyn SemanticGroup) -> TraitId {
        db.lookup_intern_concrete_trait(*self).trait_id
    }
}

/// The ID of a generic function in a concrete trait.
#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteTraitFunctionLongId {
    // Note the members are private to prevent direct call to the constructor.
    concrete_trait_id: ConcreteTraitId,
    function_id: TraitFunctionId,
}
impl ConcreteTraitFunctionLongId {
    pub fn new(
        db: &dyn SemanticGroup,
        concrete_trait_id: ConcreteTraitId,
        function_id: TraitFunctionId,
    ) -> Self {
        assert_eq!(
            concrete_trait_id.trait_id(db),
            function_id.trait_id(db.upcast()),
            "Concrete trait a trait function must belong to the same generic trait."
        );
        Self { concrete_trait_id, function_id }
    }
}
define_short_id!(
    ConcreteTraitFunctionId,
    ConcreteTraitFunctionLongId,
    SemanticGroup,
    lookup_intern_concrete_trait_function
);
impl ConcreteTraitFunctionId {
    pub fn function_id(&self, db: &dyn SemanticGroup) -> TraitFunctionId {
        db.lookup_intern_concrete_trait_function(*self).function_id
    }

    pub fn concrete_trait_id(&self, db: &dyn SemanticGroup) -> ConcreteTraitId {
        db.lookup_intern_concrete_trait_function(*self).concrete_trait_id
    }
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TraitData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<GenericParamId>,
    attributes: Vec<Attribute>,
    function_asts: OrderedHashMap<TraitFunctionId, ast::TraitItemFunction>,
}

/// Query implementation of [crate::db::SemanticGroup::trait_semantic_diagnostics].
pub fn trait_semantic_diagnostics(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Diagnostics<SemanticDiagnostic> {
    let mut diagnostics = DiagnosticsBuilder::default();

    let Ok(data) = db.priv_trait_semantic_data(trait_id) else {
        return Diagnostics::default();
    };

    diagnostics.extend(data.diagnostics);
    for trait_function_id in data.function_asts.keys() {
        diagnostics.extend(db.trait_function_diagnostics(*trait_function_id));
    }

    diagnostics.build()
}

/// Query implementation of [crate::db::SemanticGroup::trait_generic_params].
pub fn trait_generic_params(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<Vec<GenericParamId>> {
    Ok(db.priv_trait_semantic_data(trait_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::trait_attributes].
pub fn trait_attributes(db: &dyn SemanticGroup, trait_id: TraitId) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_trait_semantic_data(trait_id)?.attributes)
}

/// Query implementation of [crate::db::SemanticGroup::trait_functions].
pub fn trait_functions(
    db: &dyn SemanticGroup,
    trait_id: TraitId,
) -> Maybe<OrderedHashMap<SmolStr, TraitFunctionId>> {
    Ok(db
        .priv_trait_semantic_data(trait_id)?
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

/// Query implementation of [crate::db::SemanticGroup::priv_trait_semantic_data].
pub fn priv_trait_semantic_data(db: &dyn SemanticGroup, trait_id: TraitId) -> Maybe<TraitData> {
    let syntax_db = db.upcast();
    let module_file_id = trait_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let module_traits = db.module_traits(module_file_id.0)?;
    let trait_ast = module_traits.get(&trait_id).to_maybe()?;

    // Generic params.
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_file_id,
        &trait_ast.generic_params(syntax_db),
    );

    let attributes = ast_attributes_to_semantic(syntax_db, trait_ast.attributes(syntax_db));
    let mut function_asts = OrderedHashMap::default();
    if let ast::MaybeTraitBody::Some(body) = trait_ast.body(syntax_db) {
        for item in body.items(syntax_db).elements(syntax_db) {
            match item {
                ast::TraitItem::Function(func) => {
                    function_asts.insert(
                        db.intern_trait_function(TraitFunctionLongId(
                            module_file_id,
                            func.stable_ptr(),
                        )),
                        func,
                    );
                }
            }
        }
    }

    Ok(TraitData { diagnostics: diagnostics.build(), generic_params, attributes, function_asts })
}

// Trait function.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TraitFunctionData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    signature: semantic::Signature,
    generic_params: Vec<GenericParamId>,
    attributes: Vec<Attribute>,
    resolved_lookback: Arc<ResolvedLookback>,
}

// Selectors.
/// Query implementation of [crate::db::SemanticGroup::trait_function_diagnostics].
pub fn trait_function_diagnostics(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_trait_function_data(trait_function_id).map(|data| data.diagnostics).unwrap_or_default()
}
/// Query implementation of [crate::db::SemanticGroup::trait_function_signature].
pub fn trait_function_signature(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<semantic::Signature> {
    Ok(db.priv_trait_function_data(trait_function_id)?.signature)
}
/// Query implementation of [crate::db::SemanticGroup::trait_function_attributes].
pub fn trait_function_attributes(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<Vec<Attribute>> {
    Ok(db.priv_trait_function_data(trait_function_id)?.attributes)
}
/// Query implementation of [crate::db::SemanticGroup::trait_function_generic_params].
pub fn trait_function_generic_params(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<Vec<GenericParamId>> {
    Ok(db.priv_trait_function_data(trait_function_id)?.generic_params)
}
/// Query implementation of [crate::db::SemanticGroup::trait_function_resolved_lookback].
pub fn trait_function_resolved_lookback(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db.priv_trait_function_data(trait_function_id)?.resolved_lookback)
}

/// Query implementation of [crate::db::SemanticGroup::priv_trait_function_data].
pub fn priv_trait_function_data(
    db: &dyn SemanticGroup,
    trait_function_id: TraitFunctionId,
) -> Maybe<TraitFunctionData> {
    let syntax_db = db.upcast();
    let module_file_id = trait_function_id.module_file_id(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let trait_id = trait_function_id.trait_id(db.upcast());
    let data = db.priv_trait_semantic_data(trait_id)?;
    let function_syntax = &data.function_asts[trait_function_id];
    let declaration = function_syntax.declaration(syntax_db);
    let function_generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_file_id,
        &declaration.generic_params(syntax_db),
    );
    let trait_generic_params = db.trait_generic_params(trait_id)?;
    let generic_params =
        chain!(trait_generic_params, function_generic_params.clone()).collect_vec();
    let mut resolver = Resolver::new(db, module_file_id, &generic_params);

    let signature_syntax = declaration.signature(syntax_db);
    let mut environment = Environment::default();
    let signature = semantic::Signature::from_ast(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        FunctionSignatureId::Trait(trait_function_id),
        &mut environment,
    );

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
            crate::diagnostic::SemanticDiagnosticKind::TraitFunctionWithBody {
                trait_id,
                function_id: trait_function_id,
            },
        );
    }

    let attributes = ast_attributes_to_semantic(syntax_db, function_syntax.attributes(syntax_db));
    let resolved_lookback = Arc::new(resolver.lookback);

    Ok(TraitFunctionData {
        diagnostics: diagnostics.build(),
        signature,
        generic_params: function_generic_params,
        attributes,
        resolved_lookback,
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
