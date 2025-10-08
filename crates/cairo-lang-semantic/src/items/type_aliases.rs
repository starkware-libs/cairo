use std::sync::Arc;

use cairo_lang_defs::ids::{LookupItemId, ModuleId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use salsa::Database;

use super::generics::{GenericParamsData, semantic_generic_params};
use crate::TypeId;
use crate::diagnostic::SemanticDiagnosticKind::TypeAliasCycle;
use crate::diagnostic::{SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::types::resolve_type;

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
pub struct TypeAliasData<'db> {
    pub resolved_type: Maybe<TypeId<'db>>,
    pub attributes: Vec<Attribute<'db>>,
    pub resolver_data: Arc<ResolverData<'db>>,
}

/// Computes data about the generic parameters of a type-alias item.
pub fn type_alias_generic_params_data_helper<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    type_alias_ast: &ast::ItemTypeAlias<'db>,
    lookup_item_id: LookupItemId<'db>,
    parent_resolver_data: Option<Arc<ResolverData<'db>>>,
) -> Maybe<GenericParamsData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    let inference_id = InferenceId::LookupItemGenerics(lookup_item_id);

    let mut resolver = match parent_resolver_data {
        Some(parent_resolver_data) => {
            Resolver::with_data(db, parent_resolver_data.clone_with_inference_id(db, inference_id))
        }
        None => Resolver::new(db, module_id, inference_id),
    };
    resolver.set_feature_config(&lookup_item_id, type_alias_ast, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_id,
        &type_alias_ast.generic_params(db),
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, type_alias_ast.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Computes data about a type-alias item.
pub fn type_alias_semantic_data_helper<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    type_alias_ast: &ast::ItemTypeAlias<'db>,
    lookup_item_id: LookupItemId<'db>,
    generic_params_data: GenericParamsData<'db>,
) -> Maybe<TypeAliasData<'db>> {
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);

    let ty = resolve_type(db, diagnostics, &mut resolver, &type_alias_ast.ty(db));

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(diagnostics, type_alias_ast.stable_ptr(db).untyped());

    let ty = inference.rewrite(ty).no_err();
    let attributes = type_alias_ast.attributes(db).structurize(db);
    let resolver_data = Arc::new(resolver.data);
    Ok(TypeAliasData { resolved_type: Ok(ty), attributes, resolver_data })
}

/// Cycle handling for a type-alias item.
pub fn type_alias_semantic_data_cycle_helper<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    type_alias_ast: &ast::ItemTypeAlias<'db>,
    lookup_item_id: LookupItemId<'db>,
    generic_params_data: GenericParamsData<'db>,
) -> Maybe<TypeAliasData<'db>> {
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let err = Err(diagnostics.report(type_alias_ast.name(db).stable_ptr(db), TypeAliasCycle));

    let resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);

    let attributes = type_alias_ast.attributes(db).structurize(db);

    Ok(TypeAliasData { resolved_type: err, attributes, resolver_data: Arc::new(resolver.data) })
}
