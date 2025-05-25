use std::sync::Arc;

use cairo_lang_defs::ids::{LookupItemId, ModuleFileId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};

use super::generics::{GenericParamsData, semantic_generic_params};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::TypeAliasCycle;
use crate::diagnostic::{SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::SemanticRewriter;
use crate::types::resolve_type;
use crate::{GenericParam, TypeId};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TypeAliasData {
    pub resolved_type: Maybe<TypeId>,
    pub generic_params: Vec<GenericParam>,
    pub attributes: Vec<Attribute>,
    pub resolver_data: Arc<ResolverData>,
}

/// Computes data about the generic parameters of a type-alias item.
pub fn type_alias_generic_params_data_helper(
    db: &dyn SemanticGroup,
    module_file_id: ModuleFileId,
    type_alias_ast: &ast::ItemTypeAlias,
    lookup_item_id: LookupItemId,
    parent_resolver_data: Option<Arc<ResolverData>>,
) -> Maybe<GenericParamsData> {
    let mut diagnostics = SemanticDiagnostics::default();
    let inference_id = InferenceId::LookupItemGenerics(lookup_item_id);

    let mut resolver = match parent_resolver_data {
        Some(parent_resolver_data) => {
            Resolver::with_data(db, parent_resolver_data.clone_with_inference_id(db, inference_id))
        }
        None => Resolver::new(db, module_file_id, inference_id),
    };
    resolver.set_feature_config(&lookup_item_id, type_alias_ast, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &type_alias_ast.generic_params(db),
    );

    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, type_alias_ast.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { diagnostics: diagnostics.build(), generic_params, resolver_data })
}

/// Computes data about a type-alias item.
pub fn type_alias_semantic_data_helper(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    type_alias_ast: &ast::ItemTypeAlias,
    lookup_item_id: LookupItemId,
    generic_params_data: GenericParamsData,
) -> Maybe<TypeAliasData> {
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

    let generic_params = inference.rewrite(generic_params_data.generic_params).no_err();
    let ty = inference.rewrite(ty).no_err();
    let attributes = type_alias_ast.attributes(db).structurize(db);
    let resolver_data = Arc::new(resolver.data);
    Ok(TypeAliasData { resolved_type: Ok(ty), generic_params, attributes, resolver_data })
}

/// Cycle handling for a type-alias item.
pub fn type_alias_semantic_data_cycle_helper(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    type_alias_ast: &ast::ItemTypeAlias,
    lookup_item_id: LookupItemId,
    generic_params_data: GenericParamsData,
) -> Maybe<TypeAliasData> {
    let inference_id = InferenceId::LookupItemDeclaration(lookup_item_id);
    let err = Err(diagnostics.report(type_alias_ast.name(db).stable_ptr(db), TypeAliasCycle));

    let resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);

    let attributes = type_alias_ast.attributes(db).structurize(db);

    Ok(TypeAliasData {
        resolved_type: err,
        generic_params: generic_params_data.generic_params,
        attributes,
        resolver_data: Arc::new(resolver.data),
    })
}
