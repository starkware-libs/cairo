use std::collections::HashMap;

use cairo_lang_defs::ids::{NamedLanguageElementId, TraitConstantId, TraitFunctionId};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use cairo_lang_semantic::expr::inference::InferenceId;
use cairo_lang_semantic::lookup_item::HasResolverData;
use cairo_lang_semantic::resolve::{ResolvedConcreteItem, Resolver};
use cairo_lang_semantic::substitution::{
    GenericSubstitution, SemanticRewriter, SubstitutionRewriter,
};
use cairo_lang_semantic::{ConcreteTraitId, Parameter};
use cairo_lang_syntax::node::ast::{ImplItem, ItemImpl, MaybeImplBody};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Token, TypedSyntaxNode};
use itertools::{Itertools, chain};
use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, Range, TextEdit, WorkspaceEdit};

use crate::lang::db::{AnalysisDatabase, LsSemanticGroup, LsSyntaxGroup};
use crate::lang::lsp::ToLsp;

/// Generates a completion adding all trait members that have not yet been specified.
/// Functions are added with empty bodies, consts with placeholder values.
pub fn fill_trait_members(
    db: &AnalysisDatabase,
    node: SyntaxNode,
    params: &CodeActionParams,
) -> Option<CodeAction> {
    let file = db.find_module_file_containing_node(&node)?.file_id(db).ok()?;

    let item_impl_node = db.first_ancestor_of_kind(node, SyntaxKind::ItemImpl)?;
    let item_impl = ItemImpl::from_syntax_node(db, item_impl_node);

    // Do not complete `impl`s without braces.
    let MaybeImplBody::Some(impl_body) = item_impl.body(db) else {
        return None;
    };

    let specified_impl_items = impl_body.items(db);

    let already_implemented_item_names = specified_impl_items
        .elements(db)
        .iter()
        .filter_map(|item| match item {
            ImplItem::Function(item) => Some(item.declaration(db).name(db).token(db).text(db)),
            ImplItem::Type(item) => Some(item.name(db).token(db).text(db)),
            ImplItem::Constant(item) => Some(item.name(db).token(db).text(db)),
            _ => None, // No other items can appear in trait impl.
        })
        .collect_vec();

    let concrete_trait_id = find_concrete_trait_id(db, &item_impl)?;
    let trait_id = concrete_trait_id.trait_id(db);

    let mut trait_constants = db.trait_constants(trait_id).ok()?;
    let mut trait_types = db.trait_types(trait_id).ok()?;
    let mut trait_functions = db.trait_functions(trait_id).ok()?;

    trait_constants.retain(|key, _| !already_implemented_item_names.contains(key));
    trait_types.retain(|key, _| !already_implemented_item_names.contains(key));
    trait_functions.retain(|key, _| !already_implemented_item_names.contains(key));

    let trait_generics = db.trait_generic_params(trait_id).ok()?;
    let specified_generics = concrete_trait_id.generic_args(db);
    let substitution = GenericSubstitution::new(&trait_generics, &specified_generics);
    let mut type_concretizer = SubstitutionRewriter { db, substitution: &substitution };

    // Iterators borrowing SubstitutionRewriter mutably need intermediate collection.
    let code = chain!(
        trait_constants
            .values()
            .filter_map(|&id| constant_code(db, id, &mut type_concretizer))
            .collect_vec()
            .into_iter(),
        trait_types.values().map(|id| format!("type {};", id.name(db))),
        trait_functions
            .values()
            .filter_map(|&id| function_code(db, id, &mut type_concretizer))
            .collect_vec()
            .into_iter()
    )
    .join("\n\n");

    let impl_body_end_before_right_brace =
        specified_impl_items.as_syntax_node().span_end_without_trivia(db);

    let code_insert_position =
        impl_body_end_before_right_brace.position_in_file(db, file)?.to_lsp();

    let edit_start = code_insert_position;
    let edit_end = code_insert_position;

    let mut changes = HashMap::new();
    let url = params.text_document.uri.clone();
    let change = TextEdit { range: Range::new(edit_start, edit_end), new_text: code };

    changes.insert(url, vec![change]);

    let edit = WorkspaceEdit::new(changes);

    Some(CodeAction {
        title: String::from("Implement missing members"),
        kind: Some(CodeActionKind::QUICKFIX),
        edit: Some(edit),
        ..Default::default()
    })
}

/// Obtains semantic model of [`ItemImpl`] and returns a [`ConcreteTraitId`] it refers to.
fn find_concrete_trait_id(db: &AnalysisDatabase, item_impl: &ItemImpl) -> Option<ConcreteTraitId> {
    let lookup_item_id = db.find_lookup_item(&item_impl.as_syntax_node())?;
    let resolver_data = lookup_item_id.resolver_data(db).ok()?;

    let mut resolver = Resolver::with_data(
        db,
        resolver_data.as_ref().clone_with_inference_id(db, InferenceId::NoContext),
    );

    let mut diagnostics = SemanticDiagnostics::default();

    match resolver.resolve_concrete_path(
        &mut diagnostics,
        item_impl.trait_path(db).elements(db),
        NotFoundItemType::Trait,
    ) {
        Ok(ResolvedConcreteItem::Trait(id)) => Some(id),
        _ => None,
    }
}

fn constant_code(
    db: &AnalysisDatabase,
    id: TraitConstantId,
    type_concretizer: &mut SubstitutionRewriter<'_>,
) -> Option<String> {
    let name = id.name(db);
    let constant_data = db.priv_trait_constant_data(id).ok()?;
    let ty = type_concretizer.rewrite(constant_data.ty).ok()?.format(db);

    Some(format!("const {name}: {ty} = ();"))
}

/// Generates [`Parameter`] declaration containing its name,
/// type and optionally a `ref` indicator.
fn function_parameter(
    db: &AnalysisDatabase,
    parameter: &Parameter,
    type_concretizer: &mut SubstitutionRewriter<'_>,
) -> Option<String> {
    let prefix = match parameter.mutability {
        cairo_lang_semantic::Mutability::Reference => "ref ",
        cairo_lang_semantic::Mutability::Immutable => "",
        // `mut` modifier is not used with function arguments.
        cairo_lang_semantic::Mutability::Mutable => return None,
    };

    let name = parameter.id.name(db);
    let ty = type_concretizer.rewrite(parameter.ty).ok()?.format(db);

    Some(format!("{prefix}{name}: {ty}"))
}

/// Generates declaration of a [`TraitFunctionId`] containing its signature with parameters,
/// panic indicator, implicits and default implementation if such exists.
/// Generics are substituted with concrete types according to `concrete_types` map.
/// Returns None if function has a default implementation.
fn function_code(
    db: &AnalysisDatabase,
    id: TraitFunctionId,
    type_concretizer: &mut SubstitutionRewriter<'_>,
) -> Option<String> {
    // Do not complete functions that have default implementations.
    if db.trait_function_body(id).ok()?.is_some() {
        return None;
    }

    let name = id.name(db);

    let signature = db.trait_function_signature(id).ok()?;

    let parameters = signature
        .params
        .iter()
        .filter_map(|parameter| function_parameter(db, parameter, type_concretizer))
        .join(", ");

    let title = Some(format!("fn {name}({parameters})"));

    let return_type = type_concretizer.rewrite(signature.return_type).ok()?;
    let return_type =
        if return_type.is_unit(db) { None } else { Some(format!("-> {}", return_type.format(db))) };

    let implicits = match &signature.implicits[..] {
        [] => None,
        types => Some(format!("implicits({})", types.iter().map(|ty| ty.format(db)).join(", "))),
    };

    let nopanic = if !signature.panicable { Some(String::from("nopanic")) } else { None };

    let body: Option<String> = Some(String::from("{}"));

    Some([title, return_type, implicits, nopanic, body].into_iter().flatten().join(" "))
}
