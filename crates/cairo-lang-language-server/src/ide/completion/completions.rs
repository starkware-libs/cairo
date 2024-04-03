use cairo_lang_defs::ids::{
    FunctionWithBodyId, ImplItemId, LanguageElementId, LookupItemId, ModuleFileId, ModuleId,
    ModuleItemId, NamedLanguageElementId, TopLevelLanguageElementId, TraitFunctionId,
};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextOffset;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use cairo_lang_semantic::expr::inference::infers::InferenceEmbeddings;
use cairo_lang_semantic::expr::inference::solver::SolutionSet;
use cairo_lang_semantic::expr::inference::InferenceId;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::items::structure::SemanticStructEx;
use cairo_lang_semantic::items::us::SemanticUseEx;
use cairo_lang_semantic::lookup_item::{HasResolverData, LookupItemEx};
use cairo_lang_semantic::lsp_helpers::TypeFilter;
use cairo_lang_semantic::resolve::{ResolvedConcreteItem, ResolvedGenericItem, Resolver};
use cairo_lang_semantic::types::peel_snapshots;
use cairo_lang_semantic::{ConcreteTypeId, Pattern, TypeLongId};
use cairo_lang_syntax::node::ast::PathSegment;
use cairo_lang_syntax::node::{ast, TypedStablePtr, TypedSyntaxNode};
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, Position, Range, TextEdit};
use tracing::debug;

use crate::find_node_module;
use crate::lang::lsp::ToLsp;

#[tracing::instrument(level = "trace", skip_all)]
pub fn generic_completions(
    db: &(dyn SemanticGroup + 'static),
    module_file_id: ModuleFileId,
    lookup_items: Vec<LookupItemId>,
) -> Vec<CompletionItem> {
    let mut completions = vec![];

    // Crates.
    completions.extend(db.crate_configs().keys().map(|crate_id| CompletionItem {
        label: db.lookup_intern_crate(*crate_id).name().into(),
        kind: Some(CompletionItemKind::MODULE),
        ..CompletionItem::default()
    }));

    // Module completions.
    completions.extend(db.module_items(module_file_id.0).unwrap_or_default().iter().map(|item| {
        CompletionItem {
            label: item.name(db.upcast()).to_string(),
            kind: ResolvedGenericItem::from_module_item(db, *item)
                .ok()
                .map(resolved_generic_item_completion_kind),
            ..CompletionItem::default()
        }
    }));

    // Local variables and params.
    let Some(lookup_item_id) = lookup_items.into_iter().next() else {
        return completions;
    };
    let function_id = match lookup_item_id {
        LookupItemId::ModuleItem(ModuleItemId::FreeFunction(free_function_id)) => {
            FunctionWithBodyId::Free(free_function_id)
        }
        LookupItemId::ImplItem(ImplItemId::Function(impl_function_id)) => {
            FunctionWithBodyId::Impl(impl_function_id)
        }
        _ => {
            return completions;
        }
    };
    let Ok(signature) = db.function_with_body_signature(function_id) else {
        return completions;
    };
    for param in &signature.params {
        completions.push(CompletionItem {
            label: param.name.clone().into(),
            kind: Some(CompletionItemKind::VARIABLE),
            ..CompletionItem::default()
        });
    }

    let Ok(body) = db.function_body(function_id) else {
        return completions;
    };
    for (_id, pat) in &body.patterns {
        if let Pattern::Variable(var) = pat {
            completions.push(CompletionItem {
                label: var.name.clone().into(),
                kind: Some(CompletionItemKind::VARIABLE),
                ..CompletionItem::default()
            });
        }
    }
    completions
}

fn resolved_generic_item_completion_kind(item: ResolvedGenericItem) -> CompletionItemKind {
    match item {
        ResolvedGenericItem::Constant(_) => CompletionItemKind::CONSTANT,
        ResolvedGenericItem::Module(_) => CompletionItemKind::MODULE,
        ResolvedGenericItem::GenericFunction(_) | ResolvedGenericItem::TraitFunction(_) => {
            CompletionItemKind::FUNCTION
        }
        ResolvedGenericItem::GenericType(_) | ResolvedGenericItem::GenericTypeAlias(_) => {
            CompletionItemKind::CLASS
        }
        ResolvedGenericItem::Impl(_) | ResolvedGenericItem::GenericImplAlias(_) => {
            CompletionItemKind::CLASS
        }
        ResolvedGenericItem::Variant(_) => CompletionItemKind::ENUM_MEMBER,
        ResolvedGenericItem::Trait(_) => CompletionItemKind::INTERFACE,
        ResolvedGenericItem::Variable(_, _) => CompletionItemKind::VARIABLE,
    }
}

#[tracing::instrument(level = "trace", skip_all)]
pub fn colon_colon_completions(
    db: &(dyn SemanticGroup + 'static),
    module_file_id: ModuleFileId,
    lookup_items: Vec<LookupItemId>,
    segments: Vec<PathSegment>,
) -> Option<Vec<CompletionItem>> {
    // Get a resolver in the current context.
    let resolver_data = match lookup_items.into_iter().next() {
        Some(item) => {
            (*item.resolver_data(db).ok()?).clone_with_inference_id(db, InferenceId::NoContext)
        }
        None => Resolver::new(db, module_file_id, InferenceId::NoContext).data,
    };
    let mut resolver = Resolver::with_data(db, resolver_data);

    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast()).ok()?);
    let item = resolver
        .resolve_concrete_path(&mut diagnostics, segments, NotFoundItemType::Identifier)
        .ok()?;

    Some(match item {
        ResolvedConcreteItem::Module(module_id) => db
            .module_items(module_id)
            .unwrap_or_default()
            .iter()
            .map(|item| CompletionItem {
                label: item.name(db.upcast()).to_string(),
                kind: ResolvedGenericItem::from_module_item(db, *item)
                    .ok()
                    .map(resolved_generic_item_completion_kind),
                ..CompletionItem::default()
            })
            .collect(),
        ResolvedConcreteItem::Trait(item) => db
            .trait_functions(item.trait_id(db))
            .unwrap_or_default()
            .iter()
            .map(|(name, _)| CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                ..CompletionItem::default()
            })
            .collect(),
        ResolvedConcreteItem::Impl(item) => item
            .concrete_trait(db)
            .map(|trait_id| {
                db.trait_functions(trait_id.trait_id(db))
                    .unwrap_or_default()
                    .iter()
                    .map(|(name, _)| CompletionItem {
                        label: name.to_string(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        ..CompletionItem::default()
                    })
                    .collect()
            })
            .unwrap_or_default(),
        ResolvedConcreteItem::Type(ty) => match db.lookup_intern_type(ty) {
            TypeLongId::Concrete(ConcreteTypeId::Enum(enum_id)) => db
                .enum_variants(enum_id.enum_id(db))
                .unwrap_or_default()
                .iter()
                .map(|(name, _)| CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::ENUM_MEMBER),
                    ..CompletionItem::default()
                })
                .collect(),
            _ => vec![],
        },
        _ => vec![],
    })
}

#[tracing::instrument(level = "trace", skip_all)]
pub fn dot_completions(
    db: &dyn SemanticGroup,
    file_id: FileId,
    lookup_items: Vec<LookupItemId>,
    expr: ast::ExprBinary,
) -> Option<Vec<CompletionItem>> {
    let syntax_db = db.upcast();
    // Get a resolver in the current context.
    let lookup_item_id = lookup_items.into_iter().next()?;
    let function_with_body = lookup_item_id.function_with_body()?;
    let module_id = function_with_body.module_file_id(db.upcast()).0;
    let resolver_data = lookup_item_id.resolver_data(db).ok()?;
    let resolver = Resolver::with_data(
        db,
        resolver_data.as_ref().clone_with_inference_id(db, InferenceId::NoContext),
    );

    // Extract lhs node.
    let node = expr.lhs(syntax_db);
    let stable_ptr = node.stable_ptr().untyped();
    // Get its semantic model.
    let expr_id = db.lookup_expr_by_ptr(function_with_body, node.stable_ptr()).ok()?;
    let semantic_expr = db.expr_semantic(function_with_body, expr_id);
    // Get the type.
    let ty = semantic_expr.ty();
    if ty.is_missing(db) {
        debug!("type is missing");
        return None;
    }

    // Find relevant methods for type.
    let offset = if let Some(ModuleId::Submodule(submodule_id)) =
        find_node_module(db, file_id, expr.as_syntax_node())
    {
        let module_def_ast = submodule_id.stable_ptr(db.upcast()).lookup(syntax_db);
        if let ast::MaybeModuleBody::Some(body) = module_def_ast.body(syntax_db) {
            body.items(syntax_db).as_syntax_node().span_start_without_trivia(syntax_db)
        } else {
            TextOffset::default()
        }
    } else {
        TextOffset::default()
    };
    let position = offset.position_in_file(db.upcast(), file_id).unwrap().to_lsp();
    let relevant_methods = find_methods_for_type(db, resolver, ty, stable_ptr);

    let mut completions = Vec::new();
    for trait_function in relevant_methods {
        let Some(completion) = completion_for_method(db, module_id, trait_function, position)
        else {
            continue;
        };
        completions.push(completion);
    }

    // Find members of the type.
    let (_, long_ty) = peel_snapshots(db, ty);
    if let TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id)) = long_ty {
        db.concrete_struct_members(concrete_struct_id).ok()?.into_iter().for_each(
            |(name, member)| {
                let completion = CompletionItem {
                    label: name.to_string(),
                    detail: Some(member.ty.format(db.upcast())),
                    kind: Some(CompletionItemKind::FIELD),
                    ..CompletionItem::default()
                };
                completions.push(completion);
            },
        );
    }
    Some(completions)
}

/// Returns a completion item for a method.
#[tracing::instrument(level = "trace", skip_all)]
fn completion_for_method(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    trait_function: TraitFunctionId,
    position: Position,
) -> Option<CompletionItem> {
    let trait_id = trait_function.trait_id(db.upcast());
    let name = trait_function.name(db.upcast());
    db.trait_function_signature(trait_function).ok()?;

    // TODO(spapini): Add signature.
    let detail = trait_id.full_path(db.upcast());
    let trait_full_path = trait_id.full_path(db.upcast());
    let mut additional_text_edits = vec![];

    // If the trait is not in scope, add a use statement.
    if !module_has_trait(db, module_id, trait_id)? {
        additional_text_edits.push(TextEdit {
            range: Range::new(position, position),
            new_text: format!("use {trait_full_path};\n"),
        });
    }

    let completion = CompletionItem {
        label: format!("{}()", name),
        insert_text: Some(format!("{}(", name)),
        detail: Some(detail),
        kind: Some(CompletionItemKind::METHOD),
        additional_text_edits: Some(additional_text_edits),
        ..CompletionItem::default()
    };
    Some(completion)
}

/// Checks if a module has a trait in scope.
#[tracing::instrument(level = "trace", skip_all)]
fn module_has_trait(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    trait_id: cairo_lang_defs::ids::TraitId,
) -> Option<bool> {
    if db.module_traits_ids(module_id).ok()?.contains(&trait_id) {
        return Some(true);
    }
    for use_id in db.module_uses_ids(module_id).ok()?.iter().copied() {
        if db.use_resolved_item(use_id) == Ok(ResolvedGenericItem::Trait(trait_id)) {
            return Some(true);
        }
    }
    Some(false)
}

/// Finds all methods that can be called on a type.
#[tracing::instrument(level = "trace", skip_all)]
fn find_methods_for_type(
    db: &dyn SemanticGroup,
    mut resolver: Resolver<'_>,
    ty: cairo_lang_semantic::TypeId,
    stable_ptr: cairo_lang_syntax::node::ids::SyntaxStablePtrId,
) -> Vec<TraitFunctionId> {
    let type_filter = match ty.head(db) {
        Some(head) => TypeFilter::TypeHead(head),
        None => TypeFilter::NoFilter,
    };

    let mut relevant_methods = Vec::new();
    // Find methods on type.
    // TODO(spapini): Look only in current crate dependencies.
    for crate_id in db.crates() {
        let methods = db.methods_in_crate(crate_id, type_filter.clone());
        for trait_function in methods.iter().copied() {
            let clone_data =
                &mut resolver.inference().clone_with_inference_id(db, InferenceId::NoContext);
            let mut inference = clone_data.inference(db);
            let lookup_context = resolver.impl_lookup_context();
            // Check if trait function signature's first param can fit our expr type.
            let Some((concrete_trait_id, _)) = inference.infer_concrete_trait_by_self(
                trait_function,
                ty,
                &lookup_context,
                Some(stable_ptr),
                |_| {},
            ) else {
                debug!("can't fit");
                continue;
            };

            // Find impls for it.

            // ignore the result as nothing can be done with the error, if any.
            inference.solve().ok();
            if !matches!(
                inference.trait_solution_set(concrete_trait_id, lookup_context),
                Ok(SolutionSet::Unique(_) | SolutionSet::Ambiguous(_))
            ) {
                continue;
            }
            relevant_methods.push(trait_function);
        }
    }
    relevant_methods
}
