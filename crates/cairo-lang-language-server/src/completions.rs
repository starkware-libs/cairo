use cairo_lang_defs::ids::{
    LanguageElementId, ModuleId, TopLevelLanguageElementId, TraitFunctionId,
};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::infers::InferenceEmbeddings;
use cairo_lang_semantic::expr::inference::solver::SolutionSet;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::items::structure::SemanticStructEx;
use cairo_lang_semantic::items::us::SemanticUseEx;
use cairo_lang_semantic::lookup_item::{HasResolverData, LookupItemEx};
use cairo_lang_semantic::lsp_helpers::TypeFilter;
use cairo_lang_semantic::resolve::{ResolvedGenericItem, Resolver};
use cairo_lang_semantic::{ConcreteTypeId, TypeLongId};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use lsp::{CompletionItem, CompletionItemKind, Position, Range, TextEdit};

use crate::get_node_and_lookup_items;

pub fn dot_completions(
    db: &(dyn SemanticGroup + 'static),
    file: FileId,
    position: Position,
) -> Option<Vec<CompletionItem>> {
    let syntax_db = db.upcast();
    // Get a resolver in the current context.
    let (_, lookup_items) = get_node_and_lookup_items(db, file, position)?;
    let lookup_item_id = lookup_items.into_iter().next()?;
    let function_with_body = lookup_item_id.function_with_body()?;
    let module_id = function_with_body.module_file_id(db.upcast()).0;
    let resolver_data = lookup_item_id.resolver_data(db).ok()?;
    let resolver = Resolver::with_data(db, resolver_data.as_ref().clone());

    // Extract lhs node.
    let node = extract_lhs_expr_for_method_completion(syntax_db, file, position)?;
    let stable_ptr = node.stable_ptr().untyped();
    // Get its semantic model.
    let expr_id = db.lookup_expr_by_ptr(function_with_body, node.stable_ptr()).ok()?;
    let semantic_expr = db.expr_semantic(function_with_body, expr_id);
    // Get the type.
    let ty = semantic_expr.ty();
    if ty.is_missing(db) {
        eprintln!("Type is missing");
        return None;
    }

    // Find relevant methods for type.
    let relevant_methods = find_methods_for_type(db, resolver, ty, stable_ptr);

    let mut completions = Vec::new();
    for trait_function in relevant_methods {
        let Some(completion) = completion_for_method(db, module_id, trait_function) else {
             continue;
        };
        completions.push(completion);
    }

    // Find members of the type.
    if let TypeLongId::Concrete(ConcreteTypeId::Struct(concrete_struct_id)) =
        db.lookup_intern_type(ty)
    {
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
fn completion_for_method(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    trait_function: TraitFunctionId,
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
            range: Range::new(
                Position { line: 0, character: 0 },
                Position { line: 0, character: 0 },
            ),
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
fn module_has_trait(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    trait_id: cairo_lang_defs::ids::TraitId,
) -> Option<bool> {
    if db.module_traits_ids(module_id).ok()?.contains(&trait_id) {
        return Some(true);
    }
    for use_id in db.module_uses_ids(module_id).ok()? {
        if db.use_resolved_item(use_id) == Ok(ResolvedGenericItem::Trait(trait_id)) {
            return Some(true);
        }
    }
    Some(false)
}

/// Finds all methods that can be called on a type.
fn find_methods_for_type(
    db: &(dyn SemanticGroup + 'static),
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
        for trait_function in methods {
            let clone_data = &mut resolver.inference().clone_data();
            let mut inference = clone_data.inference(db);
            let lookup_context = resolver.impl_lookup_context();
            // Check if trait function signature's first param can fit our expr type.
            let Some((concrete_trait_id, _)) = inference.infer_concrete_trait_by_self(
                trait_function, ty, &lookup_context, Some(stable_ptr)
            ) else {
                eprintln!("Can't fit");
                continue;
            };

            // Find impls for it.
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

/// Extracts the lhs expression of a binary operator.
fn extract_lhs_expr_for_method_completion(
    db: &(dyn SemanticGroup + 'static),
    file: FileId,
    mut position: Position,
) -> Option<ast::Expr> {
    let syntax_db = db.upcast();

    // Move the cursor to the left of the dot.
    position.character -= 1;
    let (node, _) = get_node_and_lookup_items(db, file, position)?;
    let node = node.parent()?;
    if node.kind(syntax_db) != SyntaxKind::TerminalDot {
        eprintln!("Expected TerminalDot");
        return None;
    };
    let node = node.parent()?;
    if node.kind(syntax_db) != SyntaxKind::ExprBinary {
        eprintln!("Not a binary operator");
        return None;
    };
    let node = ast::ExprBinary::from_syntax_node(syntax_db, node);
    let node = node.lhs(syntax_db);
    Some(node)
}
