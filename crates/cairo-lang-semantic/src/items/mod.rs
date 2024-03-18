use cairo_lang_defs::ids::TraitId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::ast::ExprPath;
use cairo_lang_utils::try_extract_matches;

use crate::diagnostic::SemanticDiagnosticKind::NotATrait;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use crate::resolve::{ResolvedGenericItem, Resolver};

pub mod attribute;
pub mod constant;
pub mod enm;
pub mod extern_function;
pub mod extern_type;
pub mod fmt;
pub mod free_function;
pub mod function_with_body;
pub mod functions;
pub mod generics;
pub mod imp;
pub mod impl_alias;
pub mod modifiers;
pub mod module;
pub mod module_type_alias;
pub mod structure;
pub mod trt;
pub mod type_aliases;
pub mod us;
pub mod visibility;

#[cfg(test)]
mod test;

/// Tries to resolve a trait path. Reports a diagnostic if the path doesn't point to a trait.
fn resolve_trait_path(
    diagnostics: &mut SemanticDiagnostics,
    resolver: &mut Resolver<'_>,
    trait_path_syntax: &ExprPath,
) -> Maybe<TraitId> {
    try_extract_matches!(
        resolver.resolve_generic_path_with_args(
            diagnostics,
            trait_path_syntax,
            NotFoundItemType::Trait,
        )?,
        ResolvedGenericItem::Trait
    )
    .ok_or_else(|| diagnostics.report(trait_path_syntax, NotATrait))
}
