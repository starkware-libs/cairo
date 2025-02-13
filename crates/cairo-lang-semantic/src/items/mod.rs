use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{ImplDefId, TraitId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::ast::ExprPath;
use cairo_lang_utils::try_extract_matches;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::NotATrait;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::resolve::{ResolvedGenericItem, Resolver};

pub mod attribute;
pub mod constant;
pub mod enm;
pub mod extern_function;
pub mod extern_type;
pub mod feature_kind;
pub mod fmt;
pub mod free_function;
pub mod function_with_body;
pub mod functions;
pub mod generics;
pub mod imp;
pub mod impl_alias;
pub mod implization;
pub mod macro_declaration;
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
            None,
        )?,
        ResolvedGenericItem::Trait
    )
    .ok_or_else(|| diagnostics.report(trait_path_syntax, NotATrait))
}

/// A context of a trait or an impl, if in any of those. This is used in the resolver to resolve
/// "Self::" paths.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum TraitOrImplContext {
    /// No trait/impl context.
    None,
    /// The context is of a trait.
    Trait(TraitId),
    /// The context is of an impl.
    Impl(ImplDefId),
}

impl DebugWithDb<dyn SemanticGroup> for TraitOrImplContext {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        db: &(dyn SemanticGroup + 'static),
    ) -> std::fmt::Result {
        match self {
            TraitOrImplContext::None => write!(f, "None"),
            TraitOrImplContext::Trait(trait_ctx) => write!(f, "{:?}", trait_ctx.debug(db)),
            TraitOrImplContext::Impl(impl_ctx) => write!(f, "{:?}", impl_ctx.debug(db)),
        }
    }
}
