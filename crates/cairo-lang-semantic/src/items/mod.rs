use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{ImplDefId, TraitId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_syntax::node::ast::ExprPath;
use cairo_lang_utils::try_extract_matches;
use salsa::Database;

use crate::diagnostic::SemanticDiagnosticKind::NotATrait;
use crate::diagnostic::{NotFoundItemType, SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::resolve::{ResolutionContext, ResolvedGenericItem, Resolver};

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
pub mod macro_call;
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
fn resolve_trait_path<'db>(
    db: &'db dyn Database,
    diagnostics: &mut SemanticDiagnostics<'db>,
    resolver: &mut Resolver<'db>,
    trait_path_syntax: &ExprPath<'db>,
) -> Maybe<TraitId<'db>> {
    try_extract_matches!(
        resolver.resolve_generic_path_with_args(
            diagnostics,
            trait_path_syntax,
            NotFoundItemType::Trait,
            ResolutionContext::Default,
        )?,
        ResolvedGenericItem::Trait
    )
    .ok_or_else(|| diagnostics.report(trait_path_syntax.stable_ptr(db), NotATrait))
}

/// A context of a trait or an impl, if in any of those. This is used in the resolver to resolve
/// "Self::" paths.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub enum TraitOrImplContext<'db> {
    /// No trait/impl context.
    None,
    /// The context is of a trait.
    Trait(TraitId<'db>),
    /// The context is of an impl.
    Impl(ImplDefId<'db>),
}

impl<'db> DebugWithDb<'db> for TraitOrImplContext<'db> {
    type Db = dyn Database;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db dyn Database) -> std::fmt::Result {
        match self {
            TraitOrImplContext::None => write!(f, "None"),
            TraitOrImplContext::Trait(trait_ctx) => {
                write!(f, "{:?}", trait_ctx.debug(db))
            }
            TraitOrImplContext::Impl(impl_ctx) => write!(f, "{:?}", impl_ctx.debug(db)),
        }
    }
}
