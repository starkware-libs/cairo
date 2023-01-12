use std::sync::Arc;

use cairo_lang_defs::ids::{FunctionWithBodyId, LanguageElementId};
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_syntax::node::ast;

use crate::db::LoweringGroup;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind, LoweringDiagnostics};
use crate::FlatBlockEnd;

#[derive(Debug, PartialEq, Eq)]
pub enum InlineConfiguration {
    // The user did not specify any inlining preferences.
    None,
    Always,
}

/// data about inlining.
#[derive(Debug, PartialEq, Eq)]
pub struct PrivInlineData {
    /// Diagnostics produced while collecting inlining Info.
    pub diagnostics: Diagnostics<LoweringDiagnostic>,
    pub config: InlineConfiguration,
    pub is_inlineable: bool,
}

pub fn priv_inline_data(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Arc<PrivInlineData>> {
    let defs_db = db.upcast();
    let mut diagnostics = LoweringDiagnostics::new(function_id.module_file(defs_db));
    let mut seen_inline_attr = false;
    let mut config = InlineConfiguration::None;

    for attr in db.function_with_body_attributes(function_id)?.iter() {
        if attr.id != "inline" {
            continue;
        }

        if seen_inline_attr {
            diagnostics.report(
                attr.id_stable_ptr.untyped(),
                LoweringDiagnosticKind::RedundantInlineAttribute,
            );
            // If we have multiple inline attributes revert to InlineConfiguration::None.
            config = InlineConfiguration::None;
        }

        seen_inline_attr = true;

        match &attr.args[..] {
            [ast::Expr::Path(path)] if &path.node.get_text(db.upcast()) == "always" => {
                config = InlineConfiguration::Always;
            }
            [] => {
                diagnostics.report(
                    attr.id_stable_ptr.untyped(),
                    LoweringDiagnosticKind::InlineWithoutArgumentNotSupported,
                );
                continue;
            }
            _ => {
                diagnostics.report(
                    attr.args_stable_ptr.untyped(),
                    LoweringDiagnosticKind::UnsupportedInlineArguments,
                );
                continue;
            }
        }
    }

    let mut is_inlineable = true;

    if db
            .function_with_body_direct_function_with_body_callees(function_id)?
            .contains(&function_id)
            // TODO(ilya): Relax requirement, if one of the functions is does not have
            //  #[inline(always)] than we can inline it.
            || db.function_with_body_scc(function_id).len() > 1
    {
        is_inlineable = false;
        if config == InlineConfiguration::Always {
            diagnostics.report(
                function_id.untyped_stable_ptr(defs_db),
                LoweringDiagnosticKind::CannotInlineFunctionThatMightCallItself,
            );
        }
    }

    let lowered = db.function_with_body_lowered_flat(function_id)?;
    let root_block_id = lowered.root?;

    for (block_id, block) in lowered.blocks.iter() {
        match block.end {
            FlatBlockEnd::Return { .. } if block_id != root_block_id => {
                diagnostics.report(
                    function_id.untyped_stable_ptr(defs_db),
                    LoweringDiagnosticKind::InliningFunctionWithEarlyReturnNotSupported,
                );
            }
            _ => {}
        };
    }

    let diagnostics = diagnostics.build();

    Ok(Arc::new(PrivInlineData { diagnostics, config, is_inlineable }))
}
