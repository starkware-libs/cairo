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
    let mut diagnostics = LoweringDiagnostics::new(function_id.module_file(db.upcast()));
    let config = parse_inline_attribute(db, &mut diagnostics, function_id)?;

    // If the the function is marked as #[inline(always)], we need to report
    // inlining problems.
    let report_diagnostics = config == InlineConfiguration::Always;
    let is_inlineable = check_inlinable(db, &mut diagnostics, report_diagnostics, function_id)?;

    Ok(Arc::new(PrivInlineData { diagnostics: diagnostics.build(), config, is_inlineable }))
}

// Checks if the given function can be inlined.
// If report_diagnostics is true, adds a diagnostics with the reason that prevents inlining.
fn check_inlinable(
    db: &dyn LoweringGroup,
    diagnostics: &mut LoweringDiagnostics,
    report_diagnostics: bool,
    function_id: FunctionWithBodyId,
) -> Maybe<bool> {
    let defs_db = db.upcast();
    if db
            .function_with_body_direct_function_with_body_callees(function_id)?
            .contains(&function_id)
            // TODO(ilya): Relax requirement, if one of the functions is does not have
            //  #[inline(always)] than we can inline it.
            || db.function_with_body_scc(function_id).len() > 1
    {
        if report_diagnostics {
            diagnostics.report(
                function_id.untyped_stable_ptr(defs_db),
                LoweringDiagnosticKind::CannotInlineFunctionThatMightCallItself,
            );
        }
        return Ok(false);
    }

    let lowered = db.function_with_body_lowered_flat(function_id)?;
    let root_block_id = lowered.root?;
    for (block_id, block) in lowered.blocks.iter() {
        match block.end {
            FlatBlockEnd::Return { .. } if block_id != root_block_id => {
                if report_diagnostics {
                    diagnostics.report(
                        function_id.untyped_stable_ptr(defs_db),
                        LoweringDiagnosticKind::InliningFunctionWithEarlyReturnNotSupported,
                    );
                }
                return Ok(false);
            }
            _ => {}
        };
    }

    Ok(true)
}

// Parses the inline attributes for a given function.
fn parse_inline_attribute(
    db: &dyn LoweringGroup,
    diagnostics: &mut LoweringDiagnostics,
    function_id: FunctionWithBodyId,
) -> Maybe<InlineConfiguration> {
    let mut config = InlineConfiguration::None;
    let mut seen_inline_attr = false;
    for attr in db.function_with_body_attributes(function_id)?.iter() {
        if attr.id != "inline" {
            continue;
        }

        match &attr.args[..] {
            [ast::Expr::Path(path)] if &path.node.get_text(db.upcast()) == "always" => {
                config = InlineConfiguration::Always;
            }
            [] => {
                diagnostics.report(
                    attr.id_stable_ptr.untyped(),
                    LoweringDiagnosticKind::InlineWithoutArgumentNotSupported,
                );
            }
            _ => {
                diagnostics.report(
                    attr.args_stable_ptr.untyped(),
                    LoweringDiagnosticKind::UnsupportedInlineArguments,
                );
            }
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
    }
    Ok(config)
}
