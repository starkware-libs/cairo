use std::sync::Arc;

use cairo_lang_defs::ids::{FunctionWithBodyId, LanguageElementId};
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_semantic::Mutability;
use cairo_lang_syntax::node::ast;

use crate::db::LoweringGroup;
use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnosticKind, LoweringDiagnostics};

pub fn inline_diagnostics(
    db: &dyn LoweringGroup,
    function_id: FunctionWithBodyId,
) -> Maybe<Arc<Diagnostics<LoweringDiagnostic>>> {
    let defs_db = db.upcast();
    let mut diagnostics = LoweringDiagnostics::new(function_id.module_file(defs_db));

    for attr in db.function_with_body_attributes(function_id)?.iter() {
        if attr.id != "inline" {
            continue;
        }

        // TODO: move this logic to semantic/plugin
        match &attr.args[..] {
            [ast::Expr::Path(path)] => {
                if &path.node.get_text(db.upcast()) != "always" {
                    diagnostics.report(
                        path.node.stable_ptr(),
                        LoweringDiagnosticKind::InliningFailed {
                            reason: "Unexpected inline parameter.".to_string(),
                        },
                    );
                    continue;
                }
            }
            _ => {
                diagnostics.report(
                    function_id.untyped_stable_ptr(defs_db),
                    LoweringDiagnosticKind::InliningFailed {
                        reason: "Unexpected inline configuration.".to_string(),
                    },
                );
                continue;
            }
        }

        // TODO(ilya): Handle duplicate inline attribute.

        if db
            .function_with_body_direct_function_with_body_callees(function_id)?
            .contains(&function_id)
            // TODO(ilya): Relax requirement, if one of the functions is does not have
            //  #inline(always) than we can inline it.
            || db.function_with_body_scc(function_id).len() > 1
        {
            diagnostics.report(
                function_id.untyped_stable_ptr(defs_db),
                LoweringDiagnosticKind::InliningFailed {
                    reason: "Cannot inline a function that might call itself.".to_string(),
                },
            );
        }

        if !db.function_with_body_all_implicits_vec(function_id)?.is_empty() {
            diagnostics.report(
                function_id.untyped_stable_ptr(defs_db),
                LoweringDiagnosticKind::InliningFailed {
                    reason: "Cannot inline a function with implicit arguments.".to_string(),
                },
            );
        }

        if db
            .function_with_body_signature(function_id)?
            .params
            .iter()
            .any(|param| param.mutability == Mutability::Reference)
        {
            diagnostics.report(
                function_id.untyped_stable_ptr(defs_db),
                LoweringDiagnosticKind::InliningFailed {
                    reason: "Cannot inline a function with ref parameters.".to_string(),
                },
            );
        }
    }

    Ok(Arc::new(diagnostics.build()))
}
