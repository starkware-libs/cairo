use cairo_lang_diagnostics::ToOption;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::program::{Function, Program};
use cairo_lang_syntax::attribute::consts::EXECUTABLE_ATTR;
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::helpers::QueryAttrs;
use smol_str::SmolStr;

use crate::db::SierraGenGroup;

/// Collect functions marked as executable with `#[executable('key')]` attribute.
pub fn collect_executables(
    db: &dyn SierraGenGroup,
    sierra_program: &Program,
) -> Vec<(Function, SmolStr)> {
    let mut result = Vec::new();
    for crate_id in db.crates() {
        for module in db.crate_modules(crate_id).iter() {
            if let Some(free_functions) = db.module_free_functions(*module).to_option() {
                for (free_func_id, body) in free_functions.iter() {
                    // Note this is a Vec since a single function can have multiple
                    // `#[executable(..)]` attributes.
                    let attrs: Vec<_> = body
                        .query_attr(db.upcast(), EXECUTABLE_ATTR)
                        .iter()
                        .filter_map(|attr| {
                            let attr = (*attr).clone();
                            let attr = attr.structurize(db.upcast());
                            parse_arg(db, attr.args)
                        })
                        .collect();
                    // Find function corresponding to the node by full path.
                    let function = sierra_program.funcs.iter().find(|f| {
                        ConcreteFunctionWithBodyId::from_no_generics_free(
                            db.upcast(),
                            *free_func_id,
                        )
                        .and_then(|function_id| {
                            function_id
                                .function_id(db.upcast())
                                .to_option()
                                .map(|function_id| f.id == db.intern_sierra_function(function_id))
                        })
                        .unwrap_or_default()
                    });
                    if let Some(function) = function {
                        result.extend(attrs.into_iter().map(|arg| (function.clone(), arg)));
                    }
                }
            }
        }
    }
    result
}

/// Parse the argument of the `#[executable('key')]` attribute.
fn parse_arg(db: &dyn SierraGenGroup, args: Vec<AttributeArg>) -> Option<SmolStr> {
    args.first().map(|arg| {
        assert!(matches!(arg.variant, AttributeArgVariant::Unnamed { .. }));
        let text = arg.text(db.upcast());
        SmolStr::new(text)
    })
}
