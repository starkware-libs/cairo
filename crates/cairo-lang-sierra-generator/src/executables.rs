use std::collections::HashMap;

use cairo_lang_diagnostics::ToOption;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::Program;
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
) -> HashMap<SmolStr, Vec<FunctionId>> {
    let mut result = HashMap::new();
    for crate_id in db.crates() {
        for module in db.crate_modules(crate_id).iter() {
            if let Some(free_functions) = db.module_free_functions(*module).to_option() {
                for (free_func_id, body) in free_functions.iter() {
                    // Note this is a Vec since a single function can have multiple
                    // `#[executable(..)]` attributes.
                    let args: Vec<SmolStr> = body
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
                        for arg in args {
                            result.entry(arg).or_insert_with(Vec::new).push(function.id.clone());
                        }
                    }
                }
            }
        }
    }
    result
}

/// Parse the argument of the `#[executable('key')]` attribute.
fn parse_arg(db: &dyn SierraGenGroup, args: Vec<AttributeArg>) -> Option<SmolStr> {
    if args.is_empty() {
        return Some(SmolStr::new("main"));
    }
    args.first().map(|arg| {
        assert!(matches!(arg.variant, AttributeArgVariant::Unnamed { .. }));
        let text = arg.text(db.upcast());
        SmolStr::new(text)
    })
}
