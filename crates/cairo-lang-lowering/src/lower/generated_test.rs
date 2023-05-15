use std::fmt::Write;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    inlining,
    "src/lower/test_data",
    {
        loop_ :"loop",
    },
    test_generated_function
);

fn test_generated_function(
    inputs: &OrderedHashMap<String, String>,
) -> OrderedHashMap<String, String> {
    let db = &mut LoweringDatabaseForTesting::default();
    db.set_semantic_plugins(get_default_plugins());
    let (test_function, semantic_diagnostics) = setup_test_function(
        db,
        inputs["function"].as_str(),
        inputs["function_name"].as_str(),
        inputs["module_code"].as_str(),
    )
    .split();

    let mut writer = String::new();
    if let Ok(multi_lowering) = db.priv_function_with_body_multi_lowering(test_function.function_id)
    {
        writeln!(&mut writer, "Main:").unwrap();
        writeln!(
            &mut writer,
            "{:?}",
            multi_lowering.main_lowering.debug(&LoweredFormatter {
                db,
                variables: &multi_lowering.main_lowering.variables
            })
        )
        .unwrap();

        for (_, lowering) in multi_lowering.generated_lowerings.iter() {
            writeln!(&mut writer, "Generated:").unwrap();
            writeln!(
                &mut writer,
                "{:?}",
                lowering.debug(&LoweredFormatter { db, variables: &lowering.variables })
            )
            .unwrap();
        }
    }

    let lowering_diagnostics =
        db.module_lowering_diagnostics(test_function.module_id).unwrap_or_default();

    OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering".into(), writer),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ])
}
