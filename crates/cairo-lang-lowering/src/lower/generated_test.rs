use std::fmt::Write;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::TopLevelLanguageElementId;
use cairo_lang_diagnostics::get_location_marks;
use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::{ConcreteFunctionWithBodyId, ConcreteFunctionWithBodyLongId, GeneratedFunction};
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test!(
    generated,
    "src/lower/test_data",
    {
        closure :"closure",
        loop_ :"loop",
        while_ :"while",
        for_ :"for",
    },
    test_generated_function
);

fn test_generated_function(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &mut LoweringDatabaseForTesting::default();
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
            multi_lowering
                .main_lowering
                .debug(&LoweredFormatter::new(db, &multi_lowering.main_lowering.variables))
        )
        .unwrap();

        let lowering =
            db.final_concrete_function_with_body_lowered(
                ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id),
            )
            .unwrap();
        writeln!(
            &mut writer,
            "Final lowering:\n{:?}",
            lowering.debug(&LoweredFormatter::new(db, &lowering.variables))
        )
        .unwrap();

        for (key, lowering) in multi_lowering.generated_lowerings.iter() {
            let generated_id = ConcreteFunctionWithBodyLongId::Generated(GeneratedFunction {
                parent: test_function.concrete_function_id,
                key: *key,
            })
            .intern(db);

            let func_description = match key {
                crate::ids::GeneratedFunctionKey::Loop(_) => "loop".into(),
                crate::ids::GeneratedFunctionKey::TraitFunc(func, _) => func.full_path(db),
            };

            writeln!(
                &mut writer,
                "Generated {} lowering for source location:\n{}\n",
                func_description,
                get_location_marks(
                    db,
                    &generated_id.stable_location(db).unwrap().diagnostic_location(db),
                    true
                )
            )
            .unwrap();

            writeln!(
                &mut writer,
                "{:?}",
                lowering.debug(&LoweredFormatter::new(db, &lowering.variables))
            )
            .unwrap();

            let lowering = db.final_concrete_function_with_body_lowered(generated_id).unwrap();
            writeln!(
                &mut writer,
                "Final lowering:\n{:?}",
                lowering.debug(&LoweredFormatter::new(db, &lowering.variables))
            )
            .unwrap();
        }
    }

    let lowering_diagnostics =
        db.module_lowering_diagnostics(test_function.module_id).unwrap_or_default();

    TestRunnerResult::success(OrderedHashMap::from([
        ("semantic_diagnostics".into(), semantic_diagnostics),
        ("lowering".into(), writer),
        ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
    ]))
}
