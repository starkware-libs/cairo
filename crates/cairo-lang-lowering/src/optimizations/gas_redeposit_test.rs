use std::ops::Deref;
use std::sync::Arc;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_filesystem::db::FilesGroupEx;
use cairo_lang_filesystem::flag::Flag;
use cairo_lang_filesystem::ids::FlagId;
use cairo_lang_semantic::test_utils::TestFunction;
use cairo_lang_test_utils::parse_test_file::{TestFileRunner, TestRunnerResult};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::gas_redeposit;
use crate::db::LoweringGroup;
use crate::fmt::LoweredFormatter;
use crate::ids::ConcreteFunctionWithBodyId;
use crate::test_utils::LoweringDatabaseForTesting;

cairo_lang_test_utils::test_file_test_with_runner!(
    gas_redeposit,
    "src/optimizations/test_data",
    {
        gas_redeposit: "gas_redeposit",
    },
    GetRedepositTestRunner
);

struct GetRedepositTestRunner {
    db: LoweringDatabaseForTesting,
}
impl Default for GetRedepositTestRunner {
    fn default() -> Self {
        let mut db = LoweringDatabaseForTesting::new();
        let flag = FlagId::new(&db, "add_redeposit_gas");
        db.set_flag(flag, Some(Arc::new(Flag::AddRedepositGas(true))));
        Self { db }
    }
}

impl TestFileRunner for GetRedepositTestRunner {
    fn run(
        &mut self,
        inputs: &OrderedHashMap<String, String>,
        _args: &OrderedHashMap<String, String>,
    ) -> TestRunnerResult {
        let db = &self.db;
        let (test_function, semantic_diagnostics) = TestFunction::builder(
            db,
            inputs["function"].as_str(),
            inputs["function_name"].as_str(),
            inputs["module_code"].as_str(),
            None,
        )
        .build_and_check_for_diagnostics(db)
        .split();
        let function_id =
            ConcreteFunctionWithBodyId::from_semantic(db, test_function.concrete_function_id);

        let before =
            db.concrete_function_with_body_postpanic_lowered(function_id).unwrap().deref().clone();

        let lowering_diagnostics = db.module_lowering_diagnostics(test_function.module_id).unwrap();

        let mut after = before.clone();
        gas_redeposit(db, function_id, &mut after);

        TestRunnerResult::success(OrderedHashMap::from([
            ("semantic_diagnostics".into(), semantic_diagnostics),
            (
                "before".into(),
                format!("{:?}", before.debug(&LoweredFormatter::new(db, &before.variables))),
            ),
            (
                "after".into(),
                format!("{:?}", after.debug(&LoweredFormatter::new(db, &after.variables))),
            ),
            ("lowering_diagnostics".into(), lowering_diagnostics.format(db)),
        ]))
    }
}
