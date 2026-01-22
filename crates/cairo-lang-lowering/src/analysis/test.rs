//! Tests for the dataflow analysis framework.

use std::collections::HashSet;

use cairo_lang_semantic::test_utils::setup_test_function;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use super::core::{DataflowAnalyzer, Direction, StatementLocation};
use super::forward::ForwardDataflowAnalysis;
use crate::db::LoweringGroup;
use crate::ids::FunctionWithBodyLongId;
use crate::test_utils::LoweringDatabaseForTesting;
use crate::{Block, BlockEnd, BlockId, Lowered};

// ============================================================================
// Block-level Analysis: Count blocks (demonstrates transfer_block override)
// ============================================================================

/// A simple block-level analyzer that counts blocks.
/// Demonstrates overriding transfer_block for coarse-grained analysis.
#[derive(Default)]
struct BlockCounter {
    block_count: usize,
}

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for BlockCounter {
    type Info = usize; // Count of blocks seen

    const DIRECTION: Direction = Direction::Forward;

    fn initial_info(&mut self, _block_id: BlockId, _block_end: &'a BlockEnd<'db>) -> Self::Info {
        0
    }

    fn merge(
        &mut self,
        _lowered: &Lowered<'db>,
        _statement_location: StatementLocation,
        info1: Self::Info,
        info2: Self::Info,
    ) -> Self::Info {
        info1.max(info2)
    }

    // Override transfer_block for block-level analysis (no statement iteration)
    fn transfer_block(
        &mut self,
        info: &mut Self::Info,
        _block_id: BlockId,
        _block: &'a Block<'db>,
    ) {
        self.block_count += 1;
        *info += 1;
    }
}

// ============================================================================
// Statement-level Analysis: Track reachable blocks (uses default transfer_block)
// ============================================================================

/// A simple forward analyzer that tracks which blocks are reachable.
/// Demonstrates using default transfer_block with statement-level transfer_stmt.
#[derive(Default)]
struct ReachabilityAnalyzer {
    reachable_blocks: HashSet<BlockId>,
}

impl<'db, 'a> DataflowAnalyzer<'db, 'a> for ReachabilityAnalyzer {
    type Info = HashSet<BlockId>; // Set of blocks visited to reach this point

    const DIRECTION: Direction = Direction::Forward;

    fn initial_info(&mut self, _block_id: BlockId, _block_end: &'a BlockEnd<'db>) -> Self::Info {
        HashSet::new()
    }

    fn merge(
        &mut self,
        _lowered: &Lowered<'db>,
        _statement_location: StatementLocation,
        info1: Self::Info,
        info2: Self::Info,
    ) -> Self::Info {
        // Union of two reachability sets
        let mut result = info1;
        result.extend(info2);
        result
    }

    // Uses default transfer_block which iterates statements.
    // transfer_stmt is no-op (default) since reachability doesn't change at statements.

    fn visit_block_start(&mut self, info: &mut Self::Info, block_id: BlockId, _block: &Block<'db>) {
        self.reachable_blocks.insert(block_id);
        info.insert(block_id);
    }
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_block_level_analysis() {
    let db = LoweringDatabaseForTesting::default();
    let inputs = OrderedHashMap::from([
        (
            "function_code".to_string(),
            "fn foo(x: bool) -> felt252 { if x { 1 } else { 2 } }".to_string(),
        ),
        ("function_name".to_string(), "foo".to_string()),
        ("module_code".to_string(), "".to_string()),
    ]);
    let (test_function, _) = setup_test_function(&db, &inputs).split();
    let lowered = db
        .function_with_body_lowering(
            FunctionWithBodyLongId::Semantic(test_function.function_id).intern(&db),
        )
        .unwrap();

    let analyzer = BlockCounter::default();
    let mut analysis = ForwardDataflowAnalysis::new(lowered, analyzer);
    let _ = analysis.run();

    // Block-level analyzer should have counted multiple blocks
    assert!(
        analysis.analyzer.block_count >= 2,
        "Expected at least 2 blocks, got {}",
        analysis.analyzer.block_count
    );
}

#[test]
fn test_forward_single_block() {
    let db = LoweringDatabaseForTesting::default();
    let inputs = OrderedHashMap::from([
        ("function_code".to_string(), "fn foo() {}".to_string()),
        ("function_name".to_string(), "foo".to_string()),
        ("module_code".to_string(), "".to_string()),
    ]);
    let (test_function, _) = setup_test_function(&db, &inputs).split();
    let lowered = db
        .function_with_body_lowering(
            FunctionWithBodyLongId::Semantic(test_function.function_id).intern(&db),
        )
        .unwrap();

    let analyzer = ReachabilityAnalyzer::default();
    let mut analysis = ForwardDataflowAnalysis::new(lowered, analyzer);
    let _ = analysis.run();

    // Should have visited at least the root block
    assert!(!analysis.analyzer.reachable_blocks.is_empty());
    assert!(analysis.analyzer.reachable_blocks.contains(&BlockId::root()));
}

#[test]
fn test_forward_with_branching() {
    let db = LoweringDatabaseForTesting::default();
    // A function with branching creates multiple blocks
    let inputs = OrderedHashMap::from([
        (
            "function_code".to_string(),
            "fn foo(x: bool) -> felt252 { if x { 1 } else { 2 } }".to_string(),
        ),
        ("function_name".to_string(), "foo".to_string()),
        ("module_code".to_string(), "".to_string()),
    ]);
    let (test_function, _) = setup_test_function(&db, &inputs).split();
    let lowered = db
        .function_with_body_lowering(
            FunctionWithBodyLongId::Semantic(test_function.function_id).intern(&db),
        )
        .unwrap();

    let analyzer = ReachabilityAnalyzer::default();
    let mut analysis = ForwardDataflowAnalysis::new(lowered, analyzer);
    let exit_info = analysis.run().clone();

    // With branching, should visit multiple blocks
    assert!(
        analysis.analyzer.reachable_blocks.len() >= 2,
        "Expected at least 2 reachable blocks with branching"
    );

    // All processed blocks should have exit info
    for block_id in &analysis.analyzer.reachable_blocks {
        assert!(exit_info[block_id.0].is_some(), "Block {:?} should have exit info", block_id);
    }
}
