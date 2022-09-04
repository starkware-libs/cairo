use indoc::indoc;
use pretty_assertions::assert_eq;
use semantic::test_utils::setup_test_module;

use crate::db::SierraGenGroup;
use crate::test_utils::DatabaseImpl;

#[test]
fn test_program_generator() {
    let mut db = DatabaseImpl::default();
    // TODO(lior): Make bar return something like felt_add(5, bar()).
    let module_id = setup_test_module(
        &mut db,
        indoc! {"
                extern type felt;

                func foo(a: felt) -> felt {
                    bar(5)
                }

                func bar(a: felt) -> felt {
                    5
                }
            "},
    );

    let program = &*db.get_program_code(module_id).expect("").unwrap();
    assert_eq!(
        program.to_string(),
        indoc! {"


            [0]() -> ([0]);
            [1]([0]) -> ([1]);
            [2]([1]) -> ([2]);
            [1]([2]) -> ([2]);
            return([2]);
            [0]() -> ([0]);
            [1]([0]) -> ([0]);
            return([0]);

            [1]@1234() -> ();
            [0]@1234() -> ();
        "},
    );
}
