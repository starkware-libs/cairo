use indoc::indoc;
use pretty_assertions::assert_eq;
use semantic::test_utils::setup_test_module;

use crate::db::SierraGenGroup;
use crate::test_utils::SierraGenDatabaseForTesting;

#[test]
fn test_program_generator() {
    let mut db = SierraGenDatabaseForTesting::default();
    // TODO(lior): Make bar return something like felt_add(5, bar()).
    let module_id = setup_test_module(
        &mut db,
        indoc! {"
                func foo(a: felt) -> felt {
                    bar(5)
                }

                func bar(a: felt) -> felt {
                    felt_add(a, a)
                }
            "},
    )
    .unwrap()
    .module_id;

    let program = &*db.get_program_code(module_id).expect("").unwrap();
    assert_eq!(
        program.to_string(),
        indoc! {"
            type [0] = felt;
            type [1] = NonZero<[0]>;

            libfunc [3] = felt_ignore;
            libfunc [0] = felt_const<5>;
            libfunc [1] = store_temp<[0]>;
            libfunc [2] = function_call<user@[0]>;
            libfunc [5] = felt_dup;
            libfunc [4] = felt_add;

            [3]([0]) -> ();
            [0]() -> ([1]);
            [1]([1]) -> ([2]);
            [2]([2]) -> ([3]);
            [1]([3]) -> ([4]);
            return([4]);
            [5]([0]) -> ([0], [4]);
            [4]([0], [4]) -> ([1]);
            [1]([1]) -> ([2]);
            [1]([2]) -> ([3]);
            return([3]);

            [1]@0([0]: [0]) -> ([0]);
            [0]@6([0]: [0]) -> ([0]);
        "},
    );
}
