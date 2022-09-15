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

            libfunc [4] = felt_drop;
            libfunc [0] = revoke_ap_tracking;
            libfunc [1] = felt_const<5>;
            libfunc [2] = store_temp<[0]>;
            libfunc [3] = function_call<user@[0]>;
            libfunc [6] = felt_dup;
            libfunc [5] = felt_add;

            [4]([0]) -> ();
            [0]() -> ();
            [1]() -> ([1]);
            [2]([1]) -> ([2]);
            [3]([2]) -> ([3]);
            [2]([3]) -> ([4]);
            return([4]);
            [0]() -> ();
            [6]([0]) -> ([0], [4]);
            [5]([0], [4]) -> ([1]);
            [2]([1]) -> ([2]);
            [2]([2]) -> ([3]);
            return([3]);

            [1]@0([0]: [0]) -> ([0]);
            [0]@7([0]: [0]) -> ([0]);
        "},
    );
}
