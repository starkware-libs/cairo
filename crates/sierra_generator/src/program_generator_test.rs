use indoc::indoc;
use pretty_assertions::assert_eq;
use semantic::test_utils::setup_test_module;

use crate::db::SierraGenGroup;
use crate::test_utils::{replace_libfunc_ids_in_program, SierraGenDatabaseForTesting};

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

    db.module_sierra_diagnostics(module_id).expect("");
    let program = &*db.module_sierra_program(module_id).unwrap();
    // TODO(lior): Remove the unnecessary store_temp()s at the end.
    assert_eq!(
        replace_libfunc_ids_in_program(&db, program).to_string(),
        indoc! {"
            type [0] = felt;

            libfunc felt_drop = felt_drop;
            libfunc revoke_ap_tracking = revoke_ap_tracking;
            libfunc felt_const<5> = felt_const<5>;
            libfunc store_temp<[0]> = store_temp<[0]>;
            libfunc function_call<user@[0]> = function_call<user@[0]>;
            libfunc felt_dup = felt_dup;
            libfunc felt_add = felt_add;
            libfunc rename<[0]> = rename<[0]>;

            felt_drop([0]) -> ();
            revoke_ap_tracking() -> ();
            felt_const<5>() -> ([1]);
            store_temp<[0]>([1]) -> ([2]);
            function_call<user@[0]>([2]) -> ([3]);
            store_temp<[0]>([3]) -> ([4]);
            return([4]);
            revoke_ap_tracking() -> ();
            felt_dup([0]) -> ([0], [3]);
            felt_add([0], [3]) -> ([1]);
            store_temp<[0]>([1]) -> ([1]);
            rename<[0]>([1]) -> ([2]);
            return([2]);

            [1]@0([0]: [0]) -> ([0]);
            [0]@7([0]: [0]) -> ([0]);
        "},
    );
}

#[test]
fn test_type_dependency() {
    let mut db = SierraGenDatabaseForTesting::default();
    let module_id = setup_test_module(
        &mut db,
        indoc! {"
                func deref_twice(a: Ref::<Ref::<Ref::<felt>>>) -> Ref::<felt> {
                    deref::<Ref::<felt>>(deref::<Ref::<Ref::<felt>>>(a))
                }
            "},
    )
    .unwrap()
    .module_id;

    db.module_sierra_diagnostics(module_id).expect("");
    let program = &*db.module_sierra_program(module_id).unwrap();
    assert_eq!(
        replace_libfunc_ids_in_program(&db, program).to_string(),
        indoc! {"
            type [0] = felt;
            type [1] = Ref<[0]>;
            type [2] = Ref<[1]>;
            type [3] = Ref<[2]>;

            libfunc revoke_ap_tracking = revoke_ap_tracking;
            libfunc deref<[2]> = deref<[2]>;
            libfunc store_temp<[2]> = store_temp<[2]>;
            libfunc deref<[1]> = deref<[1]>;
            libfunc store_temp<[1]> = store_temp<[1]>;
            libfunc rename<[1]> = rename<[1]>;

            revoke_ap_tracking() -> ();
            deref<[2]>([0]) -> ([1]);
            store_temp<[2]>([1]) -> ([1]);
            deref<[1]>([1]) -> ([2]);
            store_temp<[1]>([2]) -> ([2]);
            rename<[1]>([2]) -> ([3]);
            return([3]);

            [0]@0([0]: [3]) -> ([1]);
        "},
    );
}
