use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;

use crate::test_utils::checked_compile_to_sierra;

#[test]
fn test_program_generator() {
    // TODO(lior): Make bar return something like felt_add(5, bar()).
    let program = checked_compile_to_sierra(indoc! {"
                func foo(a: felt) -> felt {
                    bar(5)
                }

                func bar(a: felt) -> felt {
                    felt_add(a, a)
                }
            "});

    // TODO(lior): Remove the unnecessary store_temp()s at the end.
    assert_eq!(
        program.to_string(),
        indoc! {"
            type felt = felt;

            libfunc drop<felt> = drop<felt>;
            libfunc revoke_ap_tracking = revoke_ap_tracking;
            libfunc felt_const<5> = felt_const<5>;
            libfunc store_temp<felt> = store_temp<felt>;
            libfunc function_call<user@test_crate::bar> = function_call<user@test_crate::bar>;
            libfunc rename<felt> = rename<felt>;
            libfunc burn_gas = burn_gas;
            libfunc dup<felt> = dup<felt>;
            libfunc felt_add = felt_add;

            drop<felt>([0]) -> ();
            revoke_ap_tracking() -> ();
            felt_const<5>() -> ([1]);
            store_temp<felt>([1]) -> ([3]);
            function_call<user@test_crate::bar>([3]) -> ([2]);
            rename<felt>([2]) -> ([4]);
            burn_gas() -> ();
            return([4]);
            revoke_ap_tracking() -> ();
            dup<felt>([0]) -> ([0], [3]);
            felt_add([0], [3]) -> ([1]);
            store_temp<felt>([1]) -> ([1]);
            rename<felt>([1]) -> ([2]);
            burn_gas() -> ();
            return([2]);

            test_crate::foo@0([0]: felt) -> (felt);
            test_crate::bar@8([0]: felt) -> (felt);
        "},
    );
}

#[test]
fn test_type_dependency() {
    let program = checked_compile_to_sierra(indoc! {"
                func unbox_twice(a: Box::<Box::<Box::<felt>>>) -> Box::<felt> {
                    unbox::<Box::<felt>>(unbox::<Box::<Box::<felt>>>(a))
                }
            "});

    assert_eq!(
        program.to_string(),
        indoc! {"
            type felt = felt;
            type Box<felt> = Box<felt>;
            type Box<Box<felt>> = Box<Box<felt>>;
            type Box<Box<Box<felt>>> = Box<Box<Box<felt>>>;

            libfunc revoke_ap_tracking = revoke_ap_tracking;
            libfunc unbox<Box<Box<felt>>> = unbox<Box<Box<felt>>>;
            libfunc store_temp<Box<Box<felt>>> = store_temp<Box<Box<felt>>>;
            libfunc unbox<Box<felt>> = unbox<Box<felt>>;
            libfunc store_temp<Box<felt>> = store_temp<Box<felt>>;
            libfunc rename<Box<felt>> = rename<Box<felt>>;
            libfunc burn_gas = burn_gas;

            revoke_ap_tracking() -> ();
            unbox<Box<Box<felt>>>([0]) -> ([1]);
            store_temp<Box<Box<felt>>>([1]) -> ([1]);
            unbox<Box<felt>>([1]) -> ([2]);
            store_temp<Box<felt>>([2]) -> ([2]);
            rename<Box<felt>>([2]) -> ([3]);
            burn_gas() -> ();
            return([3]);

            test_crate::unbox_twice@0([0]: Box<Box<Box<felt>>>) -> (Box<felt>);
        "},
    );
}
