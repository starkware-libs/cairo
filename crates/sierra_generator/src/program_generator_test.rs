use defs::db::DefsGroup;
use defs::ids::ModuleItemId;
use indoc::indoc;
use itertools::Itertools;
use pretty_assertions::assert_eq;
use test_case::test_case;
use test_log::test;
use utils::try_extract_matches;

use crate::db::SierraGenGroup;
use crate::replace_ids::replace_sierra_ids_in_program;
use crate::test_utils::{checked_compile_to_sierra, setup_db_and_get_crate_id};

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
            libfunc felt_const<5> = felt_const<5>;
            libfunc store_temp<felt> = store_temp<felt>;
            libfunc function_call<user@test::bar> = function_call<user@test::bar>;
            libfunc rename<felt> = rename<felt>;
            libfunc dup<felt> = dup<felt>;
            libfunc felt_add = felt_add;

            drop<felt>([0]) -> ();
            felt_const<5>() -> ([1]);
            store_temp<felt>([1]) -> ([3]);
            function_call<user@test::bar>([3]) -> ([2]);
            rename<felt>([2]) -> ([4]);
            return([4]);
            dup<felt>([0]) -> ([0], [3]);
            felt_add([0], [3]) -> ([1]);
            store_temp<felt>([1]) -> ([1]);
            rename<felt>([1]) -> ([2]);
            return([2]);

            test::foo@0([0]: felt) -> (felt);
            test::bar@6([0]: felt) -> (felt);
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

            libfunc unbox<Box<Box<felt>>> = unbox<Box<Box<felt>>>;
            libfunc store_temp<Box<Box<felt>>> = store_temp<Box<Box<felt>>>;
            libfunc unbox<Box<felt>> = unbox<Box<felt>>;
            libfunc store_temp<Box<felt>> = store_temp<Box<felt>>;
            libfunc rename<Box<felt>> = rename<Box<felt>>;

            unbox<Box<Box<felt>>>([0]) -> ([1]);
            store_temp<Box<Box<felt>>>([1]) -> ([1]);
            unbox<Box<felt>>([1]) -> ([2]);
            store_temp<Box<felt>>([2]) -> ([2]);
            rename<Box<felt>>([2]) -> ([3]);
            return([3]);

            test::unbox_twice@0([0]: Box<Box<Box<felt>>>) -> (Box<felt>);
        "},
    );
}

#[test_case(
    "f1",
    &[
        "test::f1", "test::f2", "test::f3",
        "test::f4", "test::f5", "test::f6",
    ];
    "finds all"
)]
#[test_case(
    "f2",
    &[
        "test::f2", "test::f3", "test::f4", "test::f5", "test::f6",
    ];
    "all but first"
)]
#[test_case("f3", &["test::f3", "test::f5", "test::f6"]; "f3 -> f5 -> f6")]
#[test_case("f4", &["test::f4", "test::f5", "test::f6"]; "f4 -> (f5 -> f6, f6)")]
#[test_case("f5", &["test::f5", "test::f6"]; "f5 -> f6")]
#[test_case("f6", &["test::f6"]; "self loop")]
fn test_only_include_dependecies(func_name: &str, sierra_used_funcs: &[&str]) {
    let (db, crate_id) = setup_db_and_get_crate_id(indoc! {"
        func f1() { f2(); f3(); }
        func f2() { f3(); f4(); f5(); }
        func f3() { f5(); }
        func f4() { f5(); f6(); }
        func f5() { f6(); }
        func f6() { f6(); }
    "});
    let func_id = db
        .crate_modules(crate_id)
        .iter()
        .find_map(|module_id| {
            try_extract_matches!(
                db.module_item_by_name(*module_id, func_name.into()).unwrap().unwrap(),
                ModuleItemId::FreeFunction
            )
        })
        .unwrap();
    let program = db.get_sierra_program_for_functions(vec![func_id]).unwrap();
    assert_eq!(
        replace_sierra_ids_in_program(&db, &program)
            .funcs
            .into_iter()
            .map(|f| f.id.to_string())
            .collect_vec(),
        sierra_used_funcs
    );
}
