use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleItemId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_utils::{arc_unwrap_or_clone, try_extract_matches};
use indoc::indoc;
use itertools::Itertools;
use pretty_assertions::assert_eq;
use test_case::test_case;
use test_log::test;

use crate::db::SierraGenGroup;
use crate::program_generator::SierraProgramWithDebug;
use crate::replace_ids::replace_sierra_ids_in_program;
use crate::test_utils::{checked_compile_to_sierra, setup_db_and_get_crate_id};

#[test]
fn test_program_generator() {
    // TODO(lior): Make bar return something like felt252_add(5, bar()).
    let program = checked_compile_to_sierra(indoc! {"
                fn foo(a: felt252) -> felt252 {
                    bar(5)
                }

                #[inline(never)]
                fn bar(a: felt252) -> felt252 {
                    felt252_add(felt252_add(a, a), a)
                }
            "});

    // TODO(lior): Remove the unnecessary store_temp()s at the end.
    assert_eq!(
        program.to_string(),
        indoc! {"
            type felt252 = felt252 [storable: true, drop: true, dup: true, zero_sized: false];

            libfunc drop<felt252> = drop<felt252>;
            libfunc felt252_const<5> = felt252_const<5>;
            libfunc store_temp<felt252> = store_temp<felt252>;
            libfunc function_call<user@test::bar> = function_call<user@test::bar>;
            libfunc dup<felt252> = dup<felt252>;
            libfunc felt252_add = felt252_add;

            drop<felt252>([0]) -> (); // 0
            felt252_const<5>() -> ([1]); // 1
            store_temp<felt252>([1]) -> ([1]); // 2
            function_call<user@test::bar>([1]) -> ([2]); // 3
            return([2]); // 4
            dup<felt252>([0]) -> ([0], [1]); // 5
            dup<felt252>([0]) -> ([0], [2]); // 6
            felt252_add([1], [2]) -> ([3]); // 7
            store_temp<felt252>([3]) -> ([3]); // 8
            felt252_add([3], [0]) -> ([4]); // 9
            store_temp<felt252>([4]) -> ([4]); // 10
            return([4]); // 11

            test::foo@0([0]: felt252) -> (felt252);
            test::bar@5([0]: felt252) -> (felt252);
        "},
    );
}

#[test]
fn test_type_dependency() {
    let program = checked_compile_to_sierra(indoc! {"
                use box::BoxTrait;
                fn unbox_twice(a: Box::<Box::<Box::<felt252>>>) -> Box::<felt252> {
                    a.unbox().unbox()
                }
            "});

    assert_eq!(
        program.to_string(),
        indoc! {"
            type Box<Box<Box<felt252>>> = Box<Box<Box<felt252>>> [storable: true, drop: true, dup: true, zero_sized: false];
            type Box<felt252> = Box<felt252> [storable: true, drop: true, dup: true, zero_sized: false];
            type Box<Box<felt252>> = Box<Box<felt252>> [storable: true, drop: true, dup: true, zero_sized: false];
            type felt252 = felt252 [storable: true, drop: true, dup: true, zero_sized: false];

            libfunc unbox<Box<Box<felt252>>> = unbox<Box<Box<felt252>>>;
            libfunc store_temp<Box<Box<felt252>>> = store_temp<Box<Box<felt252>>>;
            libfunc unbox<Box<felt252>> = unbox<Box<felt252>>;
            libfunc store_temp<Box<felt252>> = store_temp<Box<felt252>>;

            unbox<Box<Box<felt252>>>([0]) -> ([1]); // 0
            store_temp<Box<Box<felt252>>>([1]) -> ([1]); // 1
            unbox<Box<felt252>>([1]) -> ([2]); // 2
            store_temp<Box<felt252>>([2]) -> ([2]); // 3
            return([2]); // 4

            test::unbox_twice@0([0]: Box<Box<Box<felt252>>>) -> (Box<felt252>);
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
fn test_only_include_dependencies(func_name: &str, sierra_used_funcs: &[&str]) {
    let (db, crate_id) = setup_db_and_get_crate_id(indoc! {"
        #[inline(never)]
        fn f1() { f2(); f3(); }
        #[inline(never)]
        fn f2() { f3(); f4(); f5(); }
        #[inline(never)]
        fn f3() { f5(); }
        #[inline(never)]
        fn f4() { f5(); f6(); }
        #[inline(never)]
        fn f5() { f6(); }
        #[inline(never)]
        fn f6() { f6(); }
    "});
    let func_id = ConcreteFunctionWithBodyId::from_no_generics_free(
        &db,
        db.crate_modules(crate_id)
            .iter()
            .find_map(|module_id| {
                try_extract_matches!(
                    db.module_item_by_name(*module_id, func_name.into()).unwrap().unwrap(),
                    ModuleItemId::FreeFunction
                )
            })
            .unwrap(),
    )
    .unwrap();
    let SierraProgramWithDebug { program, .. } =
        arc_unwrap_or_clone(db.get_sierra_program_for_functions(vec![func_id]).unwrap());
    assert_eq!(
        replace_sierra_ids_in_program(&db, &program)
            .funcs
            .into_iter()
            .map(|f| f.id.to_string())
            .collect_vec(),
        sierra_used_funcs
    );
}
