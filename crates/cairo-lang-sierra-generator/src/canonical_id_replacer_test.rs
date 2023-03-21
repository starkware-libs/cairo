use cairo_lang_sierra::ProgramParser;
use indoc::indoc;
use pretty_assertions::assert_eq;

use super::CanonicalReplacer;
use crate::replace_ids::SierraIdReplacer;

#[test]
fn test_replacer() {
    let input = ProgramParser::new()
        .parse(indoc! {"
            type felt252 = felt252;
            type NonZeroFelt252 = NonZero<felt252>;
            type BoxFelt252 = Box<felt252>;

            libfunc finalize_locals = finalize_locals;
            libfunc felt252_add = felt252_add;
            libfunc felt252_mul_2 = felt252_mul_const<2>;
            libfunc felt252_sub = felt252_sub;
            libfunc felt252_dup = dup<felt252>;
            libfunc felt252_is_zero = felt252_is_zero;
            libfunc store_temp_felt252 = store_temp<felt252>;
            libfunc call_foo = function_call<user@foo>;

            felt252_dup([2]) -> ([2], [5]);
            felt252_add([1], [2]) -> ([3]);
            store_temp_felt252([3]) -> ([4]);
            store_temp_felt252([5]) -> ([5]);
            store_temp_felt252([4]) -> ([4]);
            call_foo([5], [4]) -> ([7], [8]);
            felt252_dup([8]) -> ([4], [8]);
            store_temp_felt252([4]) -> ([4]);
            return([7], [8], [4]);
            finalize_locals() -> ();

            test_program@0([1]: felt252, [2]: felt252) -> (felt252, felt252, felt252);
            foo@10([1]: felt252, [2]: felt252) -> (felt252, felt252);
            box_and_back@26([1]: felt252) -> (felt252);
            box_and_back_wrapper@31([1]: felt252) -> (felt252);
        "})
        .unwrap();

    let expected_output = ProgramParser::new()
        .parse(indoc! {"
            type [0] = felt252;
            type [1] = NonZero<[0]>;
            type [2] = Box<[0]>;

            libfunc [0] = finalize_locals;
            libfunc [1] = felt252_add;
            libfunc [2] = felt252_mul_const<2>;
            libfunc [3] = felt252_sub;
            libfunc [4] = dup<[0]>;
            libfunc [5] = felt252_is_zero;
            libfunc [6] = store_temp<[0]>;
            libfunc [7] = function_call<user@[1]>;

            [4]([2]) -> ([2], [5]);
            [1]([1], [2]) -> ([3]);
            [6]([3]) -> ([4]);
            [6]([5]) -> ([5]);
            [6]([4]) -> ([4]);
            [7]([5], [4]) -> ([7], [8]);
            [4]([8]) -> ([4], [8]);
            [6]([4]) -> ([4]);
            return([7], [8], [4]);
            [0]() -> ();

            [0]@0([1]: [0], [2]: [0]) -> ([0], [0], [0]);
            [1]@10([1]: [0], [2]: [0]) -> ([0], [0]);
            [2]@26([1]: [0]) -> ([0]);
            [3]@31([1]: [0]) -> ([0]);
        "})
        .unwrap();

    let replacer = CanonicalReplacer::from_program(&input);

    assert_eq!(replacer.apply(&input), expected_output);
}
