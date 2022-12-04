use indoc::indoc;
use pretty_assertions::assert_eq;
use sierra::ProgramParser;

use super::CanonicalReplacer;
use crate::replace_ids::SierraIdReplacer;

#[test]
fn test_replacer() {
    let input = indoc! {"
            type felt = felt;
            type NonZeroFelt = NonZero<felt>;
            type BoxFelt = Box<felt>;

            libfunc finalize_locals = finalize_locals;
            libfunc felt_add = felt_add;
            libfunc felt_mul_2 = felt_mul<2>;
            libfunc felt_sub = felt_sub;
            libfunc felt_dup = dup<felt>;
            libfunc felt_jump_nz = felt_jump_nz;
            libfunc store_temp_felt = store_temp<felt>;
            libfunc call_foo = function_call<user@foo>;

            felt_dup([2]) -> ([2], [5]);
            felt_add([1], [2]) -> ([3]);
            store_temp_felt([3]) -> ([4]);
            store_temp_felt([5]) -> ([5]);
            store_temp_felt([4]) -> ([4]);
            call_foo([5], [4]) -> ([7], [8]);
            felt_dup([8]) -> ([4], [8]);
            store_temp_felt([4]) -> ([4]);
            return([7], [8], [4]);
            finalize_locals() -> ();

            test_program@0([1]: felt, [2]: felt) -> (felt, felt, felt);
            foo@10([1]: felt, [2]: felt) -> (felt, felt);
            box_and_back@26([1]: felt) -> (felt);
            box_and_back_wrapper@31([1]: felt) -> (felt);
        "};

    let expecetd_output = indoc! {"
            type [0] = felt;
            type [1] = NonZero<[0]>;
            type [2] = Box<[0]>;

            libfunc [0] = finalize_locals;
            libfunc [1] = felt_add;
            libfunc [2] = felt_mul<2>;
            libfunc [3] = felt_sub;
            libfunc [4] = dup<[0]>;
            libfunc [5] = felt_jump_nz;
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
        "};

    let program = ProgramParser::new().parse(input).unwrap();

    let replacer = CanonicalReplacer::from_program(&program);

    assert_eq!(format!("{}", replacer.apply(&program)), expecetd_output)
}
