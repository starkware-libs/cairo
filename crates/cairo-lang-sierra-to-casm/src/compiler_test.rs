use cairo_lang_sierra::ProgramParser;
use indoc::indoc;
use pretty_assertions;
use test_case::test_case;

use crate::compiler::compile;
use crate::metadata::{calc_metadata, calc_metadata_ap_change_only};
use crate::test_utils::{read_sierra_example_file, strip_comments_and_linebreaks};

#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;
                type BoxFelt252 = Box<felt252>;

                libfunc branch_align = branch_align;
                libfunc finalize_locals = finalize_locals;
                libfunc felt252_add = felt252_add;
                libfunc felt252_mul_2 = felt252_mul_const<2>;
                libfunc felt252_sub = felt252_sub;
                libfunc felt252_dup = dup<felt252>;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc felt252_into_box = into_box<felt252>;
                libfunc felt252_unbox = unbox<felt252>;
                libfunc jump = jump;
                libfunc felt252_unwrap_non_zero = unwrap_non_zero<felt252>;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc store_temp_box_felt252 = store_temp<BoxFelt252>;
                libfunc rename_felt252 = rename<felt252>;
                libfunc call_foo = function_call<user@foo>;
                libfunc disable_ap_tracking = disable_ap_tracking;

                libfunc call_box_and_back = function_call<user@box_and_back>;

                rename_felt252([1]) -> ([1]);                      // #0
                felt252_dup([2]) -> ([2], [5]);                    // #1
                felt252_add([1], [2]) -> ([3]);                    // #2
                store_temp_felt252([3]) -> ([4]);                  // #3
                store_temp_felt252([5]) -> ([5]);                  // #4
                store_temp_felt252([4]) -> ([4]);                  // #5
                call_foo([5], [4]) -> ([7], [8]);                  // #6
                felt252_dup([8]) -> ([4], [8]);                    // #7
                store_temp_felt252([4]) -> ([4]);                  // #8
                return([7], [8], [4]);                             // #9

                finalize_locals() -> ();                           // #10
                felt252_is_zero([1]) { fallthrough() 18([1]) };    // #11
                branch_align() -> ();                              // #12
                felt252_dup([2]) -> ([1], [2]);                    // #13
                store_temp_felt252([1]) -> ([1]);                  // #14
                store_temp_felt252([2]) -> ([2]);                  // #15
                disable_ap_tracking() -> ();                       // #16
                return ([1], [2]);                                 // #17

                branch_align() -> ();                              // #18
                jump() { 20() };                                   // #19
                felt252_unwrap_non_zero([1]) -> ([1]);             // #20
                felt252_dup([2]) -> ([2], [3]);                    // #21
                felt252_sub([1], [3]) -> ([1]);                    // #22
                store_temp_felt252([1]) -> ([1]);                  // #23
                felt252_mul_2([1]) -> ([1]);                       // #24
                store_temp_felt252([1]) -> ([1]);                  // #25
                store_temp_felt252([2]) -> ([2]);                  // #26
                call_foo([1], [2]) -> ([1], [2]);                  // #27
                return ([1], [2]);                                 // #28

                felt252_into_box([1]) -> ([2]);                    // #29
                store_temp_box_felt252([2]) -> ([2]);              // #30
                felt252_unbox([2]) -> ([3]);                       // #31
                store_temp_felt252([3]) -> ([3]);                  // #32
                disable_ap_tracking() -> ();                       // #33
                return ([3]);                                      // #34

                store_temp_felt252([1]) -> ([1]);                  // #35
                call_box_and_back([1]) -> ([1]);                   // #36
                return ([1]);                                      // #37

                test_program@0([1]: felt252, [2]: felt252) -> (felt252, felt252, felt252);
                foo@10([1]: felt252, [2]: felt252) -> (felt252, felt252);
                box_and_back@29([1]: felt252) -> (felt252);
                box_and_back_wrapper@35([1]: felt252) -> (felt252);
            "},
            false,
            indoc! {"
                // test_program:
                [ap + 0] = [fp + -4] + [fp + -3], ap++;
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [ap + -2], ap++;
                // call foo
                call rel 4;
                [ap + 0] = [ap + -1], ap++;
                ret;

                // foo:
                ap += 0;
                jmp rel 5 if [fp + -4] != 0;
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [fp + -3], ap++;
                ret;
                jmp rel 2;
                [fp + -4] = [ap + 0] + [fp + -3], ap++;
                [ap + 0] = [ap + -1] * 2, ap++;
                [ap + 0] = [fp + -3], ap++;
                call rel -13;
                ret;

                // box_and_back:
                %{
                if '__boxed_segment' not in globals():
                    __boxed_segment = segments.add()
                memory[ap + 0] = __boxed_segment
                __boxed_segment += 1
                %}
                [fp + -3] = [[ap + 0] + 0], ap++;
                [ap + 0] = [ap + -1], ap++;
                [ap + 0] = [[ap + -1] + 0], ap++;
                ret;

                // box_and_back_wrapper:
                [ap + 0] = [fp + -3], ap++;
                call rel -5;
                ret;
            "};
            "good_flow")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type UninitializedFelt252 = Uninitialized<felt252>;
                type ArrayFelt252 = Array<felt252>;
                type UninitializedArrayFelt252 = Uninitialized<ArrayFelt252>;

                libfunc finalize_locals = finalize_locals;
                libfunc alloc_local_felt252 = alloc_local<felt252>;
                libfunc store_local_felt252 = store_local<felt252>;
                libfunc alloc_local_array_felt252 = alloc_local<ArrayFelt252>;
                libfunc store_local_array_felt252 = store_local<ArrayFelt252>;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc store_temp_array_felt252 = store_temp<ArrayFelt252>;

                store_temp_felt252([1]) -> ([1]);
                alloc_local_felt252() -> ([4]);
                alloc_local_felt252() -> ([5]);
                alloc_local_array_felt252() -> ([6]);
                store_local_felt252([4], [1]) -> ([4]);
                finalize_locals() -> ();
                store_local_felt252([5], [2]) -> ([5]);
                store_local_array_felt252([6], [3]) -> ([6]);
                store_temp_felt252([4]) -> ([4]);
                store_temp_felt252([5]) -> ([5]);
                store_temp_array_felt252([6]) -> ([6]);
                return ([4], [5], [6]);

                test_program@0([1]: felt252, [2]: felt252, [3]: ArrayFelt252) -> (felt252, felt252, ArrayFelt252);
            "},
            false,
            indoc! {"
                [ap + 0] = [fp + -6], ap++;
                [fp + 1] = [ap + -1];
                ap += 4;
                [fp + 2] = [fp + -5];
                [fp + 3] = [fp + -4];
                [fp + 4] = [fp + -3];
                [ap + 0] = [fp + 1], ap++;
                [ap + 0] = [fp + 2], ap++;
                [ap + 0] = [fp + 3], ap++;
                [ap + 0] = [fp + 4], ap++;
                ret;
            "};
            "alloc_local and store_local")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc branch_align = branch_align;
                libfunc jump = jump;
                libfunc store_temp_nz_felt252 = store_temp<NonZeroFelt252>;
                libfunc nz_felt252_drop = drop<NonZeroFelt252>;
                libfunc felt252_is_zero = felt252_is_zero;

                felt252_is_zero([1]) { fallthrough() 3([1]) };
                branch_align() -> ();
                return ();
                branch_align() -> ();
                store_temp_nz_felt252([1]) -> ([1]);
                nz_felt252_drop([1]) -> ();
                return ();

                test_program@0([1]: felt252) -> ();
            "},
            true,
            indoc! {"
                jmp rel 5 if [fp + -3] != 0;
                ap += 1;
                ret;
                [ap + 0] = [fp + -3], ap++;
                ret;
            "};
            "branch align")]
#[test_case(indoc! {"
                type u128 = u128;
                type RangeCheck = RangeCheck;

                libfunc branch_align = branch_align;
                libfunc revoke_ap_tracking = revoke_ap_tracking;
                libfunc u128_overflowing_add = u128_overflowing_add;
                libfunc drop<u128> = drop<u128>;
                libfunc store_temp<RangeCheck> = store_temp<RangeCheck>;

                revoke_ap_tracking() -> ();
                u128_overflowing_add([1], [2], [3]) {fallthrough([1], [2]) 6([1], [2]) };
                branch_align() -> ();
                drop<u128>([2]) -> ();
                store_temp<RangeCheck>([1]) -> ([1]);
                return ([1]);
                branch_align() -> ();
                drop<u128>([2]) -> ();
                store_temp<RangeCheck>([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: RangeCheck, [2]: u128, [3]: u128) -> (RangeCheck);
            "},
            false,
            indoc! {"
                [ap + 1] = [fp + -4] + [fp + -3], ap++;
                %{ memory[ap + -1] = memory[ap + 0] < 340282366920938463463374607431768211456 %}
                jmp rel 7 if [ap + -1] != 0, ap++;
                [ap + -1] = [ap + 0] + 340282366920938463463374607431768211456, ap++;
                [ap + -1] = [[fp + -5] + 0];
                jmp rel 6;
                [ap + -1] = [[fp + -5] + 0];
                [ap + 0] = [fp + -5] + 1, ap++;
                ret;
                [ap + 0] = [fp + -5] + 1, ap++;
                ret;
            "};
            "u128")]
#[test_case(read_sierra_example_file("fib_no_gas").as_str(),
            false,
            indoc! {"
                jmp rel 4 if [fp + -3] != 0;
                [ap + 0] = [fp + -5], ap++;
                ret;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = [fp + -5] + [fp + -4], ap++;
                [ap + 0] = [fp + -3] + -1, ap++;
                call rel -8;
                ret;
            "};
            "fib_no_gas")]
#[test_case(read_sierra_example_file("fib_jumps").as_str(),
            true,
            indoc! {"
                jmp rel 8 if [fp + -3] != 0;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4] + 1070, ap++;
                [ap + 0] = 1, ap++;
                ret;

                // Statement # 9
                // Setting up the latest memory to be of the form [n, rc, gb, a=1, b=0].
                [ap + 0] = [fp + -3], ap++;
                [ap + 0] = [fp + -5], ap++;
                [ap + 0] = [fp + -4], ap++;
                [ap + 0] = 1, ap++;
                [ap + 0] = 0, ap++;

                // Statement #18, check n.
                jmp rel 7 if [ap + -5] != 0;
                // Statement # 19 - n == 0, so we can return the latest a.
                [ap + 0] = [ap + -4], ap++;
                [ap + 0] = [ap + -4] + 470, ap++;
                [ap + 0] = [ap + -4], ap++;
                ret;

                // Statement # 28 - withdrawing gas for the main loop.
                %{ memory[ap + 0] = 1070 <= memory[ap + -3] %}

                jmp rel 7 if [ap + 0] != 0, ap++;
                [ap + 0] = [ap + -4] + 340282366920938463463374607431768210386, ap++;
                [ap + -1] = [[ap + -6] + 0];
                jmp rel 14;

                // Statement # 30
                // The main loop - given [n, rc, gb, a, b, _, _] - adds [n-1, updated_rc, updated_gb, a+b, a]
                // Memory cells form is now [n'=n-1, rc'=updated_rc, gb'=updated_gb, a'=a+b, b'=a]
                [ap + -4] = [ap + 0] + 1070, ap++;
                [ap + -1] = [[ap + -6] + 0];
                [ap + -7] = [ap + 0] + 1, ap++;
                [ap + 0] = [ap + -7] + 1, ap++;
                [ap + 0] = [ap + -3], ap++;
                [ap + 0] = [ap + -7] + [ap + -6], ap++;
                [ap + 0] = [ap + -8], ap++;
                jmp rel -24;

                // Statement # 40  - Ran out of gas - returning updated gb and -1.
                [ap + 0] = [ap + -6] + 1, ap++;
                [ap + 0] = [ap + -6], ap++;
                [ap + 0] = -1, ap++;
                ret;
            "};
            "fib_jumps")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type Unit = Struct<ut@Tuple>;

                libfunc felt252_add_3 = felt252_add_const<3>;
                libfunc drop<felt252> = drop<felt252>;
                libfunc struct_construct<Unit> = struct_construct<Unit>;
                libfunc store_temp<Unit> = store_temp<Unit>;

                felt252_add_3([0]) -> ([2]);
                drop<felt252>([2]) -> ();
                struct_construct<Unit>() -> ([3]);
                store_temp<Unit>([3]) -> ([4]);
                return([4]);

                test::foo@0([0]: felt252) -> (Unit);
            "},
            false,
            indoc! {"
                ret;
            "};
            "felt252_add_const")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type Unit = Struct<ut@Tuple>;

                libfunc felt252_div_3 = felt252_div_const<3>;
                libfunc drop<felt252> = drop<felt252>;
                libfunc struct_construct<Unit> = struct_construct<Unit>;
                libfunc store_temp<Unit> = store_temp<Unit>;

                felt252_div_3([0]) -> ([2]);
                drop<felt252>([2]) -> ();
                struct_construct<Unit>() -> ([3]);
                store_temp<Unit>([3]) -> ([4]);
                return([4]);

                test::foo@0([0]: felt252) -> (Unit);
            "},
            false,
            indoc! {"
                [fp + -3] = [ap + 0] * 3, ap++;
                ret;
            "};
            "felt252_div_const")]
#[test_case(indoc! {"
            type never = Enum<ut@never>;

            libfunc enum_match<never> = enum_match<never>;

            enum_match<never>([0]) { };
            return();

            main@0([0]: never) -> ();
            main2@1() -> ();
        "},
            false,
            indoc! {"
                ret;
            "};
            "empty_enum")]
#[test_case(indoc! {"
            type felt252 = felt252;
            type NonZeroFelt252 = NonZero<felt252>;
            type Unit = Struct<ut@Tuple>;

            libfunc branch_align = branch_align;
            libfunc jump = jump;
            libfunc felt252_is_zero = felt252_is_zero;
            libfunc drop_nz_felt252 = drop<NonZeroFelt252>;
            libfunc revoke_ap_tracking = revoke_ap_tracking;

            revoke_ap_tracking() -> ();
            felt252_is_zero([1]) { fallthrough() 4([1]) };
            branch_align() -> ();
            jump() { 6() };
            branch_align() -> ();
            drop_nz_felt252([1]) -> ();
            return ([2]);

            test_program@0([1]: felt252, [2]: Unit) -> (Unit);
        "},
        false,
        indoc! {"
            jmp rel 4 if [fp + -3] != 0;
            jmp rel 2;
            ret;
        "};
        "merge unit param")]
#[test_case(indoc! {"
            type felt252 = felt252;
            type NonZeroFelt252 = NonZero<felt252>;
            type Unit = Struct<ut@Tuple>;

            libfunc branch_align = branch_align;
            libfunc jump = jump;
            libfunc felt252_is_zero = felt252_is_zero;
            libfunc drop_nz_felt252 = drop<NonZeroFelt252>;
            libfunc revoke_ap_tracking = revoke_ap_tracking;
            libfunc store_temp_felt252 = store_temp<felt252>;
            libfunc drop_Unit = drop<Unit>;
            libfunc struct_construct<Unit> = struct_construct<Unit>;
            libfunc felt252_const<1> = felt252_const<1>;
            libfunc call_foo = function_call<user@foo>;

            felt252_is_zero([1]) { fallthrough() 5([1]) };
            branch_align() -> ();
            call_foo() -> ([2], [3]);
            drop_Unit([3]) -> ();
            jump() { 9() };
            branch_align() -> ();              // 5
            drop_nz_felt252([1]) -> ();
            call_foo() -> ([2], [3]);
            drop_Unit([3]) -> ();
            return ([2]);                      // 9

            struct_construct<Unit>() -> ([33]); // foo()
            felt252_const<1>() -> ([1]);
            store_temp_felt252([1]) -> ([1]);
            return ([1], [33]);

            test_program@0([1]: felt252) -> (felt252);
            foo@10() -> (felt252, Unit);
        "},
        false,
        indoc! {"
            jmp rel 6 if [fp + -3] != 0;
            call rel 7;
            jmp rel 4;
            call rel 3;
            ret;
            [ap + 0] = 1, ap++;
            ret;
        "};
        "Merge stack with zero_sized variable.")]
#[test_case(indoc! {"
        type felt252 = felt252;
        type const<felt252, 5> = const<felt252, 5>;
        type const<felt252, 17> = const<felt252, 17>;
        type BoxFelt252 = Box<felt252>;
        
        
        libfunc const_as_box<const<felt252, 5>> = const_as_box<const<felt252, 5>>;
        libfunc const_as_box<const<felt252, 17>> = const_as_box<const<felt252, 17>>;
        libfunc unbox<felt252> = unbox<felt252>;
        libfunc store_temp_felt252 = store_temp<felt252>;
        libfunc drop<felt252> = drop<felt252>;
        
        
        const_as_box<const<felt252, 5>>() -> ([1]);
        const_as_box<const<felt252, 17>>() -> ([2]);
        const_as_box<const<felt252, 5>>() -> ([3]);
        unbox<felt252>([1]) -> ([1]);
        unbox<felt252>([2]) -> ([2]);
        unbox<felt252>([3]) -> ([3]);
        store_temp_felt252([1]) -> ([1]);
        drop<felt252>([1]) -> ();
        store_temp_felt252([2]) -> ([2]);
        drop<felt252>([2]) -> ();
        store_temp_felt252([3]) -> ([3]);
        return([3]);
        
        test_program@0() -> (felt252);
        
    "},
    false,
    indoc! {"
        call rel 16;
        [ap + 0] = [ap + -1] + 15, ap++;
        call rel 14;
        [ap + 0] = [ap + -1] + 13, ap++;
        call rel 8;
        [ap + 0] = [ap + -1] + 7, ap++;
        [ap + 0] = [[ap + -7] + 0], ap++;
        [ap + 0] = [[ap + -5] + 0], ap++;
        [ap + 0] = [[ap + -3] + 0], ap++;
        ret;
        ret;
        dw 5;
        ret;
        dw 17;
    "};
    "Simple use of constants.")]
#[test_case(indoc! {"
    type felt252 = felt252;
    type Tuple<felt252, felt252> = Struct<ut@Tuple, felt252, felt252>;
    type const<felt252, 5> = const<felt252, 5>;
    type const<felt252, 17> = const<felt252, 17>;
    type const<
        Tuple<felt252, felt252>, 
        const<felt252, 5>, 
        const<felt252, 17>
    > = const<
        Tuple<felt252, felt252>, 
        const<felt252, 5>, 
        const<felt252, 17>
    >; 
    type const<
        Tuple<felt252, felt252>, 
        const<felt252, 17>, 
        const<felt252, 5>
    > = const<
        Tuple<felt252, felt252>, 
        const<felt252, 17>, 
        const<felt252, 5>
    >; 
    type Box<Tuple<felt252, felt252>> = Box<Tuple<felt252, felt252>>;
    
    
    libfunc const_as_box<
        const<
            Tuple<felt252, felt252>, 
            const<felt252, 5>, 
            const<felt252, 17>
        >
    > = const_as_box<
        const<
            Tuple<felt252, felt252>, 
            const<felt252, 5>, 
            const<felt252, 17>
        >
    >;
    libfunc const_as_box<
        const<
            Tuple<felt252, felt252>, 
            const<felt252, 17>, 
            const<felt252, 5>
        >
    > = const_as_box<
        const<
            Tuple<felt252, felt252>, 
            const<felt252, 17>, 
            const<felt252, 5>
        >
    >;
    libfunc unbox<Tuple<felt252, felt252>> = unbox<Tuple<felt252, felt252>>;
    libfunc store_temp<Tuple<felt252, felt252>> = store_temp<Tuple<felt252, felt252>>;
    libfunc drop<Tuple<felt252, felt252>> = drop<Tuple<felt252, felt252>>;
    
    
    const_as_box<const<Tuple<felt252, felt252>, const<felt252, 5>, const<felt252, 17>>>() -> ([1]);
    const_as_box<const<Tuple<felt252, felt252>, const<felt252, 17>, const<felt252, 5>>>() -> ([2]);
    const_as_box<const<Tuple<felt252, felt252>, const<felt252, 5>, const<felt252, 17>>>() -> ([3]);
    unbox<Tuple<felt252, felt252>>([1]) -> ([1]);
    unbox<Tuple<felt252, felt252>>([2]) -> ([2]);
    unbox<Tuple<felt252, felt252>>([3]) -> ([3]);
    store_temp<Tuple<felt252, felt252>>([1]) -> ([1]);
    store_temp<Tuple<felt252, felt252>>([2]) -> ([2]);
    store_temp<Tuple<felt252, felt252>>([3]) -> ([3]);
    drop<Tuple<felt252, felt252>>([1]) -> ();
    drop<Tuple<felt252, felt252>>([2]) -> ();
    drop<Tuple<felt252, felt252>>([3]) -> ();
    return();

    test_program@0() -> ();
    
"},
false,
indoc! {"
    call rel 19;
    [ap + 0] = [ap + -1] + 18, ap++;
    call rel 18;
    [ap + 0] = [ap + -1] + 17, ap++;
    call rel 11;
    [ap + 0] = [ap + -1] + 10, ap++;
    [ap + 0] = [[ap + -7] + 0], ap++;
    [ap + 0] = [[ap + -8] + 1], ap++;
    [ap + 0] = [[ap + -6] + 0], ap++;
    [ap + 0] = [[ap + -7] + 1], ap++;
    [ap + 0] = [[ap + -5] + 0], ap++;
    [ap + 0] = [[ap + -6] + 1], ap++;
    ret;
    ret;
    dw 5;
    dw 17;
    ret;
    dw 17;
    dw 5;
"};
"Constant structs.")]
#[test_case(indoc! {"
type felt252 = felt252;
type Tuple<felt252, felt252> = Struct<ut@Tuple, felt252, felt252>;
type Tuple<felt252, Tuple<felt252, felt252>> = Struct<ut@Tuple, felt252, Tuple<felt252, felt252>>;
type const<felt252, 5> = const<felt252, 5>;
type const<felt252, 17> = const<felt252, 17>;
type const<
        Tuple<felt252, felt252>, 
        const<felt252, 5>, 
        const<felt252, 17>
    > = const<
        Tuple<felt252, felt252>, 
        const<felt252, 5>, 
        const<felt252, 17>
    >; 
type const<
        Tuple<felt252, Tuple<felt252, felt252>>, 
        const<felt252, 17>, 
        const<Tuple<felt252, felt252>, const<felt252, 5>, const<felt252, 17>>
    > = const<
        Tuple<felt252, Tuple<felt252, felt252>>, 
        const<felt252, 17>, 
        const<Tuple<felt252, felt252>, const<felt252, 5>, const<felt252, 17>>
    >;
type Box<Tuple<felt252, Tuple<felt252, felt252>>> = Box<Tuple<felt252, Tuple<felt252, felt252>>>;

libfunc const_as_box<
        const<
            Tuple<felt252, Tuple<felt252, felt252>>, 
            const<felt252, 17>, 
            const<Tuple<felt252, felt252>, const<felt252, 5>, const<felt252, 17>>
        >
    > = const_as_box<
        const<
            Tuple<felt252, Tuple<felt252, felt252>>, 
            const<felt252, 17>, 
            const<Tuple<felt252, felt252>, const<felt252, 5>, const<felt252, 17>>
        >
    >;
libfunc unbox<
        Tuple<felt252, Tuple<felt252, felt252>>
    > = unbox<
        Tuple<felt252, Tuple<felt252, felt252>>
    >;
libfunc store_temp<
        Tuple<felt252, Tuple<felt252, felt252>>
    > = store_temp<
        Tuple<felt252, Tuple<felt252, felt252>>
    >;
libfunc drop<
        Tuple<felt252, Tuple<felt252, felt252>>
    > = drop<
        Tuple<felt252, Tuple<felt252, felt252>>
    >;

const_as_box<
        const<
            Tuple<felt252, Tuple<felt252, felt252>>, 
            const<felt252, 17>, 
            const<Tuple<felt252, felt252>, const<felt252, 5>, const<felt252, 17>>
        >
    >() -> ([1]);
unbox<Tuple<felt252, Tuple<felt252, felt252>>>([1]) -> ([1]);
store_temp<Tuple<felt252, Tuple<felt252, felt252>>>([1]) -> ([1]);
drop<Tuple<felt252, Tuple<felt252, felt252>>>([1]) -> ();

return();

test_program@0() -> ();
"},
false,
indoc! {"
    call rel 8;
    [ap + 0] = [ap + -1] + 7, ap++;
    [ap + 0] = [[ap + -1] + 0], ap++;
    [ap + 0] = [[ap + -2] + 1], ap++;
    [ap + 0] = [[ap + -3] + 2], ap++;
    ret;
    ret;
    dw 17;
    dw 5;
    dw 17;
"};
"Recursive constant structs.")]
#[test_case(indoc! {"
        type felt252 = felt252;
        type MyEnum = Enum<ut@MyEnum, felt252, felt252>;
        type const<felt252, 5> = const<felt252, 5>;
        type const<MyEnum, 0, const<felt252, 5>> = const<MyEnum, 0, const<felt252, 5>>;
        type Box<MyEnum> = Box<MyEnum>;

        libfunc const_as_box<
            const<MyEnum, 0, const<felt252, 5>>
        > = const_as_box<
            const<MyEnum, 0, const<felt252, 5>>
        >;
        libfunc unbox<MyEnum> = unbox<MyEnum>;
        libfunc store_temp<MyEnum> = store_temp<MyEnum>;
        libfunc drop<MyEnum> = drop<MyEnum>;

        const_as_box<const<MyEnum, 0, const<felt252, 5>>>() -> ([1]);
        unbox<MyEnum>([1]) -> ([1]);
        store_temp<MyEnum>([1]) -> ([1]);
        drop<MyEnum>([1]) -> ();
        return();

        test_program@0() -> ();
    "},
    false,
    indoc! {"
        call rel 7;
        [ap + 0] = [ap + -1] + 6, ap++;
        [ap + 0] = [[ap + -1] + 0], ap++;
        [ap + 0] = [[ap + -2] + 1], ap++;
        ret;
        ret;
        dw 0;
        dw 5;
    "};
    "Constant enums.")]
#[test_case(indoc! {"

        type BuiltinCosts = BuiltinCosts;

        libfunc get_builtin_costs = get_builtin_costs;
        libfunc store_temp<BuiltinCosts> = store_temp<BuiltinCosts>;
        libfunc drop<BuiltinCosts> = drop<BuiltinCosts>;

        get_builtin_costs() -> ([1]);
        store_temp<BuiltinCosts>([1]) -> ([1]);
        drop<BuiltinCosts>([1]) -> ();
        return();

        test_program@0() -> ();
    "},
    false,
    indoc! {"
        call rel 6;
        [ap + 0] = [ap + -1] + 5, ap++;
        [ap + 0] = [[ap + -1] + 0], ap++;
        ret;
    "};
    "Get builtin costs.")]
#[test_case(indoc! {"
        type BuiltinCosts = BuiltinCosts;
        type felt252 = felt252;
        type const<felt252, 5> = const<felt252, 5>;
        type Box<felt252> = Box<felt252>;

        libfunc get_builtin_costs = get_builtin_costs;
        libfunc store_temp<BuiltinCosts> = store_temp<BuiltinCosts>;
        libfunc drop<BuiltinCosts> = drop<BuiltinCosts>;
        libfunc const_as_box<const<felt252, 5>> = const_as_box<const<felt252, 5>>;
        libfunc unbox<felt252> = unbox<felt252>;
        libfunc store_temp<felt252> = store_temp<felt252>;
        libfunc drop<felt252> = drop<felt252>;

        get_builtin_costs() -> ([1]);
        store_temp<BuiltinCosts>([1]) -> ([1]);
        drop<BuiltinCosts>([1]) -> ();
        const_as_box<const<felt252, 5>>() -> ([2]);
        unbox<felt252>([2]) -> ([2]);
        store_temp<felt252>([2]) -> ([2]);
        drop<felt252>([2]) -> ();
        return();

        test_program@0() -> ();
    "},
    false,
    indoc! {"
        call rel 13;
        [ap + 0] = [ap + -1] + 12, ap++;
        [ap + 0] = [[ap + -1] + 0], ap++;
        call rel 6;
        [ap + 0] = [ap + -1] + 5, ap++;
        [ap + 0] = [[ap + -1] + 0], ap++;
        ret;
        ret;
        dw 5;
    "};
    "Get builtin costs with a const segment.")]
fn sierra_to_casm(sierra_code: &str, check_gas_usage: bool, expected_casm: &str) {
    let program = ProgramParser::new().parse(sierra_code).unwrap();
    pretty_assertions::assert_eq!(
        compile(
            &program,
            &if check_gas_usage {
                calc_metadata(&program, Default::default()).unwrap_or_default()
            } else {
                calc_metadata_ap_change_only(&program).unwrap_or_default()
            },
            check_gas_usage
        )
        .expect("Compilation failed.")
        .to_string(),
        strip_comments_and_linebreaks(expected_casm)
    );
}

// TODO(ilya, 10/10/2022): Improve error messages.
#[test_case(indoc! {"
                type felt252 = felt252;

                return([2]);

                test_program@0() -> (felt252);
            "},
            "#0: [2] is undefined.";
            "Missing reference")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_dup = dup<felt252>;

                felt252_dup([1]) -> ([1], [2]);
                felt252_dup([2]) -> ([1], [2]);
                return();

                test_program@0([1]: felt252) -> ();
            "},
            "#1->#2: [1] was overridden.";
            "Reference override")]
#[test_case(indoc! {"
                type felt252 = felt252;

                return([2]);

                test_program@0([2]: felt252) -> (felt252);
            "},
            "#0: Return arguments are not on the stack.";
            "Invalid return reference")]
#[test_case(indoc! {"
                type felt252 = felt252;

                store_temp_felt252([1]) -> ([1]);

                test_program@0([1]: felt252) -> ();
            "},
            "Error from program registry: Could not find the requested libfunc";
            "undeclared libfunc")]
#[test_case(indoc! {"
                type felt252 = felt252;

                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc store_temp_felt252 = store_temp<felt252>;
            "},
            "Error from program registry: Used the same concrete libfunc id twice";
            "Concrete libfunc Id used twice")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_add = felt252_add;

                felt252_add([1], [2]) -> ([4]);
                felt252_add([3], [4]) -> ([5]);
                return([5]);

                test_program@0([1]: felt252, [2]: felt252, [3]: felt252) -> (felt252);
            "},
            "#1: One of the arguments does not satisfy the requirements of the libfunc.";
            "Invalid reference expression for felt252_add")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type u128 = u128;
                libfunc felt252_add = felt252_add;
                felt252_add([1], [2]) -> ([3]);
                return([3]);

                test_program@0([1]: u128, [2]: u128) -> (felt252);
            "},
            "#0: One of the arguments does not match the expected type of the libfunc or return \
 statement.";
            "Types mismatch")]
#[test_case(indoc! {"
                test_program@25() -> ();
            "}, "InvalidStatementIdx";
            "Invalid entry point")]
#[test_case(indoc! {"
                type felt252 = felt252;

                return();

                foo@0([1]: felt252, [1]: felt252) -> ();
            "}, "#0: Invalid function declaration.";
            "Bad Declaration")]
#[test_case(indoc! {"
                return();

                foo@0([0]: BadType) -> ();
            "}, "Error from program registry: Could not find the requested type";
            "Unknown type")]
#[test_case(indoc! {"
            return();
            "}, "MissingAnnotationsForStatement";
            "Missing references for statement")]
#[test_case(indoc! {"
                type NonZeroFelt252 = NonZero<felt252>;
                type felt252 = felt252;
            "}, "Error from program registry: Error during type specialization";
            "type ordering bad for building size map")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_add = felt252_add;
                felt252_add([1], [2], [3]) -> ([4]);
                return();
                test_program@0([1]: felt252, [2]: felt252, [3]: felt252) -> ();
            "}, "Error from program registry: #0: Libfunc invocation input count mismatch";
            "input count mismatch")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_add = felt252_add;
                felt252_add([1], [2]) -> ([3], [4]);
                test_program@0([1]: felt252, [2]: felt252) -> ();
            "}, "Error from program registry: #0: Libfunc invocation branch #0 result count mismatch";
            "output count mismatch")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_add = felt252_add;
                felt252_add([1], [2]) { 0([3]) 1([3]) };
                test_program@0([1]: felt252, [2]: felt252) -> ();
            "}, "Error from program registry: #0: Libfunc invocation branch count mismatch";
            "branch count mismatch")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_add = felt252_add;
                felt252_add([1], [2]) { 0([3]) };
                test_program@0([1]: felt252, [2]: felt252) -> ();
            "}, "Error from program registry: #0: Libfunc invocation branch #0 target mismatch";
            "fallthrough mismatch")]
#[test_case(indoc! {"
                type felt252 = felt252;
                libfunc felt252_dup = dup<felt252>;

                felt252_dup([1]) -> ([1], [2]);
                return ([1]);
                test_program@0([1]: felt252) -> ();
            "}, "[2] is dangling at #1.";
            "Dangling references")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc branch_align = branch_align;
                libfunc felt252_dup = dup<felt252>;
                libfunc jump = jump;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc drop_nz_felt252 = drop<NonZeroFelt252>;

                felt252_dup([1]) -> ([1], [2]);
                felt252_dup([1]) -> ([1], [3]);
                felt252_is_zero([1]) { fallthrough() 7([1]) };
                branch_align() -> ();
                store_temp_felt252([2]) -> ([2]);
                store_temp_felt252([3]) -> ([3]);
                jump() { 11() };
                branch_align() -> ();
                drop_nz_felt252([1]) -> ();
                store_temp_felt252([3]) -> ([3]);
                store_temp_felt252([2]) -> ([2]);
                return ([2], [3]);

                test_program@0([1]: felt252) -> (felt252, felt252);
            "}, "#11: Inconsistent references annotations.";
            "Inconsistent references - different locations on stack")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc branch_align = branch_align;
                libfunc felt252_dup = dup<felt252>;
                libfunc felt252_drop = drop<felt252>;
                libfunc jump = jump;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc drop_nz_felt252 = drop<NonZeroFelt252>;

                felt252_dup([1]) -> ([1], [2]);
                felt252_dup([1]) -> ([1], [3]);
                felt252_is_zero([1]) { fallthrough() 8([1]) };
                branch_align() -> ();
                store_temp_felt252([2]) -> ([2]);
                // Store and drop to break the stack so it can't be tracked.
                store_temp_felt252([3]) -> ([3]);
                felt252_drop([3]) -> ();
                jump() { 13() };
                branch_align() -> ();
                drop_nz_felt252([1]) -> ();
                store_temp_felt252([2]) -> ([2]);
                // Store and drop to break the stack so it can't be tracked.
                store_temp_felt252([3]) -> ([3]);
                felt252_drop([3]) -> ();
                return ([2]); // The failed merge statement #13.

                test_program@0([1]: felt252) -> (felt252);
            "}, "#13: Inconsistent references annotations.";
            "Inconsistent references - unaligned area")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc branch_align = branch_align;
                libfunc disable_ap_tracking = disable_ap_tracking;
                libfunc enable_ap_tracking = enable_ap_tracking;
                libfunc jump = jump;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc drop_nz_felt252 = drop<NonZeroFelt252>;

                disable_ap_tracking() -> ();
                felt252_is_zero([1]) { fallthrough() 5([1]) };
                branch_align() -> ();
                enable_ap_tracking() -> ();
                jump() { 8() };
                branch_align() -> ();
                drop_nz_felt252([1]) -> ();
                enable_ap_tracking() -> ();
                return (); // The failed merge statement #8.

                test_program@0([1]: felt252) -> ();
            "}, "#8: Inconsistent ap tracking.";
            "Inconsistent ap tracking base.")]
#[test_case(indoc! {"
                libfunc enable_ap_tracking = enable_ap_tracking;

                enable_ap_tracking() -> ();
                return ();

                test_program@0() -> ();
            "}, "#0: Attempting to enable ap tracking when already enabled.";
            "Enabling ap tracking when already enabled.")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc branch_align = branch_align;
                libfunc felt252_dup = dup<felt252>;
                libfunc felt252_drop = drop<felt252>;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc store_temp_nz_felt252 = store_temp<NonZeroFelt252>;

                felt252_is_zero([1]) { fallthrough() 4([1]) };
                branch_align() -> ();
                store_temp_felt252([2]) -> ([2]);
                return ([2]);
                branch_align() -> ();
                felt252_drop([2]) -> ();
                store_temp_nz_felt252([1]) -> ([1]);
                return ([1]);

                test_program@0([1]: felt252, [2]: felt252) -> (felt252);
            "}, "#7: One of the arguments does not match the expected type \
of the libfunc or return statement.";
            "Invalid return type")]
#[test_case(indoc! {"
                type felt252 = felt252;

                libfunc felt252_dup = dup<felt252>;
                libfunc felt252_drop = drop<felt252>;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc call_foo = function_call<user@foo>;

                store_temp_felt252([1]) -> ([1]);
                felt252_dup([1]) -> ([1], [2]);
                call_foo([2]) -> ();
                store_temp_felt252([1]) -> ([1]);
                felt252_drop([1]) -> ();
                return();

                foo@0([1]: felt252) -> ();
            "}, "#2->#3: Got 'Unknown ap change' error while moving [1].";
            "Ap change error")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type NonZeroFelt252 = NonZero<felt252>;

                libfunc revoke_ap_tracking = revoke_ap_tracking;
                libfunc branch_align = branch_align;
                libfunc felt252_drop = drop<felt252>;
                libfunc felt252_is_zero = felt252_is_zero;
                libfunc felt252_unwrap_non_zero = unwrap_non_zero<felt252>;
                libfunc jump = jump;

                felt252_is_zero([1]) { fallthrough() 4([1]) };
                branch_align() -> ();
                revoke_ap_tracking() -> ();
                jump() { 7() };
                branch_align() -> ();
                felt252_unwrap_non_zero([1]) -> ([1]);
                felt252_drop([1]) -> ();
                return ();

                foo@0([1]: felt252) -> ();
            "}, "#7: Inconsistent ap tracking.";
            "Inconsistent ap tracking.")]
#[test_case(indoc! {"
                libfunc finalize_locals = finalize_locals;

                finalize_locals () -> ();
                finalize_locals () -> ();
                return ();

                test_program@0() -> ();
            "}, "#1: finalize_locals is not allowed at this point.";
            "Invalid finalize_locals 1")]
#[test_case(indoc! {"
                type felt252 = felt252;

                libfunc finalize_locals = finalize_locals;
                libfunc store_temp_felt252 = store_temp<felt252>;
                libfunc call_foo = function_call<user@foo>;

                store_temp_felt252([1]) -> ([1]);
                call_foo([1]) -> ();
                finalize_locals() -> ();
                return ();

                foo@0([1]: felt252) -> ();
            "}, "#2: finalize_locals is not allowed at this point.";
            "Invalid finalize_locals 2")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type UninitializedFelt252 = Uninitialized<felt252>;

                libfunc alloc_local_felt252 = alloc_local<felt252>;
                libfunc store_temp_felt252 = store_temp<felt252>;

                alloc_local_felt252() -> ([2]);
                store_temp_felt252([1]) -> ([1]);
                alloc_local_felt252() -> ([3]);
                return ();

                foo@0([1]: felt252) -> ();
            "}, "#2: alloc_local is not allowed at this point.";
            "Invalid alloc_local ")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type UninitializedFelt252 = Uninitialized<felt252>;

                libfunc alloc_local_felt252 = alloc_local<felt252>;
                libfunc store_local_felt252 = store_local<felt252>;
                libfunc felt252_drop = drop<felt252>;

                alloc_local_felt252() -> ([2]);
                store_local_felt252([2], [1]) -> ([2]);
                felt252_drop([2]) -> ();
                return ();

                foo@0([1]: felt252) -> ();
            "}, "#3: locals were allocated but finalize_locals was not called.";
            "missing finalize_locals ")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type UninitializedFelt252 = Uninitialized<felt252>;

                libfunc alloc_local_felt252 = alloc_local<felt252>;
                libfunc store_temp_felt252 = store_temp<UninitializedFelt252>;
                libfunc drop_felt252 = drop<UninitializedFelt252>;

                alloc_local_felt252() -> ([1]);
                store_temp_felt252([1]) -> ([1]);
                drop_felt252([1]) -> ();
                return ();

                foo@0() -> ();
            "},
            "Error from program registry: Error during libfunc specialization";
            "store_temp<Uninitialized<felt252>()")]
#[test_case(indoc! {"
                return ();

                foo@0() -> ();
                bar@0() -> ();
            "}, "#0: Belongs to two different functions.";
            "Statement in two functions")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type UninitializedFelt252 = Uninitialized<felt252>;

                libfunc enable_ap_tracking = enable_ap_tracking;
                libfunc disable_ap_tracking = disable_ap_tracking;
                libfunc alloc_local_felt252 = alloc_local<felt252>;

                disable_ap_tracking() -> ();
                enable_ap_tracking() -> ();
                alloc_local_felt252() -> ([1]);

                return ();

                foo@0() -> ();
            "}, "#2: alloc_local is not allowed at this point.";
            "Alloc local after re-enabling ap tracking")]
#[test_case(indoc! {"
                type felt252 = felt252;
                type UninitializedFelt252 = Uninitialized<felt252>;

                return ();

                foo@0([1]: UninitializedFelt252) -> ();
            "}, "Error from program registry: Function parameter type must be storable";
            "Function that uses unstorable types")]
#[test_case(indoc! {"
                type u8 = u8;
                type const<u8, 5, 3> = const<u8, 5, 3>;

                return ();

                foo@0() -> ();
            "}, "Error from program registry: Error during type specialization";
            "Invalid const data.")]
#[test_case(indoc! {"
                type u8 = u8;
                type const<u8, 256> = const<u8, 256>;

                return ();

                foo@0() -> ();
            "}, "Error from program registry: Error during type specialization";
            "Out of range const data.")]
#[test_case(indoc! {"
                type ECPoint = ECPoint;
                type const<ECPoint, 5> = const<ECPoint, 5>;

                return ();

                foo@0() -> ();
            "}, "Error from program registry: Error during type specialization";
            "Non constable const type.")]
#[test_case(indoc! {"
            type felt252 = felt252;
            type const<felt252, 5> = const<felt252, 5>;
            type Tuple<felt252, felt252> = Struct<ut@Tuple, felt252, felt252>;
            type const<
                Tuple<felt252, felt252>, 
                const<felt252, 5>
            > = const<
                Tuple<felt252, felt252>, 
                const<felt252, 5>
            >;
            return ();

            foo@0() -> ();
        "}, "Error from program registry: Error during type specialization";
        "Mismatched number of const struct members.")]
#[test_case(indoc! {"
        type felt252 = felt252;
        type u8 = u8;
        type const<felt252, 5> = const<felt252, 5>;
        type Tuple<u8, u8> = Struct<ut@Tuple, u8, u8>;
        type const<
            Tuple<u8, u8>, 
            const<felt252, 5>, 
            const<felt252, 5>
        > = const<
            Tuple<u8, u8>, 
            const<felt252, 5>, 
            const<felt252, 5>
        >;
        return ();

        foo@0() -> ();
    "}, "Error from program registry: Error during type specialization";
    "Mismatched types of const struct members.")]
fn compiler_errors(sierra_code: &str, expected_result: &str) {
    let program = ProgramParser::new().parse(sierra_code).unwrap();
    pretty_assertions::assert_eq!(
        compile(&program, &calc_metadata_ap_change_only(&program).unwrap_or_default(), false)
            .expect_err("Compilation is expected to fail.")
            .to_string(),
        expected_result
    );
}
