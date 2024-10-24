use indoc::indoc;
use pretty_assertions::assert_eq;
use test_log::test;

// Testing by parsing code and printing its display, making sure we get back the formatted code.
#[test]
fn format_test() {
    let parser = cairo_lang_sierra::ProgramParser::new();
    assert_eq!(
        parser
            .parse(indoc! {"
                // Some comment.
                type ConcreteTypeId =  TypeId; // Other comment.
                type ConcreteTypeId  = TypeId<arg>;
                type  ConcreteTypeId = TypeId<arg1, 4>;
                type [123] = TypeId<[12],  4>;
                type [4]= Enum<ut@core::option ::Option:: <core::felt252>, [3],[2]>;
                type Complex = Struct<ut@Complex:: <core::felt252, @core::felt252>,[8],[9]>;
                libfunc CalleeId = LibfuncId ;
                // Additional comment.
                libfunc OtherCalleeId = LibfuncId <arg, 4>;
                libfunc [5642] = LibfuncId<[22 ], 4>;
                libfunc CallFunction = Call<user@Function>;
                libfunc LibDependent = LibDependent<lib@[124]>;
                callee() -> ();
                callee(arg1) -> (res1);
                callee( arg1, arg2) -> ( res1, res2);
                callee() { 5( ) };
                callee(arg1 , arg2) { fallthrough() 7(res1 ) 5(res1, res2) };
                [12345]([12]) { 2([37]) fallthrough() };
                return();
                return ( r);
                return(r1 , r2);
                return ([1], [45], [0]);

                Name@5() -> ();
                Other@3([5]: T1) -> (T2);
                [343]@3([5]: [6343]) -> ([341]);
            "},)
            .map(|p| p.to_string()),
        Ok(indoc! {"
            type ConcreteTypeId = TypeId;
            type ConcreteTypeId = TypeId<arg>;
            type ConcreteTypeId = TypeId<arg1, 4>;
            type [123] = TypeId<[12], 4>;
            type [4] = Enum<ut@core::option::Option::<core::felt252>, [3], [2]>;
            type Complex = Struct<ut@Complex::<core::felt252, @core::felt252>, [8], [9]>;

            libfunc CalleeId = LibfuncId;
            libfunc OtherCalleeId = LibfuncId<arg, 4>;
            libfunc [5642] = LibfuncId<[22], 4>;
            libfunc CallFunction = Call<user@Function>;
            libfunc LibDependent = LibDependent<lib@[124]>;

            callee() -> (); // 0
            callee(arg1) -> (res1); // 1
            callee(arg1, arg2) -> (res1, res2); // 2
            callee() { 5() }; // 3
            callee(arg1, arg2) { fallthrough() 7(res1) 5(res1, res2) }; // 4
            [12345]([12]) { 2([37]) fallthrough() }; // 5
            return(); // 6
            return(r); // 7
            return(r1, r2); // 8
            return([1], [45], [0]); // 9

            Name@5() -> ();
            Other@3([5]: T1) -> (T2);
            [343]@3([5]: [6343]) -> ([341]);
        "}
        .to_string())
    );
}

// Testing by parsing code and comparing the display of VersionedProgram and Program.
#[test]
fn versioned_program_display_test() {
    let parser = cairo_lang_sierra::ProgramParser::new();
    let sierra_code = indoc! {"
                // Some comment.
                type ConcreteTypeId =  TypeId; // Other comment.
                type ConcreteTypeId  = TypeId<arg>;
                type  ConcreteTypeId = TypeId<arg1, 4>;
                type [123] = TypeId<[12],  4>;
                type [4]= Enum<ut@core::option ::Option:: <core::felt252>, [3],[2]>;
                type Complex = Struct<ut@Complex:: <core::felt252, @core::felt252>,[8],[9]>;
                libfunc CalleeId = LibfuncId ;
                // Additional comment.
                libfunc OtherCalleeId = LibfuncId <arg, 4>;
                libfunc [5642] = LibfuncId<[22 ], 4>;
                libfunc CallFunction = Call<user@Function>;
                libfunc LibDependent = LibDependent<lib@[124]>;
                callee() -> ();
                callee(arg1) -> (res1);
                callee( arg1, arg2) -> ( res1, res2);
                callee() { 5( ) };
                callee(arg1 , arg2) { fallthrough() 7(res1 ) 5(res1, res2) };
                [12345]([12]) { 2([37]) fallthrough() };
                return();
                return ( r);
                return(r1 , r2);
                return ([1], [45], [0]);

                Name@5() -> ();
                Other@3([5]: T1) -> (T2);
                [343]@3([5]: [6343]) -> ([341]);
            "};
    assert_eq!(
        parser.parse(sierra_code).map(|p| p.to_string()),
        Ok(parser.parse(sierra_code).unwrap().into_artifact().to_string())
    );
}

// Testing by parsing Sierra code with labels, making sure the labels got resolved correctly.
#[test]
fn labeled_program_display_test() {
    let parser = cairo_lang_sierra::ProgramParser::new();
    assert_eq!(
        parser
            .parse(indoc! {"
                callee() -> (); // 0
                callee(arg1) -> (res1); // 1
                Label2:
                callee(arg1, arg2) -> (res1, res2); // 2
                callee() { Label5() }; // 3
                callee(arg1, arg2) { fallthrough() Label7(res1) Label5(res1, res2) }; // 4
                Label5:
                [12345]([12]) { Label2([37]) fallthrough() }; // 5
                return(); // 6
                Label7:
                return(r); // 7
                return(r1, r2); // 8
                return([1], [45], [0]); // 9
            "},)
            .map(|p| p.to_string()),
        Ok(indoc! {"


            callee() -> (); // 0
            callee(arg1) -> (res1); // 1
            callee(arg1, arg2) -> (res1, res2); // 2
            callee() { 5() }; // 3
            callee(arg1, arg2) { fallthrough() 7(res1) 5(res1, res2) }; // 4
            [12345]([12]) { 2([37]) fallthrough() }; // 5
            return(); // 6
            return(r); // 7
            return(r1, r2); // 8
            return([1], [45], [0]); // 9

        "}
        .to_string())
    );
}
