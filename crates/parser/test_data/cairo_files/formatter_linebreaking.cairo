
func foo(x: T) -> S {
    // Cascaded dangling break
    let x1 = 1+2+3+4+5+6+7+8+9+1*2*3*4*5*6*7*8*9*1*2*3*4*5*6*7*8*9*1*2*3*4*5*6*7*8*9+1+2+3+4+5+6+7+8+9+1+2+3+4+5+6+7+8+9;
    // This test is broken in this PR, will be fixed in the next one.
    // Non-dangling break (overridden)
    // let x2 = a_very_very_very_very_very_very_very_long_name()+a_very_very_very_very_very_very_very_long_name();
    }

func bar(first_arg: T, second_arg: T, third_arg: T, fourth_arg: T, fifth_arg: T, sixth_arg: T, seventh_arg: T) -> T {
    let x = Struct{first_arg: first_arg, second_arg: second_arg, third_arg: third_arg, fourth_arg: fourth_arg, fifth_arg: fifth_arg};
}


