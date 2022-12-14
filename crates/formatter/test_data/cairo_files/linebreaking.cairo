
func foo(x: T) -> S {
    // Cascaded dangling break
    let x1 = 1+2+3+4+5+6+7+8+9+1*2*3*4*5*6*7*8*9*1*2*3*4*5*6*7*8*9*1*2*3*4*5*6*7*8*9+1+2+3+4+5+6+7+8+9+1+2+3+4+5+6+7+8+9;
    // Non-dangling break (overridden)
    let x2 = a_very_very_very_very_very_very_very_long_name()+a_very_very_very_very_very_very_very_long_name();
    let x3 = (1+2+3+4+5+6+7+8+9)+(1+2+3+4+5+6+7+8+9)+(1+2+3+4+5+6+7+8+9)+(1+2+3+4+5+6+7+8+9);
    let x4 = (1+2+3+4+5+6+7+8+9+1+2+3+4+5+6+7+8+9)+(1+2+3+4+5+6+7+8+9+1+2+3+4+5+6+7+8+9)+(1+2+3+4+5+6+7+8+9)+(1+2+3+4+5+6+7+8+9);
    let x5 = (1+2+3+4+5+6+7+8+9+1+2+3+4+5+6+7+8+9)+(1+2+3+4+5+6+7+8+9+1+2+3+4+5+6+7+8+9+1+2+3+4+5+6+7+8+9)+(1+2+3+4+5+6+7+8+9)+(1+2+3+4+5+6+7+8+9);
    let x6 = (1+0+(2+0+(3+0+(4+0+(5+0+(6+0+(7+0+(8+0+(9+0+(1+0+(2+0+(3+0+(4+0)))))))))))));
    }

func bar(first_arg: T, second_arg: T, third_arg: T, fourth_arg: T, fifth_arg: T, sixth_arg: T, seventh_arg: T,) -> T {
    let x = Struct{first_arg: first_arg, second_arg: second_arg, third_arg: third_arg, fourth_arg: fourth_arg, fifth_arg: fifth_arg};
}


