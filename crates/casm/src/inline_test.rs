use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::instructions::*;
use crate::operand::*;
use crate::{casm, deref};

#[test]
fn test_assert() {
    let x = 1;
    let y = deref!([fp + 5]);
    let z = 5;

    let ctx = casm! {
        [fp - 5] = x, ap++;
        [fp - 5] = [ap + 1] + [fp - 5], ap++;
        [fp + 5] = [ap + 1] + 2;
        [ap] = [ap + 1] * [fp - 5];
        [fp - 5] = [ap + 1] * z;
        [fp - 5] = [ap + 1] * y;
        [fp - 5] = 1, ap++;
        [fp - 5] = [ap + 1];
        jnz 123 if [ap + 17];
        jnz rel [fp - 19] if [ap + 17];
        call abs 5, ap++;
        call rel y, ap++;
        ret;
    };

    let code = ctx.instructions.iter().map(Instruction::to_string).collect::<Vec<_>>().join("\n");
    assert_eq!(
        code,
        indoc! {"
            [fp + -5] = 1, ap++
            [fp + -5] = [ap + 1] + [fp + -5], ap++
            [fp + 5] = [ap + 1] + 2
            [ap + 0] = [ap + 1] * [fp + -5]
            [fp + -5] = [ap + 1] * 5
            [fp + -5] = [ap + 1] * [fp + 5]
            [fp + -5] = 1, ap++
            [fp + -5] = [ap + 1]
            jmp rel 123 if [ap + 17] != 0
            jmp rel [fp + -19] if [ap + 17] != 0
            call abs 5, ap++
            call rel [fp + 5], ap++
            ret"}
    );
}
