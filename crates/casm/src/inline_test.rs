use indoc::indoc;
use itertools::join;
use pretty_assertions::assert_eq;

use crate::instructions::*;
use crate::operand::*;
use crate::{casm, casm_extend, deref};

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
        call abs 5, ap++;
        call rel y, ap++;
    };

    let code = join(ctx.instructions.iter().map(Instruction::to_string), "\n");
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
            call abs 5, ap++
            call rel [fp + 5], ap++"}
    );
}

#[test]
fn test_extend() {
    let mut ctx = casm! {
        [fp - 0] = 1, ap++;
    };

    casm_extend! {ctx,
        [fp - 1] = 2, ap++;
    };

    let code = join(ctx.instructions.iter().map(Instruction::to_string), "\n");
    assert_eq!(
        code,
        indoc! {"
            [fp + 0] = 1, ap++
            [fp + -1] = 2, ap++"}
    );
}
