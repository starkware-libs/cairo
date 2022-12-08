use test_log::test;

use super::Expr;

#[test]
fn operations() {
    assert_eq!(
        Expr::<usize>::from_const(1) + Expr::<usize>::from_const(2),
        Expr::<usize>::from_const(3)
    );
    assert_eq!(
        Expr::<usize>::from_const(1) - Expr::<usize>::from_const(2),
        Expr::<usize>::from_const(-1)
    );
    assert_eq!(
        Expr::<usize>::from_var(5) + Expr::<usize>::from_const(2),
        Expr::<usize> { const_term: 2, var_to_coef: [(5, 1)].into_iter().collect() }
    );
    assert_eq!(
        Expr::<usize>::from_var(5) - Expr::<usize>::from_const(2),
        Expr::<usize> { const_term: -2, var_to_coef: [(5, 1)].into_iter().collect() }
    );
    assert_eq!(
        Expr::<usize>::from_var(2) - Expr::<usize>::from_var(2),
        Expr::<usize>::from_const(0)
    );
}
