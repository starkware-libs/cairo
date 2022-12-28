use pretty_assertions::assert_eq;
use test_log::test;
use utils::ordered_hash_map::OrderedHashMap;
use utils::unordered_hash_set::UnorderedHashSet;

use super::KnownStack;

/// Creates a [KnownStack] object that contains the given vars at the top (the last variable is the
/// topmost one).
fn dummy_stack(vars: &[usize]) -> KnownStack {
    let variables_on_stack: OrderedHashMap<sierra::ids::VarId, usize> =
        vars.iter().enumerate().map(|(i, x)| (sierra::ids::VarId::from_usize(*x), i)).collect();
    KnownStack { variables_on_stack, offset: vars.len() }
}

fn assert_eq_stacks(a: &KnownStack, b: &KnownStack) {
    // Make sure both stacks have the same set of variables.
    assert_eq!(
        a.variables_on_stack.keys().collect::<UnorderedHashSet<_>>(),
        b.variables_on_stack.keys().collect::<UnorderedHashSet<_>>()
    );
    for (var, a_index) in a.variables_on_stack.iter() {
        assert_eq!(
            a.offset - *a_index,
            b.offset - b.variables_on_stack[var.clone()],
            "Wrong value found for {var}.\na: {a:?}\nb: {b:?}"
        );
    }
}

#[test]
fn merge_stacks() {
    // Same stack.
    let a = dummy_stack(&[0, 1, 2, 3]);
    let b = dummy_stack(&[0, 1, 2, 3]);
    let res = dummy_stack(&[0, 1, 2, 3]);
    assert_eq_stacks(&a.merge_with(&b), &res);
    assert_eq_stacks(&b.merge_with(&a), &res);

    // Prefix stack.
    let a = dummy_stack(&[0, 1, 2, 3]);
    let b = dummy_stack(&[2, 3]);
    let res = dummy_stack(&[2, 3]);
    assert_eq_stacks(&a.merge_with(&b), &res);
    assert_eq_stacks(&b.merge_with(&a), &res);

    // Inconsistent stacks.
    let a = dummy_stack(&[0, 1, 2]);
    let b = dummy_stack(&[0, 1, 3]);
    let res = dummy_stack(&[]);
    assert_eq_stacks(&a.merge_with(&b), &res);
    assert_eq_stacks(&b.merge_with(&a), &res);

    let a = dummy_stack(&[0, 1, 2, 3]);
    let b = dummy_stack(&[0, 1, 2]);
    let res = dummy_stack(&[]);
    assert_eq_stacks(&a.merge_with(&b), &res);
    assert_eq_stacks(&b.merge_with(&a), &res);

    // Generic.
    let a = dummy_stack(&[0, 1, 2, 3]);
    let b = dummy_stack(&[4, 2, 3]);
    let res = dummy_stack(&[2, 3]);
    assert_eq_stacks(&a.merge_with(&b), &res);
    assert_eq_stacks(&b.merge_with(&a), &res);

    // "Holes" (e.g., the first item is consistent, but the second is not).
    let a = dummy_stack(&[0, 1, 2, 3]);
    let b = dummy_stack(&[0, 4, 2, 3]);
    let res = dummy_stack(&[2, 3]);
    assert_eq_stacks(&a.merge_with(&b), &res);
    assert_eq_stacks(&b.merge_with(&a), &res);

    let a = dummy_stack(&[0, 1, 2, 3]);
    let b = dummy_stack(&[0, 1, 4, 3]);
    let res = dummy_stack(&[3]);
    assert_eq_stacks(&a.merge_with(&b), &res);
    assert_eq_stacks(&b.merge_with(&a), &res);
}
