pub trait IteratorEx<T, impl I: Iterator<T>, +Destruct<T>, +Drop<I::Item>> {
    fn advance_by_(
        ref self: T, n: usize,
    ) -> Result<
        (), NonZero<usize>,
    > {
        let mut res = Ok(());
        for i in 0..n {
            if self.next().is_none() {
                res = Err((n - i).try_into().unwrap());
                break;
            }
        }
        res
    }
}
impl IteratorExImpl<T, impl I: Iterator<T>, +Destruct<T>, +Drop<I::Item>> of IteratorEx<T, I> {}

#[test]
fn test_advance_by() {
    let mut iter = array![1_u8, 2, 3, 4].into_iter();
    assert_eq!(iter.advance_by_(2), Ok(()));
    assert_eq!(iter.next(), Some(3));
    assert_eq!(iter.advance_by_(0), Ok(()));
}
