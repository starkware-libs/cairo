pub trait IntoOrPanic: Sized + Copy + core::fmt::Debug {
    fn into_or_panic<T>(self) -> T
    where
        T: TryFrom<Self> + core::fmt::Debug,
        <T as TryFrom<Self>>::Error: core::fmt::Debug,
    {
        let as_opt: Result<T, _> = self.try_into();
        as_opt.unwrap_or_else(|_| panic!("Failed to cast from {self:?}."))
    }
}

impl IntoOrPanic for i16 {}
impl IntoOrPanic for u16 {}
impl IntoOrPanic for i32 {}
impl IntoOrPanic for u32 {}
impl IntoOrPanic for i64 {}
impl IntoOrPanic for u64 {}
impl IntoOrPanic for isize {}
impl IntoOrPanic for usize {}
