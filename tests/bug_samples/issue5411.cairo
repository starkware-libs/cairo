use core::nullable::NullableTrait;

#[test]
fn box_of_box() -> Nullable<Box<u32>> {
    NullableTrait::<Box<u32>>::new(BoxTrait::new(16))
}
