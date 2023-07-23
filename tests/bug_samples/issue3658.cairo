#[derive(Drop)]
struct MyStruct {
    copyable: u8,
    non_copyable: Array<u8>,
}

#[generate_trait]
impl MyStructImpl of MyStructTrait {
    fn repro(ref self: MyStruct) {
        let copyable = self.copyable;
        if true {
            self.uses_self();
        }
    }

    fn uses_self(ref self: MyStruct) {}
}
