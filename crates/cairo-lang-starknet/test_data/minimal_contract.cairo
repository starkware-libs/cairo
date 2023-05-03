#[contract]
mod MinimalContract {
    struct Storage {}
    #[external]
    fn empty(ref self: Storage) {}
}
