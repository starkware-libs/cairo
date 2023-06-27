#[contract]
mod MinimalContract {
    #[external]
    fn test_big_const() {
        felt_const::<361850278866613121369732278309507010562310721533159669997309205613587202048>();
    }
}
