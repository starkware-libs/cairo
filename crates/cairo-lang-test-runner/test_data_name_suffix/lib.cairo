mod a {
    mod foo {
        mod b {
            #[test]
            fn check() {}
        }
    }
}

mod b {
    #[test]
    fn check() {
        assert!(1 == 2);
    }
}
