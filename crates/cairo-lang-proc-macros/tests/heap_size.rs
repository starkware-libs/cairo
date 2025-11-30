#![cfg(feature = "heapsize")]

use std::sync::Arc;

use cairo_lang_debug::HeapSize;
use cairo_lang_proc_macros::HeapSize;

#[derive(HeapSize)]
struct TestStruct {
    a: Vec<i32>,
    b: String,
    #[allow(dead_code)]
    c: Arc<String>, // Should be skipped
}

#[derive(HeapSize)]
enum TestEnum {
    Variant1 {
        a: Vec<i32>,
        #[allow(dead_code)]
        b: String,
    },
    Variant2(String, Arc<Vec<i32>>),
    Variant3,
}

#[test]
fn test_heap_size_derive() {
    let test_struct =
        TestStruct { a: vec![1, 2, 3], b: "hello".to_string(), c: Arc::new("world".to_string()) };

    // The heap size should include Vec and String but not Arc
    let size = test_struct.heap_size();
    assert!(size > 0);

    let test_enum1 = TestEnum::Variant1 { a: vec![1, 2, 3], b: String::from("test") };
    let size1 = test_enum1.heap_size();
    assert!(size1 > 0);

    let test_enum2 = TestEnum::Variant2("hello".to_string(), Arc::new(vec![1, 2, 3]));
    let size2 = test_enum2.heap_size();
    assert!(size2 > 0);

    let test_enum3 = TestEnum::Variant3;
    let size3 = test_enum3.heap_size();
    assert_eq!(size3, 0);
}
