use std::sync::Arc;

use cairo_lang_proc_macros::HeapSize;
use cairo_lang_utils::HeapSize;

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

    assert_eq!(test_struct.a.heap_size(), 12);
    assert_eq!(test_struct.b.heap_size(), 5);
    assert_eq!(test_struct.c.heap_size(), 0);
    assert_eq!(
        test_struct.heap_size(),
        test_struct.a.heap_size() + test_struct.b.heap_size() + test_struct.c.heap_size()
    );

    let a = vec![1, 2, 3, 4];
    let a_size = a.heap_size();
    assert_eq!(a_size, 16);
    let b = String::from("test");
    let b_size = b.heap_size();
    assert_eq!(b_size, 4);
    let test_enum1 = TestEnum::Variant1 { a, b };
    assert_eq!(test_enum1.heap_size(), a_size + b_size);

    let arc = Arc::new(vec![1, 2, 3]);
    assert_eq!(arc.heap_size(), 0);
    let test_enum2 = TestEnum::Variant2("hello".to_string(), arc);
    assert_eq!(test_enum2.heap_size(), 5);

    let test_enum3 = TestEnum::Variant3;
    let size3 = test_enum3.heap_size();
    assert_eq!(size3, 0);
}
