#[test]
fn test_into_iter() {
    let mut iter = array![1, 2, 3].into_iter();
    assert_eq!(Option::Some(1), iter.next());
    assert_eq!(Option::Some(2), iter.next());
    assert_eq!(Option::Some(3), iter.next());
    assert_eq!(Option::None, iter.next());
}

#[derive(Drop, Debug)]
struct MyCollection {
    arr: Array<u32>,
}

#[generate_trait]
impl MyCollectionImpl of MyCollectionTrait {
    fn new() -> MyCollection {
        MyCollection { arr: ArrayTrait::new() }
    }

    fn add(ref self: MyCollection, elem: u32) {
        self.arr.append(elem);
    }
}

impl MyCollectionIntoIterator of IntoIterator<MyCollection> {
    type IntoIter = crate::array::ArrayIter<u32>;
    fn into_iter(self: MyCollection) -> Self::IntoIter {
        self.arr.into_iter()
    }
}

#[test]
fn test_into_iter_impl() {
    let mut c = MyCollectionTrait::new();
    c.add(0);
    c.add(1);
    c.add(2);

    let mut n = 0;
    for i in c {
        assert_eq!(i, n);
        n += 1;
    };
}
