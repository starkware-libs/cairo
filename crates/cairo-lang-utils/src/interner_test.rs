use super::{InternedElement, UnsizedInterner};

impl InternedElement for [u8] {
    fn from_u8s(buf: &[u8]) -> &Self {
        unsafe { std::mem::transmute(buf) }
    }
}

#[test]
fn test_interner() {
    let interner = UnsizedInterner::new(65536);
    let elements = vec![
        vec![0_u8],
        vec![1_u8, 2_u8],
        vec![2_u8, 1_u8],
        vec![2_u8],
        vec![1_u8, 2_u8, 3_u8],
        vec![],
        vec![1_u8, 2_u8, 3_u8, 4_u8],
        vec![2_u8, 3_u8, 4_u8],
    ];
    for el in elements.iter() {
        let id = interner.intern(&el[..]);
        assert_eq!(interner.lookup(id), el);
        assert_eq!(interner.intern(&el[..]), id);
    }
}
