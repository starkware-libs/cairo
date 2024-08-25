
#[test]
fn closure(){
    let x= 8;
    let c = |a| {
        return x*(a+3);
        7_felt252
    };
    assert_eq!(c(2), 40);
}

