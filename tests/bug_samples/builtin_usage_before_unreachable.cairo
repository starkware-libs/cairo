#[derive(Drop)]
enum NeverWithDrop {}

fn foo(x: Option<NeverWithDrop>, a: u32) {
    match x {
        Some(never_ty) => {
            let _b: u16 = a.try_into().unwrap_or(0);
            match never_ty {}
        },
        None => {},
    }
}
