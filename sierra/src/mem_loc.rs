#[derive(Debug, PartialEq)]
pub enum ResLoc {
    ArgRef(usize),
    NewMem,
}
