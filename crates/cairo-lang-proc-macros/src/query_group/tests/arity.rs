#[query_group::query_group]
pub trait ArityDb: salsa::Database {
    fn one(&self, a: ()) -> String;

    fn two(&self, a: (), b: ()) -> String;

    fn three(&self, a: (), b: (), c: ()) -> String;

    fn none(&self) -> String;
}

fn one(_db: &dyn ArityDb, _a: ()) -> String {
    todo!()
}

fn two(_db: &dyn ArityDb, _a: (), _b: ()) -> String {
    todo!()
}

fn three(_db: &dyn ArityDb, _a: (), _b: (), _c: ()) -> String {
    todo!()
}

fn none(_db: &dyn ArityDb) -> String {
    todo!()
}
