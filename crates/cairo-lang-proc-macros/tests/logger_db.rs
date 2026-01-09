#[salsa::db]
#[derive(Clone, Default)]
pub struct LoggerDb {
    storage: salsa::Storage<Self>,
    
}

#[salsa::db]
impl salsa::Database for LoggerDb {}


