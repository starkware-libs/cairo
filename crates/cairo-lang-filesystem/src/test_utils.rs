use crate::db::{
    CrateConfigStorage, CrateConfigView, FileContentStorage, FileContentView, init_files_group,
    new_crate_config_storage, new_file_content_storage, register_crate_config_view,
    register_files_group_view,
};

// Test salsa database.
#[salsa::db]
#[derive(Clone)]
pub struct FilesDatabaseForTesting {
    storage: salsa::Storage<FilesDatabaseForTesting>,
    file_contents: FileContentStorage,
    crate_configs: CrateConfigStorage,
}

#[salsa::db]
impl salsa::Database for FilesDatabaseForTesting {}
impl FileContentView for FilesDatabaseForTesting {
    fn file_content_storage(&self) -> Option<&FileContentStorage> {
        Some(&self.file_contents)
    }
}
impl CrateConfigView for FilesDatabaseForTesting {
    fn crate_config_storage(&self) -> Option<&CrateConfigStorage> {
        Some(&self.crate_configs)
    }
}

impl Default for FilesDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self {
            storage: Default::default(),
            file_contents: new_file_content_storage(),
            crate_configs: new_crate_config_storage(),
        };
        register_files_group_view(&res);
        register_crate_config_view(&res);
        init_files_group(&mut res);
        res
    }
}
