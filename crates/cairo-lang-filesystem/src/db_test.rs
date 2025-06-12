use std::sync::Arc;

use test_log::test;

use super::FilesGroup;
use crate::cfg::{Cfg, CfgSet};
use crate::db::{CrateConfiguration, FilesGroupEx};
use crate::flag::Flag;
use crate::ids::{CrateLongId, Directory, FlagId, FlagLongId};
use crate::test_utils::FilesDatabaseForTesting;

#[test]
fn test_filesystem() {
    let mut db = FilesDatabaseForTesting::default();

    let crt = CrateLongId::plain("my_crate");
    let crt2 = CrateLongId::plain("my_crate2");
    let directory = Directory::Real("src".into());
    let file_long_id = directory.file(&db, "child.cairo".into()).long(&db).clone();
    let config = CrateConfiguration::default_for_root(directory);
    db.override_file_content(file_long_id.clone(), Some("content\n".into()));
    db.set_crate_config(crt.clone(), Some(config.clone()));

    assert_eq!(db.crate_config(db.intern_crate(crt)), Some(config));
    assert!(db.crate_config(db.intern_crate(crt2)).is_none());

    let file_id = db.intern_file(file_long_id);
    assert_eq!(db.file_content(file_id).unwrap().as_ref(), "content\n");
}

#[test]
fn test_flags() {
    let mut db = FilesDatabaseForTesting::default();

    let add_withdraw_gas_flag_id = FlagLongId("add_withdraw_gas".into());

    db.set_flag(add_withdraw_gas_flag_id.clone(), Some(Arc::new(Flag::AddWithdrawGas(false))));
    let id = FlagId::new(&db, add_withdraw_gas_flag_id);

    assert_eq!(*db.get_flag(id).unwrap(), Flag::AddWithdrawGas(false));
    assert!(db.get_flag(FlagId::new(&db, FlagLongId("non_existing_flag".into()))).is_none());
}

#[test]
fn test_cfgs() {
    let mut db = FilesDatabaseForTesting::default();

    db.use_cfg(&CfgSet::from_iter([Cfg::name("test"), Cfg::kv("k", "v1")]));

    db.use_cfg(&CfgSet::from_iter([Cfg::kv("k", "v2")]));

    assert_eq!(
        *db.cfg_set(),
        CfgSet::from_iter([Cfg::name("test"), Cfg::kv("k", "v1"), Cfg::kv("k", "v2")])
    )
}
