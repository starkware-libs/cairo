use std::sync::Arc;

use test_log::test;

use super::FilesGroup;
use crate::cfg::{Cfg, CfgSet};
use crate::db::{CrateConfiguration, FilesGroupEx};
use crate::flag::Flag;
use crate::ids::{CrateId, Directory, FlagId};
use crate::test_utils::FilesDatabaseForTesting;

#[test]
fn test_filesystem() {
    let mut db = FilesDatabaseForTesting::default();

    let crt = CrateId::plain(&db, "my_crate");
    let crt2 = CrateId::plain(&db, "my_crate2");
    let directory = Directory::Real("src".into());
    let file_id = directory.file(&db, "child.cairo".into());
    let config = CrateConfiguration::default_for_root(directory);
    db.override_file_content(file_id, Some("content\n".into()));
    db.set_crate_config(crt, Some(config.clone()));

    assert_eq!(db.crate_config(crt), Some(config));
    assert!(db.crate_config(crt2).is_none());

    assert_eq!(db.file_content(file_id).unwrap().as_ref(), "content\n");
}

#[test]
fn test_flags() {
    let mut db = FilesDatabaseForTesting::default();

    let add_withdraw_gas_flag_id = FlagId::new(&db, "add_withdraw_gas");

    db.set_flag(add_withdraw_gas_flag_id, Some(Arc::new(Flag::AddWithdrawGas(false))));

    assert_eq!(*db.get_flag(add_withdraw_gas_flag_id).unwrap(), Flag::AddWithdrawGas(false));
    assert!(db.get_flag(FlagId::new(&db, "non_existing_flag")).is_none());
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
