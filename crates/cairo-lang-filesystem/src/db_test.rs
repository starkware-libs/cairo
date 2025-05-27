use std::sync::Arc;

use cairo_lang_utils::{Intern, LookupIntern};
use smol_str::SmolStr;
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

    let directory = Directory::Real("src".into());
    let child_str: SmolStr = "child.cairo".into();
    let file_id = directory.file(&db, child_str.clone().intern(&db));
    let file_input = file_id.lookup_intern(&db).into_file_input(&db);
    let overrides = db.update_file_overrides_input(file_input, Some("content\n".into()));
    db.set_file_overrides_input(overrides);

    let config = CrateConfiguration::default_for_root(directory.clone());
    let crt = CrateLongId::plain("my_crate").intern(&db);
    let crate_configs = db.update_crate_configuration_input(crt, Some(config.clone()));
    db.set_crate_configs_input(Arc::new(crate_configs));

    let crt = CrateLongId::plain("my_crate").intern(&db);
    let crt2 = CrateLongId::plain("my_crate2").intern(&db);

    assert_eq!(db.crate_config(crt), Some(config));
    assert!(db.crate_config(crt2).is_none());

    let file_id = directory.file(&db, child_str.intern(&db));
    assert_eq!(db.file_content(file_id).unwrap().long(&db).as_ref(), "content\n");
}

#[test]
fn test_flags() {
    let mut db = FilesDatabaseForTesting::default();

    let add_withdraw_gas_flag_id = FlagLongId("add_withdraw_gas".into());
    db.set_flag(add_withdraw_gas_flag_id.clone(), Some(Arc::new(Flag::AddWithdrawGas(false))));
    let id = add_withdraw_gas_flag_id.clone().intern(&db);

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
