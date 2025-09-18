use std::sync::Arc;

use cairo_lang_test_utils::test;
use cairo_lang_utils::Intern;

use super::FilesGroup;
use crate::cfg::{Cfg, CfgSet};
use crate::db::CrateConfiguration;
use crate::flag::Flag;
use crate::ids::{CrateLongId, Directory, FlagId, FlagLongId, SmolStrId};
use crate::test_utils::FilesDatabaseForTesting;
use crate::{override_file_content, set_crate_config};

#[test]
fn test_filesystem() {
    let mut db = FilesDatabaseForTesting::default();

    let directory = Directory::Real("src".into());
    let child_str = "child.cairo";
    let db_ref = &mut db;
    let file_id = directory.file(db_ref, child_str);
    override_file_content!(db_ref, file_id, Some("content\n".into()));

    let config = CrateConfiguration::default_for_root(directory.clone());
    let crt = CrateLongId::plain(SmolStrId::from(db_ref, "my_crate")).intern(db_ref);
    set_crate_config!(db_ref, crt, Some(config.clone()));

    let crt = CrateLongId::plain(SmolStrId::from(&db, "my_crate")).intern(&db);
    let crt2 = CrateLongId::plain(SmolStrId::from(&db, "my_crate2")).intern(&db);

    assert_eq!(db.crate_config(crt), Some(&config));
    assert!(db.crate_config(crt2).is_none());

    let file_id = directory.file(&db, child_str);
    assert_eq!(db.file_content(file_id).unwrap(), "content\n");
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
        db.cfg_set(),
        &CfgSet::from_iter([Cfg::name("test"), Cfg::kv("k", "v1"), Cfg::kv("k", "v2")])
    )
}
