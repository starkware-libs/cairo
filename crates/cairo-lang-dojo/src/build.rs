use std::fs;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;



use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::ids::{FileId};

use cairo_lang_parser::utils::{get_syntax_file_and_diagnostics, SimpleParserDatabase};

use crate::plugin::DojoPlugin;

pub fn build_corelib(path: PathBuf) {
    reset_corelib();
    let db = &mut SimpleParserDatabase::default();
    let file_id = FileId::new(db, path.clone());
    let mut file = File::open(path).unwrap();
    // update file contents by appending node
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let (syntax_file, _diagnostics) = get_syntax_file_and_diagnostics(db, file_id, contents.as_str());
    let plugin = DojoPlugin {};
    for item in syntax_file.items(db).elements(db).into_iter() {
            plugin.generate_corelib(db, item.clone());
    }
}

fn reset_corelib(){
    let corelib_path = detect_corelib().unwrap();
    println!("Corelib path: {}", corelib_path.display());

    fs::copy(corelib_path.join("bases/starknet.cairo"), corelib_path.join("starknet.cairo")).unwrap();
    fs::copy(corelib_path.join("bases/dojo.cairo"), corelib_path.join("dojo.cairo")).unwrap();
    fs::copy(corelib_path.join("bases/serde.cairo"), corelib_path.join("serde.cairo")).unwrap();
    fs::copy(corelib_path.join("bases/lib.cairo"), corelib_path.join("lib.cairo")).unwrap();
}