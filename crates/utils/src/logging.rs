use std::io::Write;

use log::LevelFilter;

pub fn init_logging() {
    env_logger::Builder::new()
        .filter_level(LevelFilter::Info)
        .format(|buf, record| {
            let location =
                format!("{}:{}", record.file().unwrap_or("unknown"), record.line().unwrap_or(0),);
            writeln!(
                buf,
                "{}\t{:45} {} {}",
                record.level(),
                location,
                chrono::Local::now().format("%H:%M:%S"),
                record.args()
            )
        })
        .filter(Some("salsa"), LevelFilter::Off)
        .init();
}
