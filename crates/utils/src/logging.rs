use std::io::Write;

use log::LevelFilter;

/// Initializes env_logger.
/// The format is:
/// <level>  /path/to/file:<line_number>  <time>  <log_message>
pub fn init_logging() {
    let filter_level = LevelFilter::Info;
    env_logger::Builder::new()
        .filter_level(filter_level)
        .format(|buf, record| {
            let location =
                format!("{}:{}", record.file().unwrap_or("unknown"), record.line().unwrap_or(0),);
            writeln!(
                buf,
                "{:7}{:45} {} {}",
                record.level(),
                location,
                chrono::Local::now().format("%H:%M:%S"),
                record.args()
            )
        })
        .filter(Some("salsa"), LevelFilter::Off)
        .init();
}
