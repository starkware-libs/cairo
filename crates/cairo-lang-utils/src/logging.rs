use std::io::Write;

use log::LevelFilter;

/// Initializes env_logger.
/// The format is:
/// `<level>  /path/to/file:<line_number>  <time>  <log_message>`
pub fn init_logging(log_level: LevelFilter) {
    env_logger::Builder::new()
        .filter_level(log_level)
        .format(|buf, record| {
            let location =
                format!("{}:{}", record.file().unwrap_or("unknown"), record.line().unwrap_or(0),);
            let time_format = time::macros::format_description!("[hour]:[minute]:[second]");
            let formatted_time =
                time::OffsetDateTime::now_local().unwrap().format(time_format).unwrap();
            writeln!(buf, "{:7}{:45} {formatted_time} {}", record.level(), location, record.args())
        })
        .filter(Some("salsa"), LevelFilter::Off)
        .init();
}
