use clap::ValueEnum;
use serde::Serialize;

use crate::profiling::ProfilerConfig;

/// A clap-arg wrapper for Option<[ProfilerConfig]>.
#[derive(ValueEnum, Clone, Default, Debug, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "kebab-case")]
pub enum RunProfilerConfigArg {
    #[default]
    None,
    Cairo,
    Scoped,
    Sierra,
}
impl TryFrom<RunProfilerConfigArg> for ProfilerConfig {
    type Error = ();
    fn try_from(val: RunProfilerConfigArg) -> Result<Self, Self::Error> {
        Ok(match val {
            RunProfilerConfigArg::None => return Err(()),
            RunProfilerConfigArg::Cairo => ProfilerConfig::Cairo,
            RunProfilerConfigArg::Scoped => ProfilerConfig::Scoped,
            RunProfilerConfigArg::Sierra => ProfilerConfig::Sierra,
        })
    }
}
