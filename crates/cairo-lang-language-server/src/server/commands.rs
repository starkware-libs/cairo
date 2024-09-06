use anyhow::bail;

pub enum ServerCommands {
    Reload,
}

impl TryFrom<String> for ServerCommands {
    type Error = anyhow::Error;

    fn try_from(value: String) -> anyhow::Result<Self> {
        match value.as_str() {
            "cairo.reload" => Ok(ServerCommands::Reload),
            _ => bail!("Unrecognized command: {value}"),
        }
    }
}
