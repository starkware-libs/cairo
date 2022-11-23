use serde::{Deserialize, Deserializer, Serializer};

use crate::program::Program;
use crate::ProgramParser;

// TODO(ilya): Use real serialization.

pub fn serialize_sierra<S>(program: &Program, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(&format!("{}", program))
}

pub fn deserialize_sierra<'a, D>(deserializer: D) -> Result<Program, D::Error>
where
    D: Deserializer<'a>,
{
    let serialized_program = &String::deserialize(deserializer)?;
    ProgramParser::new()
        .parse(serialized_program)
        .map_err(|err| serde::de::Error::custom(format!("Sierra parsing failed.\n{}", err)))
}
