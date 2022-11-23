use serde;

use crate::program::Program;
use crate::ProgramParser;

// TODO(ilya): Use real serialization.

impl serde::Serialize for Program {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("{}", *self))
    }
}

impl<'de> serde::Deserialize<'de> for Program {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let serialized_program = &String::deserialize(deserializer)?;
        ProgramParser::new()
            .parse(serialized_program)
            .map_err(|err| serde::de::Error::custom(format!("Sierra parsing failed.\n{}", err)))
    }
}
