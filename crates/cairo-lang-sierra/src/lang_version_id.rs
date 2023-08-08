use std::fmt;

use serde::{Deserialize, Deserializer, Serialize, Serializer};

const PROGRAM_VERSION: u64 = 1;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Default)]
pub struct LangVersionId;

impl LangVersionId {
    /// Get version as a number.
    pub const fn numeric(self) -> u64 {
        PROGRAM_VERSION
    }

    /// Construct this id if version is correct.
    pub const fn from_numeric(num: u64) -> Option<Self> {
        if num == Self.numeric() { Some(Self) } else { None }
    }
}

impl Serialize for LangVersionId {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_u64(self.numeric())
    }
}

impl From<LangVersionId> for u64 {
    fn from(pin: LangVersionId) -> Self {
        pin.numeric()
    }
}

impl<'de> Deserialize<'de> for LangVersionId {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<LangVersionId, D::Error> {
        use serde::de::Error;
        let num = u64::deserialize(d)?;
        LangVersionId::from_numeric(num).ok_or_else(|| {
            Error::custom(format!("expected Sierra Program version {}", Self.numeric()))
        })
    }
}

impl fmt::Display for LangVersionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Self.numeric())
    }
}
