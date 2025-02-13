use std::collections::BTreeSet;
use std::fmt;

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use smol_str::SmolStr;

/// Option for the `#[cfg(...)]` language attribute.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Cfg {
    pub key: SmolStr,
    pub value: Option<SmolStr>,
}

impl Cfg {
    /// Creates a `cfg` option that is matchable as `#[cfg(name)]`.
    pub fn name(name: impl Into<SmolStr>) -> Self {
        Self { key: name.into(), value: None }
    }

    /// Creates a `cfg` option that is matchable as `#[cfg(key: "value")]`.
    pub fn kv(key: impl Into<SmolStr>, value: impl Into<SmolStr>) -> Self {
        Self { key: key.into(), value: Some(value.into()) }
    }
}

impl fmt::Display for Cfg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.key)?;

        if let Some(value) = &self.value {
            write!(f, ": {value:?}")?;
        }

        Ok(())
    }
}

impl fmt::Debug for Cfg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Cfg").field(&DebugAsDisplay(&self)).finish()
    }
}

mod serde_ext {
    use serde::{Deserialize, Serialize};
    use smol_str::SmolStr;

    #[derive(Serialize, Deserialize)]
    #[serde(untagged)]
    pub enum Cfg {
        KV(SmolStr, SmolStr),
        Name(SmolStr),
    }
}

impl Serialize for Cfg {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let sd = if let Some(value) = &self.value {
            serde_ext::Cfg::KV(self.key.clone(), value.clone())
        } else {
            serde_ext::Cfg::Name(self.key.clone())
        };
        sd.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Cfg {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let sd = serde_ext::Cfg::deserialize(deserializer)?;
        match sd {
            serde_ext::Cfg::KV(k, v) => Ok(Cfg::kv(k, v)),
            serde_ext::Cfg::Name(name) => Ok(Cfg::name(name)),
        }
    }
}

/// Set of `#[cfg(...)]` options.
///
/// Behaves like a multimap, i.e. it permits storing multiple values for the same key.
/// This allows expressing, for example, the `feature` option that Rust/Cargo does.
#[derive(Clone, Default, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub struct CfgSet(BTreeSet<Cfg>);

impl CfgSet {
    /// Creates an empty `CfgSet`.
    ///
    /// This function does not allocate.
    pub fn new() -> Self {
        Self(BTreeSet::new())
    }

    /// Returns the number of elements in the set.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the set contains no elements.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Adds a value to the set.
    pub fn insert(&mut self, cfg: Cfg) {
        self.0.insert(cfg);
    }

    /// Combines two sets into new one.
    pub fn union(&self, other: &Self) -> Self {
        Self(self.0.union(&other.0).cloned().collect())
    }

    /// An iterator visiting all elements in insertion order.
    pub fn iter(&self) -> impl Iterator<Item = &Cfg> {
        self.0.iter()
    }

    /// Returns `true` if the set contains a value.
    pub fn contains(&self, cfg: &Cfg) -> bool {
        self.0.contains(cfg)
    }

    /// Returns `true` if the set is a subset of another,
    /// i.e., `other` contains at least all the values in `self`.
    pub fn is_subset(&self, other: &Self) -> bool {
        self.0.is_subset(&other.0)
    }

    /// Returns `true` if the set is a superset of another,
    /// i.e., `self` contains at least all the values in `other`.
    pub fn is_superset(&self, other: &Self) -> bool {
        other.is_subset(self)
    }
}

impl IntoIterator for CfgSet {
    type Item = Cfg;
    type IntoIter = <BTreeSet<Cfg> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a CfgSet {
    type Item = &'a Cfg;
    type IntoIter = <&'a BTreeSet<Cfg> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl FromIterator<Cfg> for CfgSet {
    fn from_iter<T: IntoIterator<Item = Cfg>>(iter: T) -> Self {
        Self(FromIterator::from_iter(iter))
    }
}

impl fmt::Debug for CfgSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tuple = f.debug_tuple("CfgSet");
        for cfg in &self.0 {
            tuple.field(&DebugAsDisplay(cfg));
        }
        tuple.finish()
    }
}

struct DebugAsDisplay<T>(T);

impl<T: fmt::Display> fmt::Debug for DebugAsDisplay<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use crate::cfg::{Cfg, CfgSet};

    #[test]
    fn contains() {
        let a = CfgSet::from_iter([Cfg::name("name"), Cfg::kv("k", "a"), Cfg::kv("k", "b")]);
        assert!(a.contains(&Cfg::name("name")));
        assert!(a.contains(&Cfg::kv("k", "a")));
        assert!(a.contains(&Cfg::kv("k", "b")));
        assert!(!a.contains(&Cfg::kv("k", "c")));
    }

    #[test]
    fn is_superset() {
        let a = CfgSet::from_iter([Cfg::name("name"), Cfg::kv("k", "a"), Cfg::kv("k", "b")]);
        let b = CfgSet::from_iter([Cfg::name("name"), Cfg::kv("k", "a")]);
        assert!(a.is_superset(&b));
    }

    #[test]
    fn serde() {
        let cfg = CfgSet::from_iter([
            Cfg::name("name"),
            Cfg::kv("k", "a"),
            Cfg::name("name2"),
            Cfg::kv("k", "b"),
        ]);

        let json = serde_json::to_value(&cfg).unwrap();

        assert_eq!(json, json!([["k", "a"], ["k", "b"], "name", "name2"]));

        let serde_cfg = serde_json::from_value::<CfgSet>(json).unwrap();

        assert_eq!(serde_cfg, cfg);
    }
}
