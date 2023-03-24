use std::collections::HashSet;
use std::fmt;

use smol_str::SmolStr;

/// Option for the `#[cfg(...)]` language attribute.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Cfg {
    pub key: SmolStr,
    pub value: Option<SmolStr>,
}

impl Cfg {
    pub fn tag(key: impl Into<SmolStr>) -> Self {
        Self { key: key.into(), value: None }
    }

    pub fn kv(key: impl Into<SmolStr>, value: impl Into<SmolStr>) -> Self {
        Self { key: key.into(), value: Some(value.into()) }
    }
}

impl fmt::Display for Cfg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.key)?;

        if let Some(value) = &self.value {
            write!(f, " = {value:?}")?;
        }

        Ok(())
    }
}

impl fmt::Debug for Cfg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Cfg").field(&DebugAsDisplay(&self)).finish()
    }
}

/// Set of `#[cfg(...)]` options.
///
/// Behaves like a multimap, i.e. it permits storing multiple values for the same key.
/// This allows expressing, for example, the `feature` option that Rust/Cargo does.
#[derive(Clone, Default, Eq, PartialEq)]
pub struct CfgSet(HashSet<Cfg>);

impl CfgSet {
    pub fn new() -> Self {
        Self(HashSet::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn insert(&mut self, cfg: Cfg) {
        self.0.insert(cfg);
    }

    pub fn union(&self, other: &Self) -> Self {
        Self(self.0.union(&other.0).cloned().collect())
    }

    pub fn iter(&self) -> impl Iterator<Item = &Cfg> {
        self.0.iter()
    }

    pub fn contains(&self, cfg: &Cfg) -> bool {
        self.0.contains(cfg)
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        self.0.is_subset(&other.0)
    }

    pub fn is_superset(&self, other: &Self) -> bool {
        other.is_subset(self)
    }
}

impl IntoIterator for CfgSet {
    type Item = Cfg;
    type IntoIter = <HashSet<Cfg> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
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
    use crate::cfg::{Cfg, CfgSet};

    #[test]
    fn contains() {
        let a = CfgSet::from_iter([Cfg::tag("tag"), Cfg::kv("k", "a"), Cfg::kv("k", "b")]);
        assert!(a.contains(&Cfg::tag("tag")));
        assert!(a.contains(&Cfg::kv("k", "a")));
        assert!(a.contains(&Cfg::kv("k", "b")));
        assert!(!a.contains(&Cfg::kv("k", "c")));
    }

    #[test]
    fn is_superset() {
        let a = CfgSet::from_iter([Cfg::tag("tag"), Cfg::kv("k", "a"), Cfg::kv("k", "b")]);
        let b = CfgSet::from_iter([Cfg::tag("tag"), Cfg::kv("k", "a")]);
        assert!(a.is_superset(&b));
    }
}
