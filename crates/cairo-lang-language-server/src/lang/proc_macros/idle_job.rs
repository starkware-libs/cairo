use std::hash::Hash;

use proc_macro_server_api::RpcResponse;
use rustc_hash::FxHashMap;
use serde::de::DeserializeOwned;
use tracing::error;

use super::cache_group::ProcMacroCacheGroup;
use super::client::RequestParams;
use crate::lang::db::AnalysisDatabase;

/// Allows tracking if HashMap changed. Faster than comparing maps at the end.
struct HashMapChangeTracker<K, V> {
    inner: FxHashMap<K, V>,
    changed: bool,
}

impl<K, V> HashMapChangeTracker<K, V> {
    fn new(inner: FxHashMap<K, V>) -> Self {
        Self { inner, changed: false }
    }

    /// Inserts into underlying HashMap and marks it as changed.
    fn insert(&mut self, key: K, value: V)
    where
        K: Eq + Hash,
    {
        self.inner.insert(key, value);
        self.changed = true;
    }

    /// Returns underlying HashMap only if it changed.
    fn inner_if_changed(self) -> Option<FxHashMap<K, V>> {
        self.changed.then_some(self.inner)
    }
}

pub fn apply_proc_macro_server_responses(db: &mut AnalysisDatabase) {
    // TODO we should check here if there are live snapshots, but no idea if it is possible with
    // salsa public api. if there are snapshots running we can skip this job, this will lead to
    // more updates at once later and less cancellation.

    let client = db.proc_macro_client();

    let mut attribute_resolutions = HashMapChangeTracker::new(db.attribute_macro_resolution());
    let mut derive_resolutions = HashMapChangeTracker::new(db.derive_macro_resolution());
    let mut inline_macro_resolutions = HashMapChangeTracker::new(db.inline_macro_resolution());

    let mut requests = client.requests_params.lock().unwrap();

    for response in client.available_responses() {
        match requests.remove(&response.id).unwrap() {
            RequestParams::Attribute(params) => {
                if let Some(result) = parse(response) {
                    attribute_resolutions.insert(params, result);
                }
            }
            RequestParams::Derive(params) => {
                if let Some(result) = parse(response) {
                    derive_resolutions.insert(params, result);
                }
            }
            RequestParams::Inline(params) => {
                if let Some(result) = parse(response) {
                    inline_macro_resolutions.insert(params, result);
                }
            }
        };
    }

    // Set input only if resolution HashMap changed, this way we don't recompute queries if there
    // were no updates.

    if let Some(resolution) = attribute_resolutions.inner_if_changed() {
        db.set_attribute_macro_resolution(resolution);
    }

    if let Some(resolution) = derive_resolutions.inner_if_changed() {
        db.set_derive_macro_resolution(resolution);
    }

    if let Some(resolution) = inline_macro_resolutions.inner_if_changed() {
        db.set_inline_macro_resolution(resolution);
    }
}

fn parse<T: DeserializeOwned>(response: RpcResponse) -> Option<T> {
    serde_json::from_value(response.value)
        .inspect_err(|_| {
            // TODO probably should panic here as it is unexpected and can break whole proc
            // macro machinery
            error!("Failed to deserialize proc-macro-server response into `ProcMacroResult`");
        })
        .ok()
}
