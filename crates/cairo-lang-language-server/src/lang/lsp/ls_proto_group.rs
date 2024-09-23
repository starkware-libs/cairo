use std::str::FromStr;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileLongId};
use cairo_lang_utils::Upcast;
use lsp_types::Uri;
use salsa::InternKey;
use tracing::error;
use url::Url;

#[cfg(test)]
#[path = "ls_proto_group_test.rs"]
mod test;

pub trait LsProtoGroup: Upcast<dyn FilesGroup> {
    /// Get a [`FileId`] from an [`Uri`].
    ///
    /// Returns `None` on failure, and errors are logged.
    fn file_for_uri(&self, uri: &Uri) -> Option<FileId> {
        match uri.scheme().map(|s| s.as_str()) {
            None | Some("file") => {
                uri.path().as_str().parse().ok().map(|path| FileId::new(self.upcast(), path))
            }
            Some("vfs") => uri
                .authority()
                .or_else(|| {
                    error!("invalid vfs uri, missing host string: {uri:?}");
                    None
                })?
                .host()
                .as_str()
                .parse::<usize>()
                .inspect_err(|e| {
                    error!("invalid vfs uri, host string is not a valid integer, {e}: {uri:?}")
                })
                .ok()
                .map(Into::into)
                .map(FileId::from_intern_id),
            _ => {
                error!("invalid uri, scheme is not supported by this language server: {uri:?}");
                None
            }
        }
    }

    /// Get the canonical [`Uri`] for a [`FileId`].
    fn uri_for_file(&self, file_id: FileId) -> Uri {
        let vf = match self.upcast().lookup_intern_file(file_id) {
            FileLongId::OnDisk(path) => {
                return Url::from_file_path(path.to_str().unwrap())
                    .unwrap()
                    .as_str()
                    .parse()
                    .unwrap();
            }
            FileLongId::Virtual(vf) => vf,
            FileLongId::External(id) => self.upcast().ext_as_virtual(id),
        };
        // NOTE: The URL is constructed using setters and path segments in order to
        //   url-encode any funky characters in parts that LS is not controlling.
        let mut url = Url::parse("vfs://").unwrap();
        url.set_host(Some(&file_id.as_intern_id().to_string())).unwrap();
        url.path_segments_mut().unwrap().push(&format!("{}.cairo", vf.name));

        Uri::from_str(url.as_ref()).unwrap()
    }
}

impl<T> LsProtoGroup for T where T: Upcast<dyn FilesGroup> + ?Sized {}
