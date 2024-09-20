use std::path::PathBuf;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileLongId};
use cairo_lang_utils::Upcast;
use salsa::InternKey;
use tower_lsp::lsp_types::Url;
use tracing::error;

#[cfg(test)]
#[path = "ls_proto_group_test.rs"]
mod test;

pub trait LsProtoGroup: Upcast<dyn FilesGroup> {
    /// Get a [`FileId`] from an [`Url`].
    ///
    /// Returns `None` on failure, and errors are logged.
    fn file_for_url(&self, uri: &Url) -> Option<FileId> {
        match uri.scheme() {
            "file" => uri
                .to_file_path()
                .inspect_err(|()| error!("invalid file url: {uri}"))
                .ok()
                .map(|path| FileId::new(self.upcast(), path)),
            "vfs" =>
            // For vfs://[workspace_folder]/1234.cairo -> 1234
            // For vfs://1234.cairo -> 1234
            {
                uri.host_str()
                    .expect("No host str")
                    .parse::<usize>()
                    .inspect_err(|e| {
                        error!("invalid vfs url, host string is not a valid integer, {e}: {uri}")
                    })
                    .ok()
                    .map(Into::into)
                    .map(FileId::from_intern_id)
            }
            _ => {
                error!("invalid url, scheme is not supported by this language server: {uri}");
                None
            }
        }
    }

    /// Get the canonical [`Url`] for a [`FileId`].
    fn url_for_file(&self, file_id: FileId, workspace_folder: Option<PathBuf>) -> Url {
        let vf = match self.upcast().lookup_intern_file(file_id) {
            FileLongId::OnDisk(path) => return Url::from_file_path(path).unwrap(),
            FileLongId::Virtual(vf) => vf,
            FileLongId::External(id) => self.upcast().ext_as_virtual(id),
        };
        // NOTE: The URL is constructed using setters and path segments in order to
        //   url-encode any funky characters in parts that LS is not controlling.
        let workspace_scope = workspace_folder.map(|workspace_folder| {
            let workspace_name = workspace_folder
                .file_name()
                .expect("No workspace name detected")
                .to_str()
                .expect("Workspace name is not correct utf-8");
            format!("[{}]", workspace_name)
        });
        let mut url = Url::parse("vfs://").unwrap();
        url.set_host(Some(&file_id.as_intern_id().to_string())).unwrap();
        let mut path_segments = url.path_segments_mut().unwrap();
        if let Some(workspace_scope) = workspace_scope {
            path_segments.push(&workspace_scope);
        }
        path_segments.push(&format!("{}.cairo", vf.name));
        drop(path_segments);
        url
    }
}

impl<T> LsProtoGroup for T where T: Upcast<dyn FilesGroup> + ?Sized {}
