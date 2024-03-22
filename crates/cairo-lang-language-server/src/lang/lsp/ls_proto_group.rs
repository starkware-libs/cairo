use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_utils::Upcast;
use salsa::InternKey;
use tower_lsp::lsp_types::Url;

pub trait LsProtoGroup: Upcast<dyn FilesGroup> {
    /// Get a [`FileId`] from an [`Url`].
    ///
    /// ## Safety
    /// This function assumes the `url` was produced by this language server. It panics for unknown
    /// url schemes or parse failures.
    fn file_for_url(&self, uri: &Url) -> FileId {
        match uri.scheme() {
            "file" => {
                let path = uri.to_file_path().expect("Invalid file URL.");
                FileId::new(self.upcast(), path)
            }
            "vfs" => {
                let id = uri
                    .host_str()
                    .expect("Invalid VFS URL: missing host string.")
                    .parse::<usize>()
                    .expect("Invalid VFS URL: host string is not a valid integer.");
                FileId::from_intern_id(id.into())
            }
            _ => panic!("Invalid URL: scheme is not supported by this language server."),
        }
    }
}

impl<T> LsProtoGroup for T where T: Upcast<dyn FilesGroup> {}
