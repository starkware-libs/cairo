use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileLongId};
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

    /// Get the canonical [`Url`] for a [`FileId`].
    fn url_for_file(&self, file_id: FileId) -> Url {
        match self.upcast().lookup_intern_file(file_id) {
            FileLongId::OnDisk(path) => {
                Url::from_file_path(path).expect("Salsa is expected to store absolute paths.")
            }
            FileLongId::Virtual(virtual_file) => {
                let url = format!(
                    "vfs://{}/{}.cairo",
                    file_id.as_intern_id().as_usize(),
                    virtual_file.name
                );
                Url::parse(&url).unwrap()
            }
        }
    }
}

impl<T> LsProtoGroup for T where T: Upcast<dyn FilesGroup> + ?Sized {}
