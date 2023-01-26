pub use cairo_lang_project::*;

#[derive(thiserror::Error, Debug)]
pub enum ProjectError {
    #[error("Only files with .cairo extension can be compiled.")]
    BadFileExtension,
    #[error("Couldn't read {path}: No such file.")]
    NoSuchFile { path: String },
    #[error("Couldn't handle {path}: Not a legal path.")]
    BadPath { path: String },
    #[error("Failed to load project config.")]
    LoadProjectError,
}
