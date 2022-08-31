use super::ids::GreenId;
use super::kind::SyntaxKind;

/// Gets the vector of children ids that are the indexing key for this SyntaxKind.
/// Each SyntaxKind has some children that are defined in the spec to be its indexing key
/// for its stable pointer. See [super::stable_ptr].
pub fn get_key_fields(_kind: SyntaxKind, _children: Vec<GreenId>) -> Vec<GreenId> {
    // TODO(spapini): Implement this.
    vec![]
}
