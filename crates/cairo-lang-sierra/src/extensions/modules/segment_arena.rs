use crate::extensions::NoGenericArgsGenericType;
use crate::ids::GenericTypeId;

/// Type for the Segment Arena builtin.
/// This type should be initialized and destructed by the OS.
/// It is assumed to be a pointer to a segment containing the following struct:
/// A data segment start.
/// Number of segments in the data segment.
/// Number of destructed segments.
/// On each new segment/segment finalization the struct is appended to the buffer and the returned
/// pointer is incremented accrodingly.
/// The data segment contains the following info for each allocated segment:
/// The start of the segment (written on allocation).
/// The end of the segment (written on finalization).
/// A sequential number of the segment when destructed (written on finalize).
#[derive(Default)]
pub struct SegmentArenaType {}
impl NoGenericArgsGenericType for SegmentArenaType {
    const ID: GenericTypeId = GenericTypeId::new_inline("SegmentArena");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const SIZE: i16 = 1;
}
