use crate::extensions::NoGenericArgsGenericType;
use crate::ids::GenericTypeId;

/// Type for the Dict Manager builtin.
/// This type should be initialized and destructed by the OS.
/// It is assumed to be a pointer to a segment containing the following struct:
/// Dict_infos segment start
/// Number of dicts in the dict_infos segment.
/// Number of destructed dictionaries.
/// On each dict_new/dict_finalize the struct is appended to the segment and the returned pointer is
/// incremented accrodingly.
/// The dict_infos segment contains the following info for each dict:
/// The start of the dict (written on dict_new).
/// The end of the dict (written on dict_finalize).
/// A sequential number of the dict when destructed (written on dict_finalize).
#[derive(Default)]
pub struct DictManagerType {}
impl NoGenericArgsGenericType for DictManagerType {
    const ID: GenericTypeId = GenericTypeId::new_inline("DictManager");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const SIZE: i16 = 1;
}
