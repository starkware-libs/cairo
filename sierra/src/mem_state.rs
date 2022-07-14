//use crate::error::Error;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MemLocation {
    Temp(i64),
    Local(i64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    Transient,
    Const(i64),
    Final(MemLocation),
    Add(MemLocation, MemLocation),
    AddConst(MemLocation, i64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemState {
    pub local_cursur: usize,
    pub local_allocated: bool,
    pub temp_used: bool,
    pub temp_cursur: usize,
    pub temp_invalidated: bool,
}

//pub fn merge_consecutive<'a>(
//    locs: impl Iterator<Item = (&'a Location, usize)>,
//) -> Result<(Location, usize), Error> {
//    let mut start_loc: Option<&Location> = None;
//    let mut combined_size = 0;
//    for (loc, size) in locs {
//        if size == 0 {
//            continue;
//        }
//        match &start_loc {
//            None => {
//                start_loc = Some(loc);
//            }
//            Some(Location::Temp(start_offset)) => match loc {
//                Location::Temp(next) if *next == (start_offset + combined_size as i64) => {}
//                _ => {
//                    return Err(Error::LocationsNonCosecutive);
//                }
//            },
//            Some(Location::Local(start_offset)) => match loc {
//                Location::Local(next) if *next == (start_offset + combined_size as i64) => {}
//                _ => {
//                    return Err(Error::LocationsNonCosecutive);
//                }
//            },
//            _ => {
//                return Err(Error::LocationsNonCosecutive);
//            }
//        }
//        combined_size += size;
//    }
//    Ok((
//        match start_loc {
//            None => Location::Transient(vec![]),
//            Some(start_loc) => start_loc.clone(),
//        },
//        combined_size,
//    ))
//}
//
//#[cfg(test)]
//mod tests {
//    use super::*;
//
//    #[test]
//    fn empty() {
//        assert_eq!(
//            merge_consecutive(vec![].into_iter()),
//            Ok((Location::Transient(vec![]), 0))
//        );
//    }
//
//    #[test]
//    fn single_value() {
//        assert_eq!(
//            merge_consecutive(vec![(&Location::Local(1), 5)].into_iter()),
//            Ok((Location::Local(1), 5))
//        );
//        assert_eq!(
//            merge_consecutive(vec![(&Location::Temp(2), 3)].into_iter()),
//            Ok((Location::Temp(2), 3))
//        );
//        assert_eq!(
//            merge_consecutive(vec![(&Location::Transient(vec![]), 0)].into_iter()),
//            Ok((Location::Transient(vec![]), 0))
//        );
//        assert_eq!(
//            merge_consecutive(vec![(&Location::Transient(vec![]), 1)].into_iter()),
//            Ok((Location::Transient(vec![]), 1))
//        );
//    }
//
//    #[test]
//    fn two_values() {
//        assert_eq!(
//            merge_consecutive(vec![(&Location::Local(2), 3), (&Location::Local(5), 1)].into_iter()),
//            Ok((Location::Local(2), 4))
//        );
//        assert_eq!(
//            merge_consecutive(vec![(&Location::Temp(-6), 4), (&Location::Temp(-2), 2)].into_iter()),
//            Ok((Location::Temp(-6), 6))
//        );
//        assert_eq!(
//            merge_consecutive(
//                vec![(&Location::Temp(2), 3), (&Location::Transient(vec![]), 0)].into_iter()
//            ),
//            Ok((Location::Temp(2), 3))
//        );
//        assert_eq!(
//            merge_consecutive(
//                vec![(&Location::Local(2), 3), (&Location::Transient(vec![]), 0)].into_iter()
//            ),
//            Ok((Location::Local(2), 3))
//        );
//        assert_eq!(
//            merge_consecutive(vec![(&Location::Local(2), 4), (&Location::Local(5), 1)].into_iter()),
//            Err(Error::LocationsNonCosecutive)
//        );
//        assert_eq!(
//            merge_consecutive(vec![(&Location::Temp(-6), 3), (&Location::Temp(-2), 2)].into_iter()),
//            Err(Error::LocationsNonCosecutive)
//        );
//        assert_eq!(
//            merge_consecutive(
//                vec![(&Location::Temp(-6), 4), (&Location::Local(-2), 2)].into_iter()
//            ),
//            Err(Error::LocationsNonCosecutive)
//        );
//        assert_eq!(
//            merge_consecutive(
//                vec![(&Location::Temp(2), 3), (&Location::Transient(vec![]), 1)].into_iter()
//            ),
//            Err(Error::LocationsNonCosecutive)
//        );
//    }
//}
