use std::ops::{Range, RangeBounds};

use serde::Serialize;

#[derive(Debug, Default, Serialize, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    pub start_byte: usize,
    pub end_byte: usize,
}

impl Location {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start_byte: start,
            end_byte: end,
        }
    }

    pub fn to_range(&self) -> Range<usize> {
        self.start_byte..self.end_byte
    }

    pub fn merge(&self, other: Location) -> Location {
        (self.start_byte..other.end_byte).into()
    }

    pub fn contains(&self, byte_offset: usize) -> bool {
        byte_offset >= self.start_byte && byte_offset <= self.end_byte
    }
}

impl From<(usize, usize)> for Location {
    fn from((start_byte, end_byte): (usize, usize)) -> Self {
        Self {
            start_byte,
            end_byte,
        }
    }
}

impl From<Range<usize>> for Location {
    fn from(range: Range<usize>) -> Self {
        let start = match range.start_bound() {
            std::ops::Bound::Included(start) => *start,
            std::ops::Bound::Excluded(start) => *start,
            std::ops::Bound::Unbounded => {
                panic!("can only construct a location from bounded ranges")
            }
        };

        let end = match range.end_bound() {
            std::ops::Bound::Included(end) => *end,
            std::ops::Bound::Excluded(end) => *end,
            std::ops::Bound::Unbounded => {
                panic!("can only construct a location from bounded ranges")
            }
        };

        Location {
            start_byte: start,
            end_byte: end,
        }
    }
}
