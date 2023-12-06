use core::ops::{Bound, Range, RangeBounds};

pub(crate) fn third<A, B, C>(t: (A, B, C)) -> C {
  t.2
}

pub(crate) fn simplify_range<R>(range: R, len: usize) -> Range<usize>
where
    R: RangeBounds<usize>,
{
  ::core::ops::Range { start: len as usize, end: len }
}



















