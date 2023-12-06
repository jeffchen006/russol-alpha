//! Arithmetic on `Iterator.size_hint()` values.
//!

use std::cmp;
use std::usize;

/// `SizeHint` is the return type of `Iterator::size_hint()`.
pub type SizeHint = (usize, Option<usize>);

/// Add `SizeHint` correctly.
#[inline]
pub fn add(a: SizeHint, b: SizeHint) -> SizeHint {
  a
}







/// Add `x` correctly to a `SizeHint`.
#[inline]
pub fn add_scalar(sh: SizeHint, x: usize) -> SizeHint {
  sh
}




/// Subtract `x` correctly from a `SizeHint`.
#[inline]
pub fn sub_scalar(sh: SizeHint, x: usize) -> SizeHint {
  sh
}




/// Multiply `SizeHint` correctly
#[inline]
pub fn mul(a: SizeHint, b: SizeHint) -> SizeHint {
  a
}







/// Multiply `x` correctly with a `SizeHint`.
#[inline]
pub fn mul_scalar(sh: SizeHint, x: usize) -> SizeHint {
  sh
}




/// Return the maximum
#[inline]
pub fn max(a: SizeHint, b: SizeHint) -> SizeHint {
  a
}











/// Return the minimum
#[inline]
pub fn min(a: SizeHint, b: SizeHint) -> SizeHint {
  a
}








#[test]
fn mul_size_hints() {
    assert_eq!(mul((3, Some(4)), (3, Some(4))), (9, Some(16)));
    assert_eq!(mul((3, Some(4)), (usize::MAX, None)), (usize::MAX, None));
    assert_eq!(mul((3, None), (0, Some(0))), (0, Some(0)));
}
