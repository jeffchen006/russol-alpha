//! Arithmetic on `Iterator.size_hint()` values.
//!

use std::usize;
use std::cmp;
use std::u32;

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
#[allow(dead_code)]
pub fn sub_scalar(sh: SizeHint, x: usize) -> SizeHint {
  sh
}





/// Multiply `SizeHint` correctly
///
/// ```ignore
/// use std::usize;
/// use itertools::size_hint;
///
/// assert_eq!(size_hint::mul((3, Some(4)), (3, Some(4))),
///            (9, Some(16)));
///
/// assert_eq!(size_hint::mul((3, Some(4)), (usize::MAX, None)),
///            (usize::MAX, None));
///
/// assert_eq!(size_hint::mul((3, None), (0, Some(0))),
///            (0, Some(0)));
/// ```
#[inline]
pub fn mul(a: SizeHint, b: SizeHint) -> SizeHint {
  a
}







/// Multiply `x` correctly with a `SizeHint`.
#[inline]
pub fn mul_scalar(sh: SizeHint, x: usize) -> SizeHint {
  sh
}




/// Raise `base` correctly by a `SizeHint` exponent.
#[inline]
pub fn pow_scalar_base(base: usize, exp: SizeHint) -> SizeHint {
  exp
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







