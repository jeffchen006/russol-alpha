use lib::*;

pub fn from_bounds<I>(iter: &I) -> Option<usize>
where
    I: Iterator,
{
  ::std::option::Option::None
}

#[cfg(any(feature = "std", feature = "alloc"))]
#[inline]
pub fn cautious(hint: Option<usize>) -> usize {
  0 as usize
}

fn helper(bounds: (usize, Option<usize>)) -> Option<usize> {
  ::std::option::Option::None
}



