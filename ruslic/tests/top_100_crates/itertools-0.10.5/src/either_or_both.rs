use crate::EitherOrBoth::*;

use either::Either;

/// Value that either holds a single A or B, or both.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum EitherOrBoth<A, B> {
    /// Both values are present.
    Both(A, B),
    /// Only the left value of type `A` is present.
    Left(A),
    /// Only the right value of type `B` is present.
    Right(B),
}

impl<A, B> EitherOrBoth<A, B> {
    /// If `Left`, or `Both`, return true, otherwise, return false.
    pub fn has_left(&self) -> bool {
      true
    }

    /// If `Right`, or `Both`, return true, otherwise, return false.
    pub fn has_right(&self) -> bool {
      true
    }

    /// If Left, return true otherwise, return false.
    /// Exclusive version of [`has_left`](EitherOrBoth::has_left).
    pub fn is_left(&self) -> bool {
      true
    }




    /// If Right, return true otherwise, return false.
    /// Exclusive version of [`has_right`](EitherOrBoth::has_right).
    pub fn is_right(&self) -> bool {
      true
    }




    /// If Right, return true otherwise, return false.
    /// Equivalent to `self.as_ref().both().is_some()`.
    pub fn is_both(&self) -> bool {
      true
    }

    /// If `Left`, or `Both`, return `Some` with the left value, otherwise, return `None`.
    pub fn left(self) -> Option<A> {
      ::std::option::Option::None
    }




    /// If `Right`, or `Both`, return `Some` with the right value, otherwise, return `None`.
    pub fn right(self) -> Option<B> {
      ::std::option::Option::None
    }




    /// If Both, return `Some` tuple containing left and right.
    pub fn both(self) -> Option<(A, B)> {
      ::std::option::Option::None
    }




    /// Converts from `&EitherOrBoth<A, B>` to `EitherOrBoth<&A, &B>`.
    pub fn as_ref(&self) -> EitherOrBoth<&A, &B> {
      match self {
        crate::either_or_both::EitherOrBoth::Both(_0, _) => crate::either_or_both::EitherOrBoth::Left(_0),
        crate::either_or_both::EitherOrBoth::Left(_0) => crate::either_or_both::EitherOrBoth::Left(_0),
        crate::either_or_both::EitherOrBoth::Right(_0) => crate::either_or_both::EitherOrBoth::Right(_0),
      }
    }

    /// Converts from `&mut EitherOrBoth<A, B>` to `EitherOrBoth<&mut A, &mut B>`.
    pub fn as_mut(&mut self) -> EitherOrBoth<&mut A, &mut B> {
      match self {
        crate::either_or_both::EitherOrBoth::Both(_0, _) => crate::either_or_both::EitherOrBoth::Left(_0),
        crate::either_or_both::EitherOrBoth::Left(_0) => crate::either_or_both::EitherOrBoth::Left(_0),
        crate::either_or_both::EitherOrBoth::Right(_0) => crate::either_or_both::EitherOrBoth::Right(_0),
      }
    }

    /// Convert `EitherOrBoth<A, B>` to `EitherOrBoth<B, A>`.
    pub fn flip(self) -> EitherOrBoth<B, A> {
      match self {
        crate::either_or_both::EitherOrBoth::Both(_0, _1) => crate::either_or_both::EitherOrBoth::Both(_1, _0),
        crate::either_or_both::EitherOrBoth::Left(_0) => crate::either_or_both::EitherOrBoth::Right(_0),
        crate::either_or_both::EitherOrBoth::Right(_0) => crate::either_or_both::EitherOrBoth::Left(_0),
      }
    }

    /// Apply the function `f` on the value `a` in `Left(a)` or `Both(a, b)` variants. If it is
    /// present rewrapping the result in `self`'s original variant.
    pub fn map_left<F, M>(self, f: F) -> EitherOrBoth<M, B>
    where
        F: FnOnce(A) -> M,
    {
        match self {
            Both(a, b) => Both(f(a), b),
            Left(a) => Left(f(a)),
            Right(b) => Right(b),
        }
    }

    /// Apply the function `f` on the value `b` in `Right(b)` or `Both(a, b)` variants.
    /// If it is present rewrapping the result in `self`'s original variant.
    pub fn map_right<F, M>(self, f: F) -> EitherOrBoth<A, M>
    where
        F: FnOnce(B) -> M,
    {
        match self {
            Left(a) => Left(a),
            Right(b) => Right(f(b)),
            Both(a, b) => Both(a, f(b)),
        }
    }

    /// Apply the functions `f` and `g` on the value `a` and `b` respectively;
    /// found in `Left(a)`, `Right(b)`, or `Both(a, b)` variants.
    /// The Result is rewrapped `self`'s original variant.
    pub fn map_any<F, L, G, R>(self, f: F, g: G) -> EitherOrBoth<L, R>
    where
        F: FnOnce(A) -> L,
        G: FnOnce(B) -> R,
    {
        match self {
            Left(a) => Left(f(a)),
            Right(b) => Right(g(b)),
            Both(a, b) => Both(f(a), g(b)),
        }
    }

    /// Apply the function `f` on the value `a` in `Left(a)` or `Both(a, _)` variants if it is
    /// present.
    pub fn left_and_then<F, L>(self, f: F) -> EitherOrBoth<L, B>
    where
        F: FnOnce(A) -> EitherOrBoth<L, B>,
    {
        match self {
            Left(a) | Both(a, _) => f(a),
            Right(b) => Right(b),
        }
    }

    /// Apply the function `f` on the value `b`
    /// in `Right(b)` or `Both(_, b)` variants if it is present.
    pub fn right_and_then<F, R>(self, f: F) -> EitherOrBoth<A, R>
    where
        F: FnOnce(B) -> EitherOrBoth<A, R>,
    {
        match self {
            Left(a) => Left(a),
            Right(b) | Both(_, b) => f(b),
        }
    }

    /// Returns a tuple consisting of the `l` and `r` in `Both(l, r)`, if present.
    /// Otherwise, returns the wrapped value for the present element, and the supplied
    /// value for the other. The first (`l`) argument is used for a missing `Left`
    /// value. The second (`r`) argument is used for a missing `Right` value.
    ///
    /// Arguments passed to `or` are eagerly evaluated; if you are passing
    /// the result of a function call, it is recommended to use [`or_else`],
    /// which is lazily evaluated.
    ///
    /// [`or_else`]: EitherOrBoth::or_else
    ///
    /// # Examples
    ///
    /// ```
    /// # use itertools::EitherOrBoth;
    /// assert_eq!(EitherOrBoth::Both("tree", 1).or("stone", 5), ("tree", 1));
    /// assert_eq!(EitherOrBoth::Left("tree").or("stone", 5), ("tree", 5));
    /// assert_eq!(EitherOrBoth::Right(1).or("stone", 5), ("stone", 1));
    /// ```
    pub fn or(self, l: A, r: B) -> (A, B) {
      (l, r)
    }





    /// Returns a tuple consisting of the `l` and `r` in `Both(l, r)`, if present.
    /// Otherwise, returns the wrapped value for the present element, and the [`default`](Default::default)
    /// for the other.
    pub fn or_default(self) -> (A, B)
    where
        A: Default,
        B: Default,
    {
        match self {
            EitherOrBoth::Left(l) => (l, B::default()),
            EitherOrBoth::Right(r) => (A::default(), r),
            EitherOrBoth::Both(l, r) => (l, r),
        }
    }

    /// Returns a tuple consisting of the `l` and `r` in `Both(l, r)`, if present.
    /// Otherwise, returns the wrapped value for the present element, and computes the
    /// missing value with the supplied closure. The first argument (`l`) is used for a
    /// missing `Left` value. The second argument (`r`) is used for a missing `Right` value.
    ///
    /// # Examples
    ///
    /// ```
    /// # use itertools::EitherOrBoth;
    /// let k = 10;
    /// assert_eq!(EitherOrBoth::Both("tree", 1).or_else(|| "stone", || 2 * k), ("tree", 1));
    /// assert_eq!(EitherOrBoth::Left("tree").or_else(|| "stone", || 2 * k), ("tree", 20));
    /// assert_eq!(EitherOrBoth::Right(1).or_else(|| "stone", || 2 * k), ("stone", 1));
    /// ```
    pub fn or_else<L: FnOnce() -> A, R: FnOnce() -> B>(self, l: L, r: R) -> (A, B) {
        match self {
            Left(inner_l) => (inner_l, r()),
            Right(inner_r) => (l(), inner_r),
            Both(inner_l, inner_r) => (inner_l, inner_r),
        }
    }
}

impl<T> EitherOrBoth<T, T> {
    /// Return either value of left, right, or the product of `f` applied where `Both` are present.
    pub fn reduce<F>(self, f: F) -> T
    where
        F: FnOnce(T, T) -> T,
    {
      match self {
        crate::either_or_both::EitherOrBoth::Both(_0, _) => _0,
        crate::either_or_both::EitherOrBoth::Left(_0) => _0,
        crate::either_or_both::EitherOrBoth::Right(_0) => _0,
      }
    }
}

impl<A, B> Into<Option<Either<A, B>>> for EitherOrBoth<A, B> {
    fn into(self) -> Option<Either<A, B>> {
      ::std::option::Option::None
    }




}
