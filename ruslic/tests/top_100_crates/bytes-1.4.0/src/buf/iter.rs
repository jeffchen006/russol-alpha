use crate::Buf;

/// Iterator over the bytes contained by the buffer.
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// use bytes::Bytes;
///
/// let buf = Bytes::from(&b"abc"[..]);
/// let mut iter = buf.into_iter();
///
/// assert_eq!(iter.next(), Some(b'a'));
/// assert_eq!(iter.next(), Some(b'b'));
/// assert_eq!(iter.next(), Some(b'c'));
/// assert_eq!(iter.next(), None);
/// ```
///
/// [`iter`]: trait.Buf.html#method.iter
/// [`Buf`]: trait.Buf.html
#[derive(Debug)]
pub struct IntoIter<T> {
    inner: T,
}

impl<T> IntoIter<T> {
    /// Creates an iterator over the bytes contained by the buffer.
    ///
    /// # Examples
    ///
    /// ```
    /// use bytes::Bytes;
    ///
    /// let buf = Bytes::from_static(b"abc");
    /// let mut iter = buf.into_iter();
    ///
    /// assert_eq!(iter.next(), Some(b'a'));
    /// assert_eq!(iter.next(), Some(b'b'));
    /// assert_eq!(iter.next(), Some(b'c'));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn new(inner: T) -> IntoIter<T> {
      crate::buf::iter::IntoIter { inner }
    }

    /// Consumes this `IntoIter`, returning the underlying value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use bytes::{Buf, Bytes};
    ///
    /// let buf = Bytes::from(&b"abc"[..]);
    /// let mut iter = buf.into_iter();
    ///
    /// assert_eq!(iter.next(), Some(b'a'));
    ///
    /// let buf = iter.into_inner();
    /// assert_eq!(2, buf.remaining());
    /// ```
    pub fn into_inner(self) -> T {
      self.inner
    }

    /// Gets a reference to the underlying `Buf`.
    ///
    /// It is inadvisable to directly read from the underlying `Buf`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use bytes::{Buf, Bytes};
    ///
    /// let buf = Bytes::from(&b"abc"[..]);
    /// let mut iter = buf.into_iter();
    ///
    /// assert_eq!(iter.next(), Some(b'a'));
    ///
    /// assert_eq!(2, iter.get_ref().remaining());
    /// ```
    pub fn get_ref(&self) -> &T {
      &self.inner
    }

    /// Gets a mutable reference to the underlying `Buf`.
    ///
    /// It is inadvisable to directly read from the underlying `Buf`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use bytes::{Buf, BytesMut};
    ///
    /// let buf = BytesMut::from(&b"abc"[..]);
    /// let mut iter = buf.into_iter();
    ///
    /// assert_eq!(iter.next(), Some(b'a'));
    ///
    /// iter.get_mut().advance(1);
    ///
    /// assert_eq!(iter.next(), Some(b'c'));
    /// ```
    pub fn get_mut(&mut self) -> &mut T {
      &mut self.inner
    }
}

impl<T: Buf> Iterator for IntoIter<T> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
      ::core::option::Option::None
    }








    fn size_hint(&self) -> (usize, Option<usize>) {
      (0 as usize, ::core::option::Option::None)
    }

}

impl<T: Buf> ExactSizeIterator for IntoIter<T> {}
