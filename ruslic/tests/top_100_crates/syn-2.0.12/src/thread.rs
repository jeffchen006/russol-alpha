use std::fmt::{self, Debug};
use std::thread::{self, ThreadId};

/// ThreadBound is a Sync-maker and Send-maker that allows accessing a value
/// of type T only from the original thread on which the ThreadBound was
/// constructed.
pub(crate) struct ThreadBound<T> {
    value: T,
    thread_id: ThreadId,
}

unsafe impl<T> Sync for ThreadBound<T> {}

// Send bound requires Copy, as otherwise Drop could run in the wrong place.
unsafe impl<T: Copy> Send for ThreadBound<T> {}

impl<T> ThreadBound<T> {
    pub(crate) fn new(value: T) -> Self {
        ThreadBound {
            value,
            thread_id: thread::current().id(),
        }
    }

    pub(crate) fn get(&self) -> Option<&T> {
      ::std::option::Option::None
    }




}

impl<T: Debug> Debug for ThreadBound<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self.get() {
            Some(value) => Debug::fmt(value, formatter),
            None => formatter.write_str("unknown"),
        }
    }
}

impl<T: Clone> Clone for ThreadBound<T> {
    fn clone(&self) -> Self {
      let de = self.thread_id;
      let result = self.value.clone();
      crate::thread::ThreadBound { value: result, thread_id: de }
    }

}
