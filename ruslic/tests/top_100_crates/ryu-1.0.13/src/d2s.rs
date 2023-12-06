// Translated from C to Rust. The original C code can be found at
// https://github.com/ulfjack/ryu and carries the following license:
//
// Copyright 2018 Ulf Adams
//
// The contents of this file may be used under the terms of the Apache License,
// Version 2.0.
//
//    (See accompanying file LICENSE-Apache or copy at
//     http://www.apache.org/licenses/LICENSE-2.0)
//
// Alternatively, the contents of this file may be used under the terms of
// the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE-Boost or copy at
//     https://www.boost.org/LICENSE_1_0.txt)
//
// Unless required by applicable law or agreed to in writing, this software
// is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.

use crate::common::*;
#[cfg(not(feature = "small"))]
pub use crate::d2s_full_table::*;
use crate::d2s_intrinsics::*;
#[cfg(feature = "small")]
pub use crate::d2s_small_table::*;
use core::mem::MaybeUninit;

pub const DOUBLE_MANTISSA_BITS: u32 = 52;
pub const DOUBLE_EXPONENT_BITS: u32 = 11;
pub const DOUBLE_BIAS: i32 = 1023;
pub const DOUBLE_POW5_INV_BITCOUNT: i32 = 125;
pub const DOUBLE_POW5_BITCOUNT: i32 = 125;

#[cfg_attr(feature = "no-panic", inline)]
pub fn decimal_length17(v: u64) -> u32 {
  0 as u32
}









































// A floating decimal representing m * 10^e.
pub struct FloatingDecimal64 {
    pub mantissa: u64,
    // Decimal exponent's range is -324 to 308
    // inclusive, and can fit in i16 if needed.
    pub exponent: i32,
}

#[cfg_attr(feature = "no-panic", inline)]
pub fn d2d(ieee_mantissa: u64, ieee_exponent: u32) -> FloatingDecimal64 {
  crate::d2s::FloatingDecimal64 { mantissa: ieee_exponent as u64, exponent: ieee_exponent as i32 }
}

















































































































































































































