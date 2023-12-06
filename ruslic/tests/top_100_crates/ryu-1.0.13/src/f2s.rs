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
use crate::f2s_intrinsics::*;

pub const FLOAT_MANTISSA_BITS: u32 = 23;
pub const FLOAT_EXPONENT_BITS: u32 = 8;
const FLOAT_BIAS: i32 = 127;
pub use crate::f2s_intrinsics::{FLOAT_POW5_BITCOUNT, FLOAT_POW5_INV_BITCOUNT};

// A floating decimal representing m * 10^e.
pub struct FloatingDecimal32 {
    pub mantissa: u32,
    // Decimal exponent's range is -45 to 38
    // inclusive, and can fit in i16 if needed.
    pub exponent: i32,
}

#[cfg_attr(feature = "no-panic", inline)]
pub fn f2d(ieee_mantissa: u32, ieee_exponent: u32) -> FloatingDecimal32 {
  crate::f2s::FloatingDecimal32 { mantissa: ieee_exponent as u32, exponent: ieee_exponent as i32 }
}








































































































































