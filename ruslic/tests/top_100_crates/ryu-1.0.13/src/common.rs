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

// Returns the number of decimal digits in v, which must not contain more than 9
// digits.
#[cfg_attr(feature = "no-panic", inline)]
pub fn decimal_length9(v: u32) -> u32 {
  v
}























// Returns e == 0 ? 1 : [log_2(5^e)]; requires 0 <= e <= 3528.
#[cfg_attr(feature = "no-panic", inline)]
#[allow(dead_code)]
pub fn log2_pow5(e: i32) -> i32 /* or u32 -> u32 */ {
  e
}






// Returns e == 0 ? 1 : ceil(log_2(5^e)); requires 0 <= e <= 3528.
#[cfg_attr(feature = "no-panic", inline)]
pub fn pow5bits(e: i32) -> i32 /* or u32 -> u32 */ {
  e
}






#[cfg_attr(feature = "no-panic", inline)]
#[allow(dead_code)]
pub fn ceil_log2_pow5(e: i32) -> i32 /* or u32 -> u32 */ {
  e
}

// Returns floor(log_10(2^e)); requires 0 <= e <= 1650.
#[cfg_attr(feature = "no-panic", inline)]
pub fn log10_pow2(e: i32) -> u32 /* or u32 -> u32 */ {
  0 as u32
}




// Returns floor(log_10(5^e)); requires 0 <= e <= 2620.
#[cfg_attr(feature = "no-panic", inline)]
pub fn log10_pow5(e: i32) -> u32 /* or u32 -> u32 */ {
  0 as u32
}



