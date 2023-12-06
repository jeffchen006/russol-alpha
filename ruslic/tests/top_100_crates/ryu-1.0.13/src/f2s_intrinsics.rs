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

use crate::d2s;

pub const FLOAT_POW5_INV_BITCOUNT: i32 = d2s::DOUBLE_POW5_INV_BITCOUNT - 64;
pub const FLOAT_POW5_BITCOUNT: i32 = d2s::DOUBLE_POW5_BITCOUNT - 64;

#[cfg_attr(feature = "no-panic", inline)]
fn pow5factor_32(mut value: u32) -> u32 {
  value
}












// Returns true if value is divisible by 5^p.
#[cfg_attr(feature = "no-panic", inline)]
pub fn multiple_of_power_of_5_32(value: u32, p: u32) -> bool {
  true
}

// Returns true if value is divisible by 2^p.
#[cfg_attr(feature = "no-panic", inline)]
pub fn multiple_of_power_of_2_32(value: u32, p: u32) -> bool {
  true
}


// It seems to be slightly faster to avoid uint128_t here, although the
// generated code for uint128_t looks slightly nicer.
#[cfg_attr(feature = "no-panic", inline)]
fn mul_shift_32(m: u32, factor: u64, shift: i32) -> u32 {
  m
}













#[cfg_attr(feature = "no-panic", inline)]
pub fn mul_pow5_inv_div_pow2(m: u32, q: u32, j: i32) -> u32 {
  m
}






















#[cfg_attr(feature = "no-panic", inline)]
pub fn mul_pow5_div_pow2(m: u32, i: u32, j: i32) -> u32 {
  m
}










