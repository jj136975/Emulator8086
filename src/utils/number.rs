use std::ops::{BitAnd, BitOr, BitXor};

#[inline]
pub fn div_rem<T: std::ops::Div<Output = T> + std::ops::Rem<Output = T> + Copy>(
    x: T,
    y: T,
) -> (T, T) {
    let quot = x / y;
    let rem = x % y;
    (quot, rem)
}

#[inline(always)]
pub const fn extend_sign(value: u8) -> u16 {
    value as i8 as i16 as u16
}

/// An operand width of the 8086 (`u8` or `u16`), with the arithmetic the
/// instruction implementations need in order to be written once for both.
/// The `oc_` operations return (result, overflow, carry).
pub trait SpecialOps:
    Copy + PartialEq + BitAnd<Output = Self> + BitOr<Output = Self> + BitXor<Output = Self>
{
    /// Width in bits.
    const BITS: u32;
    const ZERO: Self;
    const ONE: Self;
    const ALL_ONES: Self;

    /// The top bit: the sign of the value read as a signed number.
    fn is_negative(self) -> bool;
    /// The low eight bits.
    fn low_byte(self) -> u8;
    /// Bit `n`, which must be below `BITS`.
    fn bit(self, n: u32) -> bool;

    /// Rotation by `count` bits; any count is allowed.
    fn rotl(self, count: u32) -> Self;
    fn rotr(self, count: u32) -> Self;
    /// Shifts by `count` bits, which must be below `BITS`.
    fn logical_shl(self, count: u32) -> Self;
    fn logical_shr(self, count: u32) -> Self;
    fn arithmetic_shr(self, count: u32) -> Self;

    fn oc_carry_add(self, y: Self, carry: bool) -> (Self, bool, bool);
    fn oc_add(self, y: Self) -> (Self, bool, bool);

    fn oc_carry_sub(self, y: Self, carry: bool) -> (Self, bool, bool);
    fn oc_sub(self, y: Self) -> (Self, bool, bool);

    fn rotate_carry_left(self, count: u32, carry: bool) -> (Self, bool);
    fn rotate_carry_right(self, count: u32, carry: bool) -> (Self, bool);
}

macro_rules! special_uint_impl {
    (
        Self = $SelfT:ty,
        SignedT = $SignedT:ident,
    ) => {
        impl SpecialOps for $SelfT {
            const BITS: u32 = <$SelfT>::BITS;
            const ZERO: Self = 0;
            const ONE: Self = 1;
            const ALL_ONES: Self = <$SelfT>::MAX;

            #[inline]
            fn is_negative(self) -> bool {
                (self as $SignedT) < 0
            }

            #[inline]
            fn low_byte(self) -> u8 {
                self as u8
            }

            #[inline]
            fn bit(self, n: u32) -> bool {
                (self >> n) & 1 != 0
            }

            #[inline]
            fn rotl(self, count: u32) -> Self {
                self.rotate_left(count)
            }

            #[inline]
            fn rotr(self, count: u32) -> Self {
                self.rotate_right(count)
            }

            #[inline]
            fn logical_shl(self, count: u32) -> Self {
                self.wrapping_shl(count)
            }

            #[inline]
            fn logical_shr(self, count: u32) -> Self {
                self.wrapping_shr(count)
            }

            #[inline]
            fn arithmetic_shr(self, count: u32) -> Self {
                (self as $SignedT).wrapping_shr(count) as $SelfT
            }

            #[inline]
            fn oc_carry_add(self, y: Self, carry: bool) -> (Self, bool, bool) {
                let (a, c1) = self.overflowing_add(y);
                let (c, c2) = a.overflowing_add(carry as $SelfT);
                let wide = (self as $SignedT as i32) + (y as $SignedT as i32) + (carry as i32);
                let overflow = wide < ($SignedT::MIN as i32) || wide > ($SignedT::MAX as i32);
                (c, overflow, c1 || c2)
            }

            #[inline]
            fn oc_add(self, y: Self) -> (Self, bool, bool) {
                let (res, carry) = self.overflowing_add(y);
                let (_, overflow) = (self as $SignedT).overflowing_add(y as $SignedT);
                (res, overflow, carry)
            }
            #[inline]
            fn oc_carry_sub(self, y: Self, carry: bool) -> (Self, bool, bool) {
                let (a, c1) = self.overflowing_sub(y);
                let (c, c2) = a.overflowing_sub(carry as $SelfT);
                let wide = (self as $SignedT as i32) - (y as $SignedT as i32) - (carry as i32);
                let overflow = wide < ($SignedT::MIN as i32) || wide > ($SignedT::MAX as i32);
                (c, overflow, c1 || c2)
            }

            #[inline]
            fn oc_sub(self, y: Self) -> (Self, bool, bool) {
                let (res, carry) = self.overflowing_sub(y);
                let (_, overflow) = (self as $SignedT).overflowing_sub(y as $SignedT);
                (res, overflow, carry)
            }

            fn rotate_carry_left(self, count: u32, carry: bool) -> (Self, bool) {
                let bits = Self::BITS + 1; // 9 for u8, 17 for u16
                let count = count % bits;
                if count == 0 {
                    return (self, carry);
                }
                // Use u32 to avoid shift-overflow panics (max 17 bits needed)
                let wide = ((carry as u32) << Self::BITS) | (self as u32);
                let rotated = ((wide << count) | (wide >> (bits - count))) & ((1u32 << bits) - 1);
                ((rotated as $SelfT), (rotated >> Self::BITS) & 1 != 0)
            }

            fn rotate_carry_right(self, count: u32, carry: bool) -> (Self, bool) {
                let bits = Self::BITS + 1;
                let count = count % bits;
                if count == 0 {
                    return (self, carry);
                }
                let wide = ((carry as u32) << Self::BITS) | (self as u32);
                let rotated = ((wide >> count) | (wide << (bits - count))) & ((1u32 << bits) - 1);
                ((rotated as $SelfT), (rotated >> Self::BITS) & 1 != 0)
            }
        }
    };
}

special_uint_impl! {
    Self = u8,
    SignedT = i8,
}

special_uint_impl! {
    Self = u16,
    SignedT = i16,
}
