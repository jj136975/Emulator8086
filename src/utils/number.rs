use num_traits::PrimInt;

#[inline]
pub fn div_rem<T: std::ops::Div<Output=T> + std::ops::Rem<Output=T> + Copy>(x: T, y: T) -> (T, T) {
    let quot = x / y;
    let rem = x % y;
    (quot, rem)
}

#[inline(always)]
pub const fn extend_sign(value: u8) -> u16 {
    value as i8 as i16 as u16
}

pub trait SpecialOps: PrimInt {
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
    }
}

special_uint_impl! {
    Self = u8,
    SignedT = i8,
}

special_uint_impl! {
    Self = u16,
    SignedT = i16,
}