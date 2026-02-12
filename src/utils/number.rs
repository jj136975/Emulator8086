use std::mem::size_of;
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
                let (a, o1) = (self as $SignedT).overflowing_add(y as $SignedT);
                let (_, o2) = a.overflowing_add(carry as $SignedT);
                (c, o1 || o2, c1 || c2)
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
                let (a, o1) = (self as $SignedT).overflowing_sub(y as $SignedT);
                let (_, o2) = a.overflowing_sub(carry as $SignedT);
                (c, o1 || o2, c1 || c2)
            }

            #[inline]
            fn oc_sub(self, y: Self) -> (Self, bool, bool) {
                let (res, carry) = self.overflowing_sub(y);
                let (_, overflow) = (self as $SignedT).overflowing_sub(y as $SignedT);
                (res, overflow, carry)
            }

            fn rotate_carry_left(self, count: u32, carry: bool) -> (Self, bool) {
                let count = count % (Self::BITS + 1);
                if count == 0 {
                    return (self, carry);
                }
                if (count == 1) {
                    return ((self << 1) | ((carry as $SelfT)), (self as $SignedT) < 0);
                }
                ((self << count) | ((carry as $SelfT) << (count - 1)) | (self >> ((Self::BITS + 1) - count)), self & (1 << (Self::BITS - count)) != 0)
            }
            
            fn rotate_carry_right(self, count: u32, carry: bool) -> (Self, bool) {
                let count = count % (Self::BITS + 1);
                if count == 0 {
                    return (self, carry);
                }
                if (count == 1) {
                    return ((self >> 1) | ((carry as $SelfT) << (Self::BITS - 1)), self & 1 == 1);
                }
                ((self >> count) | ((carry as $SelfT) << (Self::BITS - count)) | (self << ((Self::BITS + 1) - count)), self & (1 << (count - 1)) != 0)
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