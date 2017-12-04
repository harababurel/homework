use num::FromPrimitive;
use std::cmp::PartialEq;
use std::ops::Add;
use std::cmp;
use std::convert::From;
use itertools;

#[derive(Clone, Debug)]
pub struct BigUint {
    data: Vec<u32>,
}

impl BigUint {
    pub fn new(digits: Vec<u32>) -> BigUint {
        BigUint { data: digits }
    }

    pub fn zero() -> BigUint {
        BigUint { data: vec![0] }
    }

    pub fn digits(&self) -> &Vec<u32> {
        &self.data
    }

    pub fn base() -> u64 {
        4294967296
    }
}

impl FromPrimitive for BigUint {
    fn from_i64(n: i64) -> Option<Self> {
        if n < 0 {
            return None;
        }
        Self::from_u64(n as u64)
    }

    fn from_u64(n: u64) -> Option<Self> {
        let mut digits: Vec<u32> = Vec::new();
        let mut n = n;

        while n > 0 {
            digits.push((n % BigUint::base()) as u32);
            n /= BigUint::base();
        }

        Some(BigUint { data: digits })
    }
}

impl<'a, 'b> Add<&'b BigUint> for BigUint {
    type Output = BigUint;

    fn add(self, rhs: &'b BigUint) -> BigUint {
        let mut digits: Vec<u32> = Vec::new();
        let mut carry = 0 as u32;

        for i in 0..cmp::max(self.digits().len(), rhs.digits().len()) {
            let a: u32 = match self.digits().get(i) {
                Some(x) => *x,
                None => 0,
            };

            let b: u32 = match rhs.digits().get(i) {
                Some(x) => *x,
                None => 0,
            };

            let x = (a as u64) + (b as u64) + (carry as u64);

            digits.push((x % BigUint::base()) as u32);
            carry = (x / BigUint::base()) as u32;
        }
        if carry > 0 {
            digits.push(carry as u32);
        }

        BigUint { data: digits }
    }
}

impl<'a, 'b> Add<&'b BigUint> for &'a BigUint {
    type Output = BigUint;

    fn add(self, rhs: &'b BigUint) -> BigUint {
        self.clone() + rhs
    }
}

impl PartialEq for BigUint {
    fn eq(&self, other: &BigUint) -> bool {
        itertools::equal(self.digits().iter().rev().skip_while(|x| **x == 0),
            other.digits().iter().rev().skip_while(|x| **x == 0))
    }
}

impl From<u32> for BigUint {
    fn from(n: u32) -> Self {
        BigUint { data: vec![n] }
    }
}

#[test]
fn test_simple_addition() {
    let x = BigUint::from(1234);
    let y = BigUint::from(5678);
    let z = BigUint::from(6912);

    assert_eq!(&x + &y, z);
    assert_eq!(x + &y, z);
}
