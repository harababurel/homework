extern crate crossbeam;

use num::bigint::BigUint;
use num::traits::{Zero, ToPrimitive};
use num::integer::Integer;
use bounded_spsc_queue::Producer;
use bounded_spsc_queue;

pub struct Adder {
    xs: Vec<BigUint>,
    capacity: usize,
}

impl Adder {
    pub fn new() -> Adder {
        Adder { xs: vec![], capacity: 1e3 as usize }
    }

    pub fn with_capacity(capacity: usize) -> Adder {
        Adder { xs: vec![], capacity }
    }

    pub fn push(&mut self, x: BigUint) {
        self.xs.push(x);
    }

    pub fn compute(&self) -> BigUint {
        self.xs.iter().fold(Zero::zero(), |acc, x| acc + x)
    }

    pub fn compute_rec(&self) -> BigUint {
        self.compute_slice(self.xs.as_slice())
    }

    fn compute_slice(&self, _xs: &[BigUint]) -> BigUint {
        if _xs.len() == 1 {
            return _xs[0].clone();
        }

        let mid = _xs.len() / 2;
        let left = self.compute_slice(&_xs[0..mid]);
        let right = self.compute_slice(&_xs[mid..]);

        return left + right;
    }

    pub fn compute_par(&self) -> BigUint {
        let (producer, consumer) = bounded_spsc_queue::make(self.capacity);
        self.compute_slice_par(self.xs.as_slice(), producer);

        let digits: Vec<u32> = (0..)
            .map(|_| consumer.pop())
            .take_while(|&x| x.is_some())
            .map(|x| x.unwrap())
            .collect();

        BigUint::new(digits)
    }

    fn compute_slice_par(&self, _xs: &[BigUint], producer: Producer<Option<u32>>) {
        if _xs.len() == 1 {
            let mut x = _xs[0].clone();
            let mut digit: BigUint;

            let base = BigUint::new(vec![0, 1]);

            while !x.is_zero() {
                let (quotient, remainder) = x.div_rem(&base);
                x = quotient;
                digit = remainder;
                producer.push(Some(digit.to_u32().unwrap()));
            }
            producer.push(None);
            return;
        }

        crossbeam::scope(|scope| {
            let mid = _xs.len() / 2;

            let (producer_left, consumer_left) = bounded_spsc_queue::make(self.capacity);
            scope.spawn(move || { self.compute_slice_par(&_xs[0..mid], producer_left); });

            let (producer_right, consumer_right) = bounded_spsc_queue::make(self.capacity);
            scope.spawn(move || { self.compute_slice_par(&_xs[mid..], producer_right); });

            scope.spawn(move || {
                let mut carry: u32 = 0;
                let (mut left_finished, mut right_finished) = (false, false);
                while !left_finished || !right_finished {
                    let (mut left_digit, mut right_digit) = (0 as u32, 0 as u32);

                    if !left_finished {
                        match consumer_left.pop() {
                            Some(x) => left_digit = x,
                            None => left_finished = true,
                        }
                    }

                    if !right_finished {
                        match consumer_right.pop() {
                            Some(x) => right_digit = x,
                            None => right_finished = true,
                        }
                    }

                    let base: u64 = 4294967296;
                    let new_digit: u32 = (((left_digit as u64) + (right_digit as u64) + carry as u64) % base) as u32;

                    producer.push(Some(new_digit));
                    carry = (((left_digit as u64) + (right_digit as u64) + (carry as u64)) / base) as u32;
                }

                if carry > 0 {
                    producer.push(Some(carry));
                }
                producer.push(None);
            });
        });
    }
}
