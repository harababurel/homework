extern crate crossbeam;

//use num::bigint::BigUint;
use bigint::BigUint;
use bounded_spsc_queue::Producer;
use bounded_spsc_queue;

pub struct Adder {
    xs: Vec<BigUint>,
    capacity: usize,
}

impl Adder {
    pub fn new() -> Adder {
        Adder {
            xs: vec![],
            capacity: 1e3 as usize,
        }
    }

    pub fn with_capacity(capacity: usize) -> Adder {
        Adder {
            xs: vec![],
            capacity,
        }
    }

    pub fn push(&mut self, x: BigUint) {
        self.xs.push(x);
    }

    pub fn compute(&self) -> BigUint {
        self.xs.iter().fold(BigUint::zero(), |acc, x| acc + x)
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

        return left + &right;
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
            for digit in _xs[0].digits() {
                producer.push(Some(*digit));
            }
            producer.push(None);
            return;
        }

        crossbeam::scope(|scope| {
            let mid = _xs.len() / 2;

            let (producer_left, consumer_left) = bounded_spsc_queue::make(self.capacity);
            scope.spawn(move || {
                self.compute_slice_par(&_xs[0..mid], producer_left);
            });

            let (producer_right, consumer_right) = bounded_spsc_queue::make(self.capacity);
            scope.spawn(move || {
                self.compute_slice_par(&_xs[mid..], producer_right);
            });

            scope.spawn(move || {
                let mut carry = 0 as u64;
                let (mut left_finished, mut right_finished) = (false, false);
                while !left_finished || !right_finished {
                    let (mut left_digit, mut right_digit) = (0 as u64, 0 as u64);

                    if !left_finished {
                        match consumer_left.pop() {
                            Some(x) => left_digit = x as u64,
                            None => left_finished = true,
                        }
                    }

                    if !right_finished {
                        match consumer_right.pop() {
                            Some(x) => right_digit = x as u64,
                            None => right_finished = true,
                        }
                    }

                    let new_digit = ((left_digit + right_digit + carry) % BigUint::base()) as u32;

                    producer.push(Some(new_digit));
                    carry = (left_digit + right_digit + carry) / BigUint::base();
                }

                if carry > 0 {
                    producer.push(Some(carry as u32));
                }
                producer.push(None);
            });
        });
    }
}
