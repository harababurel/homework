extern crate num;
extern crate primal;

use num::bigint::BigInt;
use num::traits::{Signed, ToPrimitive, One};
use num::integer::Integer;

#[cfg(test)]
mod tests {
    use num::bigint::BigInt;
    use std::time::Instant;
    use super::*;

    fn measure_runtime<F>(f: F)
        where
            F: FnOnce() -> Vec<BigInt>,
    {
        let now = Instant::now();
        f();
        let duration = now.elapsed();
        println!(
            "{:.3}s",
            duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9
        );
    }

    #[test]
    fn pollard_rho_runtime_test() {
        ["9007199254740881",
            "9007199254740997",
            "9007199254740991",
            "9007199254740992",
            "9999986200004761",
            "99999989237606677",
            "999999866000004473",
            "9999999942014077477",
            "99999980360000964323",
            "99999980380000962361",
            "99994452415200570527",
            "99999999999999999999",
            "9960060529"]
            .iter()
            .map(|x| x.parse::<BigInt>().unwrap())
            .for_each(|x| {
                print!("Factorizing {} using Pollard Rho... ", &x);
                measure_runtime(|| factorize(&x));
            });
    }

        // #[test]
    fn slow_factorization_runtime_test() {
        [
//            "9007199254740991",
//            "9007199254740992",
//            "9999986200004761",
//            "99999989237606677",
//            "999999866000004473",
//            "9999999942014077477",
//            "99999980360000964323",
//            "99999980380000962361",
            "99994452415200570527",
            "99999999999999999999"]
            .iter()
            .map(|x| x.parse::<BigInt>().unwrap())
            .for_each(|x| {
                print!("Factorizing {} using basic algorithm... ", &x);
                measure_runtime(|| factorize_but_slowly(&x));
            });
    }


    #[test]
    fn factorization_test() {
        test_factors("121", &["11", "11"]);
        test_factors("9007199254740991", &["6361", "69431", "20394401"]);
    }

    #[test]
    fn product_test() {
        ["1234", "4096", "42", "409532421321"].iter().for_each(|x| test_product(x));
    }


    fn test_factors(n: &str, expected_factors: &[&str]) {
        let n = n.parse::<BigInt>().unwrap();
        let expected_factors: Vec<BigInt> = expected_factors.iter().map(|x| x.parse::<BigInt>().unwrap()).collect();

        assert_eq!(factorize(&n), expected_factors);
    }

    fn test_product(n: &str) {
        let n = n.parse::<BigInt>().unwrap();
        assert_eq!(factorize(&n)
                       .iter()
                       .fold(BigInt::from(1), |acc, x| acc * x),
                   n);
    }
}


// Polynomial function to be used by the Pollard rho algorithm
fn g(x: &BigInt, n: &BigInt) -> BigInt {
//    (x * x + 1) % n
    (x*x + x + 1) % n
}

// Uses Pollard rho to generate an arbitrary non-trivial factor of `n`. Returns None in case of failure.
fn get_factor(n: &BigInt) -> Option<BigInt> {
    let mut x = BigInt::from(2);
    let mut y = BigInt::from(2);
    let mut d = BigInt::from(1);

    while d == BigInt::from(1) {
        x = g(&x, &n);
        y = g(&g(&y, &n), &n);
        d = (&x - &y).abs().gcd(n);
    }

    if d == *n { None } else { Some(d) }
}

// Factorizes `n` using the Pollard rho algorithm.
// Returns a vector of all factors.
pub fn factorize(n: &BigInt) -> Vec<BigInt> {
    let mut n = (*n).clone();

    match n.to_u64() {
        Some(x) => {
            if primal::is_prime(x) {
                return vec![n];
            }
        }
        None => (),
    }

    let mut factors = Vec::new();
    loop {
        match get_factor(&n) {
            Some(factor) => {
                n = n / &factor;
                factors.push(factor);
            }
            None => {
                factors.push(n);
                break;
            }
        };
    }

    factors.sort();
    factors
}

// Factorizes `n` using the trivial algorithm.
// Performs best on small factors.
pub fn factorize_but_slowly(n: &BigInt) -> Vec<BigInt> {
    let mut n = (*n).clone();
    let mut factors = Vec::new();

    let mut factor = BigInt::from(2);
    while factor <= n {
        while n.is_multiple_of(&factor) {
            n = n / &factor;
            factors.push(factor.clone());
        }
        factor = factor + BigInt::one();
    }

    factors
}
