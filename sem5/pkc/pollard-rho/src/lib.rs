extern crate num;

use num::bigint::BigInt;
use num::traits::Signed;
use num::integer::Integer;

#[cfg(test)]
mod tests {
    use num::bigint::BigInt;
    use super::*;

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

fn g(x: &BigInt, n: &BigInt) -> BigInt {
    (x * x + 1) % n
}

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

pub fn factorize(n: &BigInt) -> Vec<BigInt> {
    let mut n = (*n).clone();
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
