extern crate num;
extern crate primal;
extern crate rayon;

#[cfg(test)]
mod tests {
    #[test]
    // Test the power function
    fn power_test() {
        assert_eq!(::power(2, 10, 123), 1024 % 123);
    }

    #[test]
    // Test numbers which are Carmichael
    fn carmichael_positives() {
        let xs: [u64; 6] = [561, 1105, 1729, 2465, 2821, 6601];
        for x in xs.iter() {
            assert!(::is_carmichael(*x));
        }
    }

    #[test]
    // Test numbers which are NOT Carmichael
    fn carmichael_negatives() {
        let xs: [u64; 6] = [562, 1103, 1731, 2469, 2816, 6605];
        for x in xs.iter() {
            assert!(!::is_carmichael(*x));
        }
    }
}

// Returns whether or not an integer `n` is a Carmichael number.
pub fn is_carmichael(n: u64) -> bool {
    if n <= 1 || n % 2 == 0 || primal::is_prime(n as u64) {
        return false;
    }

    for b in 1..(n + 1) {
        if num::integer::gcd(b, n) == 1 {
            if power(b, n - 1, n) != 1 {
                return false;
            }
        }
    }

    true
}

// Returns a^b
fn power(a: u64, b: u64, n: u64) -> u64 {
    let mut ret = 1;
    for _ in 0..b {
        ret = (ret * a) % n;
    }

    ret
}
