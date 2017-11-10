extern crate num;
extern crate primal;

#[cfg(test)]
mod tests {
    #[test]
    fn power_test() {
        assert_eq!(::power(2, 10, 123), 1024 % 123);
    }

    #[test]
    fn carmichael_positives() {
        let xs: [u32; 6] = [561, 1105, 1729, 2465, 2821, 6601];
        for x in xs.iter() {
            assert!(::is_carmichael(*x));
        }
    }

    #[test]
    fn carmichael_negatives() {
        let xs: [u32; 6] = [562, 1103, 1731, 2469, 2816, 6605];
        for x in xs.iter() {
            assert!(!::is_carmichael(*x));
        }
    }
}

pub fn is_carmichael(n: u32) -> bool {
    if n <= 1 || primal::is_prime(n as u64) {
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

fn power(a: u32, b: u32, n: u32) -> u32 {
    let mut ret = 1;
    for _ in 0..b {
        ret = (ret * a) % n;
    }

    ret
}
