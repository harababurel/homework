extern crate pollard_rho;
extern crate num;

use num::bigint::BigInt;

fn main() {
    let n = "9007199254740991".parse::<BigInt>().unwrap();
    pollard_rho::factorize(&n).iter().for_each(|factor| println!("factor: {}", factor));
}