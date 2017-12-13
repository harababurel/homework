extern crate pollard_rho;
extern crate num;

use num::bigint::BigInt;

fn main() {
    let n = "563789213645".parse::<BigInt>().unwrap();
    pollard_rho::factorize(&n).iter().for_each(|factor| println!("factor: {}", factor));


//    let n = "1392".parse::<BigInt>().unwrap();
//    pollard_rho::factorize_but_slowly(&n).iter().for_each(|factor| println!("factor: {}", factor));
}