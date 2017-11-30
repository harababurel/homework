extern crate bigsum;
extern crate rand;
extern crate num;

use bigsum::adder::Adder;
use num::bigint::ToBigUint;


fn main() {
    let mut adder = Adder::new();
    adder.push(1337.to_biguint().unwrap());
    adder.push(42.to_biguint().unwrap());
    adder.push(8.to_biguint().unwrap());
    adder.push(995.to_biguint().unwrap());

    let result = adder.compute_par();
    println!("result: {}", result);
}
