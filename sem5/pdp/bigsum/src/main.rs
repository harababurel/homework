extern crate bigsum;
extern crate rand;
extern crate num;

use bigsum::adder::Adder;
use bigsum::bigint::BigUint;


fn main() {
    let mut adder = Adder::new();
    adder.push(BigUint::from(1234));
    adder.push(BigUint::from(42));
    adder.push(BigUint::from(8));
    adder.push(BigUint::from(995));

    let result = adder.compute_par();
    println!("result: {:?}", result);
}
