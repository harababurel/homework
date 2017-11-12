extern crate lab6;
use lab6::polynomial::*;
use lab6::multiplier;

fn main() {
    let mut x = Polynomial::new();
    let mut y = Polynomial::new();

    *x.get_mut(0) = 1;
    *x.get_mut(1) = 1;

    *y.get_mut(0) = -1;
    *y.get_mut(1) = 1;

    println!("{} + {} = {}", &x, &y, &x + &y);
    println!("{} * {} = {}", &x, &y, multiplier::seq_product(&x, &y));

}
