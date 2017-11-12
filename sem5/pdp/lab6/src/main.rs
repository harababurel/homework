extern crate lab6;
use lab6::polynomial::Polynomial;

fn main() {
    let x = Polynomial::new_rand(10, 0, 10);
    let y = Polynomial::new_rand(10, 0, 10);

    println!("size: {}", x.len());
    println!("{:?}", x);
    println!("{:?}", y);

    let sum = x + y;
    match sum {
        Some(poly) => println!("Sum is: \n{:?}", poly),
        None => println!("Mismatched sizes"),
    };

}
