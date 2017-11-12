extern crate lab6;
use lab6::polynomial::Polynomial;

fn main() {
    let x = Polynomial::new_rand(3, -4, 4);
    let y = Polynomial::new_rand(2, -4, 4);

    println!("{}", x);
    println!("{}", y);

    println!("Sum is: \n{}", &x + &y);

}
