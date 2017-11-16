extern crate lab6;
use std::time::Instant;
use lab6::polynomial::*;
use lab6::multiplier;

fn measure_runtime<F>(f: F) -> Polynomial
where
    F: FnOnce() -> Polynomial,
{

    let now = Instant::now();
    let product = f();
    let duration = now.elapsed();

    println!(
        "Duration: {:.3}s",
        duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9
    );

    product
}


fn main() {

    // let x = Polynomial::new_rand(4 as usize, -100, 100);
    // let y = Polynomial::new_rand(2 as usize, -100, 100);
    let x = Polynomial { xs: vec![-1, 1] };
    let y = Polynomial { xs: vec![1, 1] };

    println!("x = {}", x);
    println!("y = {}", y);
    println!();


    println!("x.degree() = {}", x.degree());

    println!("Running");
    let sum = measure_runtime(|| &x + &y);
    println!("{} + {} = {}", &x, &y, sum);


    let seq_product = measure_runtime(|| multiplier::seq_product(&x, &y));
    let par_product = measure_runtime(|| multiplier::par_product(&x, &y));

    println!("{} * {} = {:?}", &x, &y, seq_product);
    println!("{} * {} = {:?}", &x, &y, par_product);

    assert_eq!(seq_product, par_product);
}
