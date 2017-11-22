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
    // let x = Polynomial::new_rand(1e3 as usize, -100, 100);
    // let y = Polynomial::new_rand(1e3 as usize, -100, 100);

    let x = Polynomial::from(vec![8, 9, 0, -3]);
    let y = Polynomial::from(vec![5, -3, 1, 1]);

    println!("Running");

    let seq_product = measure_runtime(|| multiplier::seq_product(&x, &y));
    let par_imperative_product = measure_runtime(|| multiplier::par_imperative_product(&x, &y));
    let par_functional_product = measure_runtime(|| multiplier::par_functional_product(&x, &y));
    let seq_karatsuba = measure_runtime(|| multiplier::seq_karatsuba(&x, &y));
    let par_karatsuba = measure_runtime(|| multiplier::par_karatsuba(&x, &y));

    println!("{} * {} = {}", &x, &y, par_imperative_product);

    assert_eq!(seq_product, par_imperative_product);
    assert_eq!(seq_product, par_functional_product);
    assert_eq!(seq_product, seq_karatsuba);
    assert_eq!(seq_product, par_karatsuba);
}
