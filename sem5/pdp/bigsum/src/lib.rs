extern crate num;
extern crate itertools;
extern crate rand;
extern crate lossyq;
extern crate bounded_spsc_queue;
extern crate cpuprofiler;

#[cfg(test)]
mod tests {
    use adder::Adder;
    use rand;
    use rand::Rng;
    use bigint::BigUint;
    use std::time::Instant;
    use cpuprofiler::PROFILER;


    fn measure_runtime<F>(f: F) -> BigUint
        where
            F: FnOnce() -> BigUint,
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


    #[test]
    fn test_recursive_addition() {
        let n_digits = 1000000;
        let n_values = 4;

        let mut rng = rand::thread_rng();
        let mut adder = Adder::with_capacity(n_digits + 10);
        let mut expected_sum = BigUint::zero();

        (0..n_values).for_each(|_| {
            let digits = (0..n_digits).map(|_| rng.gen::<u32>()).collect();
            let x = BigUint::new(digits);

            expected_sum = &expected_sum + &x;
            adder.push(x);
        });


        assert_eq!(expected_sum, measure_runtime(|| adder.compute()));
        assert_eq!(expected_sum, measure_runtime(|| adder.compute_rec()));
//        PROFILER.lock().unwrap().start("./parallel-sum.profile").expect("Couldn't start");
        assert_eq!(expected_sum, measure_runtime(|| adder.compute_par()));
//        PROFILER.lock().unwrap().stop().expect("Couldn't stop");
    }
}

pub mod adder;
pub mod bigint;
