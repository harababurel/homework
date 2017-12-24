extern crate carmichael;
extern crate rayon;
use rayon::prelude::*;

fn main() {
    let n = 100000;

    let xs: Vec<u64> = (1u64..n as u64).collect();

    xs.par_iter()
        .filter(|x| carmichael::is_carmichael(**x))
        .for_each(|x| {
            println!("{}", x);
        });

}
