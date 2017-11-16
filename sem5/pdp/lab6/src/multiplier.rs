use polynomial::Polynomial;
use rayon::prelude::*;
use std::cmp;

pub fn seq_product<'a, 'b>(a: &'a Polynomial, b: &Polynomial) -> Polynomial {
    let mut product = Polynomial::new_with_degree(a.degree() + b.degree());

    for i in 0..1 + a.degree() {
        for j in 0..1 + b.degree() {
            *product.get_mut(i + j) += a.get(i) * b.get(j);
        }
    }

    product.resize_to_fit()
    // product
}

pub fn par_product<'a, 'b>(a: &'a Polynomial, b: &'b Polynomial) -> Polynomial {
    let mut product = Polynomial::new_with_degree(a.degree() + b.degree());
    product.xs.par_iter_mut().enumerate().for_each(|(i, x)| {
        *x = 0;
        for j in 0..1 + cmp::min(i, a.degree()) {
            let k = i - j;

            if k <= b.degree() {
                *x += a.get(j) * b.get(k);
            }
        }
    });

    product.resize_to_fit()
}
