use crossbeam;
use polynomial::Polynomial;
use rayon::prelude::*;
use std::cmp;
use std::cmp::Ordering;

pub fn seq_product<'a, 'b>(a: &'a Polynomial, b: &Polynomial) -> Polynomial {
    let mut product = Polynomial::with_degree(a.degree() + b.degree());

    for i in 0..1 + a.degree() {
        for j in 0..1 + b.degree() {
            *product.get_mut(i + j) += a.get(i) * b.get(j);
        }
    }

    product.resize_to_fit();
    product
}

pub fn par_imperative_product<'a, 'b>(a: &'a Polynomial, b: &'b Polynomial) -> Polynomial {
    let (&a, &b) = match a.degree().cmp(&b.degree()) {
        Ordering::Less => (&a, &b),
        _ => (&b, &a),
    };
    let mut product = Polynomial::with_degree(a.degree() + b.degree());
    product.xs.par_iter_mut().enumerate().for_each(|(i, x)| {
        *x = 0;
        for j in 0..1 + cmp::min(i, a.degree()) {
            *x += a.get(j) * b.get(i - j);
        }
    });

    product.resize_to_fit();
    product
}

pub fn par_functional_product<'a, 'b>(a: &'a Polynomial, b: &'b Polynomial) -> Polynomial {
    let (&a, &b) = match a.degree().cmp(&b.degree()) {
        Ordering::Less => (&a, &b),
        _ => (&b, &a),
    };

    let mut product = Polynomial::from(
        (0..1 + a.degree() + b.degree())
            .into_par_iter()
            .map(|i| {
                (0..1 + cmp::min(i, a.degree()))
                    .map(|j| a.get(j) * b.get(i - j))
                    .sum()
            })
            .collect::<Vec<i32>>(),
    );
    product.resize_to_fit();
    product
}


pub fn seq_karatsuba<'a, 'b>(x: &'a Polynomial, y: &'b Polynomial) -> Polynomial {
    if cmp::min(x.degree(), y.degree()) <= 210 {
        return par_functional_product(&x, &y);
    }

    let n = cmp::max(x.degree(), y.degree());
    let m = n / 2;

    let mut x0 = x.clone();
    x0.resize(m - 1);
    let mut x1 = x.clone();
    x1.divide_by_power(m);

    let mut y0 = y.clone();
    y0.resize(m - 1);
    let mut y1 = y.clone();
    y1.divide_by_power(m);

    let z0 = seq_karatsuba(&x0, &y0);
    let z2 = seq_karatsuba(&x1, &y1);
    let z1 = seq_karatsuba(&(x0 + x1), &(y0 + y1)) - &z2 - &z0;

    (z2.multiply_by_power(m) + z1).multiply_by_power(m) + z0
}


pub fn par_karatsuba<'a, 'b>(x: &'a Polynomial, y: &'b Polynomial) -> Polynomial {
    if cmp::min(x.degree(), y.degree()) <= 120 {
        return par_functional_product(&x, &y);
    }

    let n = cmp::max(x.degree(), y.degree());
    let m = n / 2;

    let mut x0 = x.clone();
    x0.resize(m - 1);
    let mut x1 = x.clone();
    x1.divide_by_power(m);

    let mut y0 = y.clone();
    y0.resize(m - 1);
    let mut y1 = y.clone();
    y1.divide_by_power(m);
    let z0_handle = crossbeam::scope(|scope| scope.spawn(|| par_karatsuba(&x0, &y0)));

    let z2_handle = crossbeam::scope(|scope| scope.spawn(|| par_karatsuba(&x1, &y1)));

    let z0 = z0_handle.join();
    let z2 = z2_handle.join();

    let z1_handle = crossbeam::scope(|scope| {
        scope.spawn(|| par_karatsuba(&(x0 + x1), &(y0 + y1)) - &z2 - &z0)
    });

    (z2.multiply_by_power(m) + z1_handle.join()).multiply_by_power(m) + z0
}
