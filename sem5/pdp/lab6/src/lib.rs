#![feature(iterator_for_each)]
extern crate either;
extern crate rand;
extern crate rayon;
extern crate crossbeam;

#[cfg(test)]
mod tests {
    extern crate rand;
    use rand::Rng;
    use polynomial::Polynomial;
    use multiplier;

    #[test]
    fn test_products() {
        test_product(vec![1, 1], vec![-1, 1], vec![-1, 0, 1]);
        test_product(vec![0, 0, 3], vec![7, -5, 4], vec![0, 0, 21, -15, 12]);
    }

    #[test]
    fn test_identity_product() {
        let x = Polynomial::new_rand(100, -100, 100);
        let one = Polynomial::from(vec![1]);
        assert_eq!(multiplier::seq_product(&x, &one), x);
        test_all_products_match(&x, &one);
    }

    #[test]
    fn test_negative_one_product() {
        let x = Polynomial::new_rand(100, -100, 100);
        let neg_1 = Polynomial { xs: vec![-1] };
        let neg_x = multiplier::par_product(&x, &neg_1);

        assert_eq!(multiplier::seq_product(&neg_x, &neg_1), x);
        test_all_products_match(&neg_x, &neg_1);
    }

    #[test]
    fn test_zero_product() {
        for _ in 0..10 {
            let x = Polynomial::new_rand(50, -100, 100);
            let zero = Polynomial::new();
            assert_eq!(multiplier::seq_product(&x, &zero), zero);
            assert_eq!(multiplier::par_product(&x, &zero), zero);
            assert_eq!(multiplier::new_product(&x, &zero), zero);
            assert_eq!(multiplier::seq_karatsuba(&x, &zero), zero);
        }
    }

    #[test]
    fn test_random_products() {
        for _ in 0..10 {
            let x_degree = rand::thread_rng().gen_range::<usize>(0, 200);
            let y_degree = rand::thread_rng().gen_range::<usize>(0, 200);
            let x = Polynomial::new_rand(x_degree, -1000, 1000);
            let y = Polynomial::new_rand(y_degree, -1000, 1000);

            assert_eq!(
                multiplier::seq_product(&x, &y),
                multiplier::seq_product(&y, &x)
            );

            test_all_products_match(&x, &y);
            test_all_products_match(&y, &x);
        }
    }

    #[test]
    fn test_divide_by_power() {
        let x = Polynomial::from(vec![0, 1, 2, 3, 4, 5, 0, 0]);
        let tests = vec![
            (0, x.clone()),
            (2, Polynomial::from(vec![2, 3, 4, 5])),
            (5, Polynomial::from(vec![5])),
            (6, Polynomial::new()),
            (7, Polynomial::new()),
            (8, Polynomial::new()),
            (100, Polynomial::new()),
        ];

        for (i, expected) in tests {
            let mut y = x.clone();
            y.divide_by_power(i);
            assert_eq!(y, expected);
        }
    }


    #[test]
    fn test_multiply_by_power() {
        let x = Polynomial::from(vec![0, 1, 5, 0, 0]);
        let tests = vec![
            (0, x.clone()),
            (2, Polynomial::from(vec![0, 0, 0, 1, 5, 0, 0])),
            (6, Polynomial::from(vec![0, 0, 0, 0, 0, 0, 0, 1, 5, 0, 0])),
        ];

        for (i, expected) in tests {
            assert_eq!(x.clone().multiply_by_power(i), expected);
        }
    }


    fn test_product(xs: Vec<i32>, ys: Vec<i32>, res: Vec<i32>) {
        let x = Polynomial { xs: xs };
        let y = Polynomial { xs: ys };
        let z = Polynomial { xs: res };

        assert_eq!(multiplier::seq_product(&x, &y), z);
        test_all_products_match(&x, &y);
    }

    pub fn test_all_products_match(a: &Polynomial, b: &Polynomial) {
        let x = multiplier::seq_product(&a, &b);
        let y = multiplier::par_product(&a, &b);
        let z = multiplier::new_product(&a, &b);
        let t = multiplier::seq_karatsuba(&a, &b);
        assert_eq!(x, y);
        assert_eq!(x, z);
        assert_eq!(x, y);
        assert_eq!(x, t);
    }
}

pub mod polynomial;
pub mod multiplier;
