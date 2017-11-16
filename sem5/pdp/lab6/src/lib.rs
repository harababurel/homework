#![feature(iterator_for_each)]
extern crate rayon;
extern crate rand;

#[cfg(test)]
mod tests {
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
        assert_eq!(multiplier::seq_product(&x, &Polynomial { xs: vec![1] }), x);
        assert_eq!(multiplier::par_product(&x, &Polynomial { xs: vec![1] }), x);
    }

    #[test]
    fn test_negative_one_product() {
        let x = Polynomial::new_rand(100, -100, 100);
        let neg_1 = Polynomial { xs: vec![-1] };
        let neg_x = multiplier::par_product(&x, &neg_1);

        assert_eq!(multiplier::par_product(&neg_x, &neg_1), x);
    }

    #[test]
    fn test_zero_product() {
        for _ in 0..10 {
            let x = Polynomial::new_rand(50, -100, 100);
            let zero = Polynomial::new();
            assert_eq!(multiplier::seq_product(&x, &zero), zero);
            assert_eq!(multiplier::par_product(&x, &zero), zero);
        }
    }

    #[test]
    fn test_commutativity() {
        for _ in 0..50 {
            let x = Polynomial::new_rand(50, -100, 100);
            let y = Polynomial::new_rand(50, -100, 100);

            assert_eq!(
                multiplier::seq_product(&x, &y),
                multiplier::seq_product(&y, &x)
            );

            assert_eq!(
                multiplier::par_product(&x, &y),
                multiplier::par_product(&y, &x)
            );
        }
    }


    fn test_product(xs: Vec<i32>, ys: Vec<i32>, res: Vec<i32>) {
        let x = Polynomial { xs: xs };
        let y = Polynomial { xs: ys };
        let z = Polynomial { xs: res };

        assert_eq!(multiplier::seq_product(&x, &y), z);
        assert_eq!(multiplier::par_product(&x, &y), z);
    }
}

pub mod polynomial;
pub mod multiplier;
