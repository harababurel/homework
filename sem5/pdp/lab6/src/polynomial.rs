extern crate rand;
use self::rand::Rng;
use std::ops;
use std::iter::Zip;

#[derive(Debug)]
pub struct Polynomial {
    xs: Vec<i32>,
}

impl Polynomial {
    pub fn new(size: usize) -> Polynomial {
        let xs = vec![0; size];
        Polynomial { xs }
    }

    pub fn new_rand(size: usize, min: i32, max: i32) -> Polynomial {
        let mut xs = vec![0; size];
        let mut rng = rand::thread_rng();

        for x in xs.iter_mut() {
            *x = rng.gen_range::<i32>(min, max);
        }

        Polynomial { xs }

    }

    pub fn len(&self) -> usize {
        self.xs.len()
    }
}

impl ops::Add<Polynomial> for Polynomial {
    type Output = Option<Polynomial>;

    fn add(self, _rhs: Polynomial) -> Option<Polynomial> {
        if self.len() != _rhs.len() {
            return None;
        }

        // let ret = Polynomial::new(self.len());

        let ret_xs: Vec<i32> = self.xs
            .iter()
            .zip(_rhs.xs.iter())
            .map(|x| x.0 + x.1)
            .collect();

        Some(Polynomial { xs: ret_xs })
    }
}
