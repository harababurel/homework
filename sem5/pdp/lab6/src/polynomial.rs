extern crate rand;
use self::rand::Rng;
use std::ops;
use std::fmt;
use std::cmp::Ordering;

#[derive(Debug)]
pub struct Polynomial {
    xs: Vec<i32>,
}

impl Polynomial {
    pub fn new() -> Polynomial {
        Polynomial { xs: Vec::new() }
    }

    pub fn new_rand(degree: usize, min: i32, max: i32) -> Polynomial {
        let mut rng = rand::thread_rng();

        let xs: Vec<i32> = (0..degree)
            .map(|_| rng.gen_range::<i32>(min, max))
            .collect();

        Polynomial { xs }

    }

    pub fn len(&self) -> usize {
        self.xs.len()
    }
}

impl<'a, 'b> ops::Add<&'b Polynomial> for &'a Polynomial {
    type Output = Polynomial;

    fn add(self, _rhs: &'b Polynomial) -> Polynomial {
        let xs: Vec<i32> = self.xs
            .iter()
            .zip(_rhs.xs.iter())
            .map(|x| x.0 + x.1)
            .collect();

        Polynomial { xs }
    }
}

impl fmt::Display for Polynomial {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;

        for x in self.xs.iter().enumerate().rev() {
            let exp = x.0;

            let sign = match x.1.cmp(&0) {
                Ordering::Less => " -",
                Ordering::Equal => continue,
                Ordering::Greater => " +",
            };

            let coef = match x.1 {
                &-1 => String::new(),
                &1 => String::new(),
                &x => x.abs().to_string(),
            };

            write!(f, "{}{}x^{}", sign, coef, exp)?;
        }

        write!(f, ")")
    }
}
