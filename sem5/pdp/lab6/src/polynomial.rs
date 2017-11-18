use rand;
use rand::Rng;
use std::ops;
use std::fmt;
use std::cmp::{min, max};

#[derive(Clone, Debug)]
pub struct Polynomial {
    pub xs: Vec<i32>,
}

impl<'a> Polynomial {
    pub fn new() -> Polynomial {
        Polynomial { xs: vec![0] }
    }

    pub fn with_degree(degree: usize) -> Polynomial {
        Polynomial { xs: vec![0; degree + 1] }
    }

    pub fn new_rand(degree: usize, min: i32, max: i32) -> Polynomial {
        Polynomial {
            xs: (0..degree)
                .map(|i| if i + 1 == degree {
                    let mut x = 0;
                    while x == 0 {
                        x = rand::thread_rng().gen_range::<i32>(min, max);
                    }
                    x
                } else {
                    rand::thread_rng().gen_range::<i32>(min, max)
                })
                .collect(),
        }


    }

    pub fn degree(&self) -> usize {
        match self.xs
            .iter()
            .enumerate()
            .rev()
            .skip_while(|&(_, &x)| x == 0)
            .take(1)
            .last() {
            Some((i, _)) => i,
            None => 0,
        }
    }

    pub fn get(&self, exponent: usize) -> i32 {
        match self.xs.get(exponent) {
            Some(x) => *x,
            None => 0,
        }
    }

    pub fn get_mut(&mut self, exponent: usize) -> &mut i32 {
        if exponent > self.degree() {
            self.resize(exponent)
        }

        self.xs.get_mut(exponent).unwrap()
    }

    pub fn resize(&mut self, degree: usize) {
        self.xs.resize(degree + 1, 0);
    }

    pub fn resize_to_fit(&mut self) {
        loop {
            if self.xs.len() == 1 {
                break;
            }
            match self.xs.last() {
                Some(&0) => self.xs.pop(),
                _ => break,
            };
        }
    }

    pub fn divide_by_power(&mut self, exponent: usize) {
        let upper = min(self.xs.len(), exponent);
        self.xs.drain(0..upper);
    }

    pub fn multiply_by_power(mut self, exponent: usize) -> Self {
        self.xs.reverse();
        self.xs.extend(vec![0; exponent]);
        self.xs.reverse();
        self
    }
}

impl<'a, 'b> ops::Add<&'b Polynomial> for &'a Polynomial {
    type Output = Polynomial;

    fn add(self, _rhs: &'b Polynomial) -> Polynomial {
        let mut ret = Polynomial::new();

        (0..1 + max(self.degree(), _rhs.degree())).for_each(|i| {
            *ret.get_mut(i) = self.get(i) + _rhs.get(i)
        });

        ret.resize_to_fit();
        ret
    }
}

impl ops::Add<Polynomial> for Polynomial {
    type Output = Polynomial;

    fn add(self, _rhs: Polynomial) -> Polynomial {
        let mut ret = Polynomial::new();

        (0..1 + max(self.degree(), _rhs.degree())).for_each(|i| {
            *ret.get_mut(i) = self.get(i) + _rhs.get(i)
        });

        ret.resize_to_fit();
        ret
    }
}


impl<'a, 'b> ops::Sub<&'a Polynomial> for &'b Polynomial {
    type Output = Polynomial;

    fn sub(self, _rhs: &'a Polynomial) -> Polynomial {
        let mut ret = Polynomial::new();

        (0..1 + max(self.degree(), _rhs.degree())).for_each(|i| {
            *ret.get_mut(i) = self.get(i) - _rhs.get(i)
        });

        ret.resize_to_fit();
        ret
    }
}

impl<'a, 'b> ops::Sub<&'a Polynomial> for Polynomial {
    type Output = Polynomial;

    fn sub(self, _rhs: &'a Polynomial) -> Polynomial {
        &self - _rhs
    }
}


impl<'a> ops::Sub<Polynomial> for &'a Polynomial {
    type Output = Polynomial;

    fn sub(self, _rhs: Polynomial) -> Polynomial {
        self - &_rhs
    }
}


// impl<'a, 'b> ops::SubAssign<&'b Polynomial> for &'a Polynomial {
//     fn sub_assign(&mut self, _rhs: &'b Polynomial) {
//         let mut ret = Polynomial::new();

//         (0..1 + max(self.degree(), _rhs.degree())).for_each(|i| {
//             *ret.get_mut(i) = self.get(i) - _rhs.get(i)
//         });
//         ret.resize_to_fit();
//         // self.xs = ret.xs;
//         self = &mut ret.clone();
//     }
// }

impl<'a> fmt::Display for Polynomial {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;

        for (exp, &coef) in self.xs.iter().enumerate().rev() {

            if coef == 0 {
                continue;
            }

            let sign = match (exp == self.degree(), coef < 0) {
                (true, true) => "-",
                (true, false) => "",
                (false, true) => " - ",
                (false, false) => " + ",
            };

            let coef = if coef.abs() == 1 && exp != 0 {
                String::new()
            } else {
                coef.abs().to_string()
            };

            match exp {
                0 => write!(f, "{}{}", sign, coef)?,
                1 => write!(f, "{}{}x", sign, coef)?,
                _ => write!(f, "{}{}x^{}", sign, coef, exp)?,
            };

        }

        write!(f, ")")
    }
}

impl PartialEq for Polynomial {
    fn eq(&self, other: &Polynomial) -> bool {
        self.degree() == other.degree() &&
            self.xs.iter().zip(&other.xs).fold(true, |acc, (x, y)| {
                acc & (x == y)
            })
    }
}

impl From<Vec<i32>> for Polynomial {
    fn from(xs: Vec<i32>) -> Polynomial {
        Polynomial { xs }
    }
}
