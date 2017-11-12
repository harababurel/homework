extern crate rand;
use self::rand::Rng;
use std::ops;
use std::fmt;
use std::cmp::{Ordering, max};

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

    pub fn degree(&self) -> usize {
        // TODO: find the index of the largest non-zero coefficient
        self.xs.len()
    }

    // pub fn set_coefficient(&mut self, exponent: usize, coefficient: i32) {
    //     if self.xs.len() < exponent + 1 {
    //         self.xs.resize(exponent + 1, 0);
    //     }

    //     self.xs[exponent] = coefficient;
    // }

    pub fn get(&self, exponent: usize) -> i32 {
        match self.xs.get(exponent) {
            Some(x) => *x,
            None => 0,
        }
    }

    pub fn get_mut(&mut self, exponent: usize) -> &mut i32 {
        if self.xs.len() < exponent + 1 {
            self.xs.resize(exponent + 1, 0);
        }

        self.xs.get_mut(exponent).unwrap()
    }
}

impl<'a, 'b> ops::Add<&'b Polynomial> for &'a Polynomial {
    type Output = Polynomial;

    fn add(self, _rhs: &'b Polynomial) -> Polynomial {
        // let xs: Vec<i32> = self.xs
        //     .iter()
        //     .zip(_rhs.xs.iter())
        //     .map(|x| x.0 + x.1)
        //     .collect();
        let mut ret = Polynomial::new();

        for i in 0..max(self.degree(), _rhs.degree()) {
            *ret.get_mut(i) = self.get(i) + _rhs.get(i);
        }

        // Polynomial { xs }
        ret
    }
}

impl fmt::Display for Polynomial {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;

        for x in self.xs.iter().enumerate().rev() {
            let exp = x.0;
            let coef = *x.1;

            if coef == 0 {
                continue;
            }

            let sign = match coef {
                1 => {
                    if exp + 1 == self.degree() {
                        String::new()
                    } else {
                        String::from("+")
                    }
                }
                -1 => String::from("-"),
                x => {
                    match x.cmp(&0) {
                        Ordering::Less => String::from(x.to_string()),
                        Ordering::Greater => format!("+{}", x.to_string()),
                        _ => String::new(),
                    }
                }
            };

            let repr = match exp {
                0 => format!("{}{}", sign, coef.abs().to_string()),
                1 => format!("{}x", sign),
                _ => format!("{}{}{}", sign, &String::from("x^"), &exp.to_string()),
            };

            write!(f, "{}", repr)?;
        }

        write!(f, ")")
    }
}
