#![feature(range_contains)]
extern crate image;
extern crate rayon;

use image::*;
use std::f64::consts;
use std::fs::File;
use std::path::Path;
use std::cmp;
use rayon::prelude::*;

pub struct Transformer {
    pub img: DynamicImage,
}

impl Transformer {
    pub fn new(width: u32, height: u32) -> Transformer {
        Transformer { img: DynamicImage::new_rgb8(width, height) }
    }

    pub fn load(filepath: &str) -> Result<Transformer, ImageError> {
        match image::open(&Path::new(&filepath)) {
            Ok(img) => Ok(Transformer { img }),
            Err(e) => Err(e),
        }
    }

    pub fn save(&self, filepath: &str) -> Result<(), String> {
        File::create(&Path::new(filepath))
            .map_err(|err| err.to_string())
            .and_then(|ref mut file| {
                self.img.save(file, image::PNG).map_err(
                    |err| err.to_string(),
                )
            })
    }

    pub fn sharpen(&mut self) {
        self.apply_kernel(
            &vec![
                vec![-1.0, -1.0, -1.0, -1.0, -1.0],
                vec![-1.0, 2.0, 2.0, 2.0, -1.0],
                vec![-1.0, 2.0, 8.0, 2.0, -1.0],
                vec![-1.0, 2.0, 2.0, 2.0, -1.0],
                vec![-1.0, -1.0, -1.0, -1.0, -1.0],
            ],
            1.0 / 8.0,
            0.0,
        );
    }

    pub fn box_blur(&mut self, size: usize) {
        let mut kernel = Vec::new();
        kernel.resize(size, vec![1.0; size]);

        self.apply_kernel(&kernel, 1.0 / (size as f64).powi(2), 0.0);
    }

    pub fn gaussian_blur(&mut self, size: usize, sigma: f64) {
        let mut kernel = Vec::new();
        kernel.resize(size, vec![0.0; size]);

        let sigma_sq = sigma.powi(2);
        let mut sum: f64 = 0.0;
        for i in 0..size {
            for j in 0..size {
                let x_sq = (i as f64 - (size / 2) as f64).powi(2);
                let y_sq = (j as f64 - (size / 2) as f64).powi(2);

                kernel[i][j] = consts::E.powf(-(x_sq + y_sq) / (2.0 * sigma_sq)) /
                    (2.0 * sigma_sq * consts::PI);
                sum += kernel[i][j];
            }
        }

        for i in 0..size {
            for j in 0..size {
                kernel[i][j] /= sum;
            }
        }

        self.apply_kernel(&kernel, 1.0, 0.0);
    }

    pub fn motion_blur(&mut self, size: usize) {
        let mut kernel = Vec::new();
        kernel.resize(size, vec![0.0; size]);

        for (i, line) in kernel.iter_mut().enumerate() {
            line[i] = 1.0;
        }

        self.apply_kernel(&kernel, 1.0 / (size as f64), 0.0);
    }

    pub fn find_edges(&mut self) {
        self.apply_kernel(
            &vec![
                vec![-1.0, -1.0, -1.0],
                vec![-1.0, 8.0, -1.0],
                vec![-1.0, -1.0, -1.0],
            ],
            1.0,
            0.0,
        );
        self.img = self.img.grayscale();
    }

    pub fn emboss(&mut self, size: usize, grayscale: bool) {
        let mut kernel = Vec::new();
        kernel.resize(size, vec![0.0; size]);

        for i in 0..size {
            for j in 0..size {
                kernel[i][j] = match (i + j + 1).cmp(&size) {
                    std::cmp::Ordering::Less => -1.0,
                    std::cmp::Ordering::Equal => 0.0,
                    std::cmp::Ordering::Greater => 1.0,
                };
            }
        }

        self.apply_kernel(&kernel, 1.0, 128.0);

        if grayscale {
            self.img = self.img.grayscale();
        }
    }

    fn fit_in_range(x: i32, range: std::ops::Range<u32>) -> u32 {
        cmp::max(range.start as i32, cmp::min(range.end as i32 - 1, x)) as u32
    }

    fn apply_kernel(&mut self, kernel: &Vec<Vec<f64>>, factor: f64, bias: f64) {
        let mut new_image = self.img.clone();
        let kernel_delta = kernel.len() / 2;

        let pixels: Vec<_> = self.img.pixels().into_iter().collect();
        let transformed_pixels: Vec<_> = pixels
            .into_par_iter()
            .map(|pixel| {
                let new_values: Vec<u8> = (0..3)
                    .map(|channel| {
                        let mut value = 0.0;
                        for kernel_i in 0..kernel.len() {
                            for kernel_j in 0..kernel.len() {
                                let img_i = Transformer::fit_in_range(
                                    pixel.0 as i32 + kernel_i as i32 - kernel_delta as i32,
                                    0..self.img.width(),
                                );
                                let img_j = Transformer::fit_in_range(
                                    pixel.1 as i32 + kernel_j as i32 - kernel_delta as i32,
                                    0..self.img.height(),
                                );
                                let neighbor = self.img.get_pixel(img_i, img_j);
                                value += kernel[kernel_i][kernel_j] * neighbor.data[channel] as f64;
                            }
                        }

                        Transformer::fit_in_range((value * factor + bias) as i32, (0..255)) as u8
                    })
                    .collect();

                (
                    pixel.0,
                    pixel.1,
                    Rgba(
                        [new_values[0], new_values[1], new_values[2], pixel.2.data[3]],
                    ),
                )
            })
            .collect();

        transformed_pixels.into_iter().for_each(|x| {
            new_image.put_pixel(x.0, x.1, x.2);
        });

        self.img = new_image;
    }
}
