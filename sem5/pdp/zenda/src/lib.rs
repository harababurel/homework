#![feature(range_contains)]
extern crate image;
extern crate rayon;

use image::*;
use std::fs::File;
use std::path::Path;
use std::cmp;
use rayon::prelude::*;

pub struct Transformer {
    pub img: DynamicImage
}

impl Transformer {
    pub fn new(width: u32, height: u32) -> Transformer {
        Transformer { img: DynamicImage::new_rgb8(width, height) }
    }

    pub fn load(filepath: &str) -> Result<Transformer, ImageError> {
        let img = image::open(&Path::new(&filepath));

        match img {
            Ok(img) => Ok(Transformer { img }),
            Err(e) => Err(e),
        }
    }

    pub fn save(&self, filepath: &str) -> Result<(), String> {
        File::create(&Path::new(filepath))
            .map_err(|err| err.to_string())
            .and_then(|ref mut file|
                self.img
                    .save(file, image::PNG)
                    .map_err(|err| err.to_string()))
    }

    pub fn sharpen(&mut self) {
        self.apply_kernel(&vec![vec![-1.0, -1.0, -1.0, -1.0, -1.0],
                                vec![-1.0, 2.0, 2.0, 2.0, -1.0],
                                vec![-1.0, 2.0, 8.0, 2.0, -1.0],
                                vec![-1.0, 2.0, 2.0, 2.0, -1.0],
                                vec![-1.0, -1.0, -1.0, -1.0, -1.0]],
                          1.0 / 8.0,
                          0.0);
    }

    pub fn blur(&mut self, amount: usize) {
        let mut kernel: Vec<Vec<f32>> = Vec::new();
        kernel.resize(amount, vec![1.0; amount]);


        self.apply_kernel(&kernel, 1.0 / (amount * amount) as f32, 0.0);
    }

    pub fn gaussian_blur(&mut self, amount: usize) {
        // TODO: use provided amount
        self.apply_kernel(&vec![vec![1.0, 4.0, 6.0, 4.0, 1.0],
                                vec![4.0, 16.0, 24.0, 16.0, 4.0],
                                vec![6.0, 24.0, 36.0, 24.0, 6.0],
                                vec![4.0, 16.0, 24.0, 16.0, 4.0],
                                vec![1.0, 4.0, 6.0, 4.0, 1.0]],
                          1.0,
                          0.0);
    }

    pub fn motion_blur(&mut self, amount: usize) {
        let mut kernel: Vec<Vec<f32>> = Vec::new();
        kernel.resize(amount, vec![0.0; amount]);

        for (i, line) in kernel.iter_mut().enumerate() {
            line[i] = 1.0;
        }

        self.apply_kernel(&kernel,
                          1.0 / (amount as f32),
                          0.0);
    }


    pub fn find_edges(&mut self) {
        self.apply_kernel(&vec![vec![-1.0, -1.0, -1.0],
                                vec![-1.0, 8.0, -1.0],
                                vec![-1.0, -1.0, -1.0]],
                          1.0,
                         0.0);
    }


    pub fn emboss(&mut self) {
        self.apply_kernel(&vec![vec![-1.0, -1.0, 0.0],
                                vec![-1.0, 0.0, 1.0],
                                vec![0.0, 1.0, 1.0]],
                          1.0,
                          128.0);
    }


    pub fn hard_emboss(&mut self) {
        self.apply_kernel(&vec![vec![-1.0, -1.0, -1.0, -1.0, 0.0],
                                vec![-1.0, -1.0, -1.0, 0.0, 1.0],
                                vec![-1.0, -1.0, 0.0, 1.0, 1.0],
                                vec![-1.0, 0.0, 1.0, 1.0, 1.0],
                                vec![0.0, 1.0, 1.0, 1.0, 1.0]],
                          1.0,
                          128.0);
    }

    fn apply_kernel(&mut self, kernel: &Vec<Vec<f32>>, factor: f32, bias: f32) {
        let mut new_image = self.img.clone();
        let kernel_delta = kernel.len() / 2;

        let pixels: Vec<_> = self.img.pixels().into_iter().collect();
        let transformed_pixels: Vec<_> = pixels.into_par_iter().map(|pixel| {
            let new_values: Vec<u8> = (0..4).map(|channel| {
                let mut value = 0.0;

                for kernel_i in 0..kernel.len() {
                    for kernel_j in 0..kernel.len() {
                        let img_i = pixel.0 as i32 + kernel_i as i32 - kernel_delta as i32;
                        let img_j = pixel.1 as i32 + kernel_j as i32 - kernel_delta as i32;

                        if (0..self.img.width()).contains(img_i as u32) &&
                            (0..self.img.height()).contains(img_j as u32) {
                            let neighbor = self.img.get_pixel(img_i as u32, img_j as u32);
                            value += kernel[kernel_i][kernel_j] * neighbor.data[channel] as f32;
                        }
                    }
                }

                cmp::max(0, cmp::min(255, (value * factor + bias) as i32)) as u8
            }).collect();

            (pixel.0, pixel.1, Rgba([new_values[0], new_values[1], new_values[2], pixel.2.data[3]]))
        }).collect();

        transformed_pixels
            .into_iter()
            .for_each(|x| {
                new_image.put_pixel(x.0, x.1, x.2);
            });

        self.img = new_image;
    }
}
