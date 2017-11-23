extern crate image;

use image::*;
use std::fs::File;
use std::path::Path;
use std::io;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

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
            .and_then(|ref mut file| self.img.save(file, image::PNG)
                .map_err(|err| err.to_string()))
    }
}
