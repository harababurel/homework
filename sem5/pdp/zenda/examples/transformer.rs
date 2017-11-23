extern crate zenda;

use zenda::Transformer;

fn main() {
    let transformer = Transformer::load("examples/mango.jpg")
        .expect("Could not create transformer");

    transformer.save("/tmp/mango.png").expect("Could not save image.");
}