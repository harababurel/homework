extern crate zenda;

use zenda::Transformer;

fn main() {
    let mut transformer = Transformer::load("examples/sheep.jpeg")
        .expect("Could not create transformer");

    transformer.find_edges();
    transformer.save("/tmp/output.png").expect("Could not save image.");
}