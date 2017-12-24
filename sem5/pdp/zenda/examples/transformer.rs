extern crate zenda;

use zenda::Transformer;

fn main() {
    // Construct a transformer on the given image.
    let mut transformer =
        Transformer::load("examples/some_pic.jpeg").expect("Could not create transformer");

    // Apply a filter on the image.
    transformer.find_edges();

    // Save it.
    transformer.save("/tmp/output.png").expect(
        "Could not save image.",
    );
}
