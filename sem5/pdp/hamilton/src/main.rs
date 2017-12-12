extern crate petgraph;
extern crate hamilton;

fn main() {
    let graph = hamilton::read_graph("data/1.json").unwrap();
    println!("{:?}", graph);

    hamilton::find_cycle(&graph);
}
