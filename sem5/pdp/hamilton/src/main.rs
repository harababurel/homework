extern crate petgraph;
extern crate hamilton;

fn main() {
    let graph = hamilton::read_graph("data/infoarena10.json").unwrap();
    println!("{:?}", graph);

    match hamilton::find_cycle(&graph) {
        Some(cycle) => {
            println!("Detected cycle: {}", cycle.join(" -> "));
        }
        None => {
            println!("No cycle");
        }
    }
}
