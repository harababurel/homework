use super::*;

#[test]
fn test1() {
    run_test("data/1.json", true);
}

#[test]
fn test2() {
    run_test("data/2.json", true);
}

#[test]
fn test3() {
    run_test("data/3.json", true);
}

#[test]
fn infoarena1() {
    run_test("data/infoarena1.json", true);
}

#[test]
fn infoarena2() {
    run_test("data/infoarena2.json", false);
}

#[test]
fn infoarena3() {
    run_test("data/infoarena3.json", false);
}

#[test]
fn infoarena4() {
    run_test("data/infoarena4.json", true);
}

#[test]
fn infoarena5() {
    run_test("data/infoarena5.json", true);
}

#[test]
fn infoarena6() {
    run_test("data/infoarena6.json", true);
}

#[test]
fn infoarena7() {
    run_test("data/infoarena7.json", false);
}

#[test]
fn infoarena8() {
    run_test("data/infoarena8.json", false);
}

#[test]
fn infoarena9() {
    run_test("data/infoarena9.json", true);
}

#[test]
fn infoarena10() {
    run_test("data/infoarena10.json", true);
}

#[test]
fn infoarena11() {
    run_test("data/infoarena11.json", true);
}

#[test]
fn infoarena12() {
    run_test("data/infoarena12.json", true);
}

#[test]
fn infoarena13() {
    run_test("data/infoarena13.json", true);
}

#[test]
fn infoarena14() {
    run_test("data/infoarena14.json", true);
}

#[test]
fn infoarena15() {
    run_test("data/infoarena15.json", true);
}

#[test]
fn infoarena16() {
    run_test("data/infoarena16.json", true);
}

#[test]
fn infoarena17() {
    run_test("data/infoarena17.json", true);
}

#[test]
fn infoarena18() {
    run_test("data/infoarena18.json", true);
}

#[test]
fn infoarena19() {
    run_test("data/infoarena19.json", true);
}

#[test]
fn infoarena20() {
    run_test("data/infoarena20.json", true);
}

fn run_test(graph_filename: &str, has_cycle: bool) {
    let graph = read_graph(graph_filename).unwrap();
    let cycle = find_cycle(&graph);

    assert_eq!(cycle.is_some(), has_cycle);

    if cycle.is_some() {
        verify_cycle(&graph, &cycle.unwrap());
    }
}

fn verify_cycle(graph: &Graph, cycle: &Vec<String>) {
    assert_eq!(cycle.len(), 1 + graph.node_count());
    assert_eq!(cycle.first(), cycle.last());


    println!("Detected cycle: {}", cycle.join(" -> "));

    let mut encountered_nodes: HashSet<&str> = HashSet::new();
    for i in 0..cycle.len() - 1 {
        let (x, y) = (&cycle[i], &cycle[i + 1]);

        if !graph.is_adjacent(x, y) {
            println!("Edge {} -> {} does not exist!", &x, &y);
        }
        assert_eq!(true, graph.is_adjacent(x, y));

        encountered_nodes.insert(x);
        encountered_nodes.insert(y);
    }

    assert_eq!(encountered_nodes.len(), graph.node_count());
}
