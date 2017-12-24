extern crate fixedbitset;
extern crate petgraph;
extern crate rayon;
extern crate serde_json;

use fixedbitset::FixedBitSet;
use petgraph::graphmap::DiGraphMap;
use petgraph::visit::GetAdjacencyMatrix;
use rayon::prelude::*;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{Read, Result};

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub struct Graph {
    pub label_to_id: HashMap<String, usize>,
    pub labels: Vec<String>,
    pub graph: DiGraphMap<usize, ()>,
}

impl Graph {
    pub fn new() -> Graph {
        Graph {
            label_to_id: HashMap::new(),
            labels: Vec::new(),
            graph: DiGraphMap::new(),
        }
    }

    pub fn add_node(&mut self, label: &str) {
        let id = self.labels.len();
        self.label_to_id.insert(label.to_string(), id);
        self.labels.push(label.to_string());

        self.graph.add_node(id);
    }

    pub fn add_edge(&mut self, a: &str, b: &str) {
        if !self.label_to_id.contains_key(a) {
            self.add_node(a);
        }

        if !self.label_to_id.contains_key(b) {
            self.add_node(b);
        }

        self.graph.add_edge(
            self.label_to_id[a],
            self.label_to_id[b],
            (),
        );
    }

    pub fn is_adjacent(&self, a: &str, b: &str) -> bool {
        let a = self.label_to_id[a];
        let b = self.label_to_id[b];

        self.graph.is_adjacent(&self.graph.adjacency_matrix(), a, b)
    }

    pub fn node_count(&self) -> usize {
        self.graph.node_count()
    }
}

fn read_json_from_file(filename: &str) -> Result<serde_json::Value> {
    let mut s = String::new();
    File::open(&filename)?.read_to_string(&mut s)?;

    Ok(serde_json::from_str(&s)?)
}

pub fn read_graph(filename: &str) -> Result<Graph> {
    let mut graph = Graph::new();

    let json = read_json_from_file(filename)?;
    json["graph"]["nodes"]
        .as_array()
        .unwrap()
        .into_iter()
        .for_each(|node| { graph.add_node(node["id"].as_str().unwrap()); });

    json["graph"]["edges"]
        .as_array()
        .unwrap()
        .into_iter()
        .for_each(|edge| {
            graph.add_edge(
                edge["source"].as_str().unwrap(),
                &edge["target"].as_str().unwrap(),
            );
        });

    Ok(graph)
}

pub fn find_cycle(graph: &Graph) -> Option<Vec<String>> {
    // TODO: make sure all infoarena tests pass.
    let source = graph.labels.first().unwrap();

    let mut dp: HashMap<FixedBitSet, Vec<Option<String>>> = HashMap::new();
    let mut processed: HashSet<FixedBitSet> = HashSet::new();
    let mut queue: VecDeque<FixedBitSet> = VecDeque::new();

    let initial_state = FixedBitSet::with_capacity(graph.node_count());
    dp.insert(initial_state.clone(), vec![None; graph.node_count()]);
    processed.insert(initial_state.clone());
    queue.push_back(initial_state.clone());

    while !queue.is_empty() {
        let state = queue.pop_front().unwrap();
        dp.insert(state.clone(), vec![None; graph.node_count()]);

        // Add future states to the queue
        (0..graph.node_count())
            .filter(|x| !state.contains(*x))
            .map(|x| {
                let mut new_state = state.clone();
                new_state.set(x, true);
                new_state
            })
            .for_each(|state| if !processed.contains(&state) {
                queue.push_back(state.clone());
                processed.insert(state);
            });

        let nodes: Vec<usize> = state.ones().collect();
        let answers: Vec<Option<String>> = nodes
            .iter()
            .map(|&v| {
                let mut prev_state = state.clone();
                prev_state.set(v, false);

                if &prev_state == &initial_state {
                    return Some(graph.labels[v].clone());
                }

                match prev_state
                    .ones()
                    .filter(|&u| {
                        graph.is_adjacent(&graph.labels[u], &graph.labels[v]) &&
                            dp.get(&prev_state).unwrap()[u].is_some()
                    })
                    .next() {
                    Some(x) => Some(graph.labels[x].clone()),
                    None => None,
                }
            })
            .collect();

        for (&i, answer) in nodes.iter().zip(answers) {
            dp.get_mut(&state).unwrap()[i] = answer;
        }

        //        println!("dp contains the following states:");
        //        for state in dp.keys() {
        //            println!("\tstate = {:?}; possible = {:?}", state, dp.get(&state).unwrap());
        //        }
        //        println!();
    }

    let mut final_state = FixedBitSet::with_capacity(graph.node_count());
    final_state.set_range(.., true);

    let cycle_exists = graph.labels.iter().any(|v| {
        graph.is_adjacent(&v, &source) &&
            dp.get(&final_state).unwrap()[*graph.label_to_id.get(v).unwrap()].is_some()
    });

    if cycle_exists {
        let mut cycle: Vec<String> = Vec::new();
        let mut node = source.clone();
        let mut current_state = final_state.clone();

        for _ in 0..graph.node_count() {
            cycle.push(node.clone());
            let &id = graph.label_to_id.get(&node).unwrap();
            let new_node = dp.get(&current_state)
                .unwrap()
                .get(id)
                .unwrap()
                .clone()
                .unwrap();

            current_state.set(id, false);
            node = new_node;
        }

        let first = cycle[0].clone();
        cycle.push(first);
        cycle.reverse();
        return Some(cycle);
    }

    None
}
