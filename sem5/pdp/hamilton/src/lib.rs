extern crate petgraph;
extern crate serde_json;
extern crate fixedbitset;
extern crate rayon;

use fixedbitset::FixedBitSet;

use std::fs::File;
use std::io::{Read, Result};

use rayon::prelude::*;

use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet, VecDeque};

use petgraph::graphmap::DiGraphMap;
use petgraph::visit::GetAdjacencyMatrix;

fn hash(label: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    label.hash(&mut hasher);
    hasher.finish()
}

#[derive(Debug)]
pub struct Graph {
    pub labels: Vec<String>,
    pub graph: DiGraphMap<u64, ()>,
}

fn read_json_from_file(filename: &str) -> Result<serde_json::Value> {
    let mut s = String::new();
    File::open(&filename)?.read_to_string(&mut s)?;

    Ok(serde_json::from_str(&s)?)
}

pub fn read_graph(filename: &str) -> Result<Graph> {
    let json = read_json_from_file(filename)?;

    let mut graph: DiGraphMap<u64, ()> = DiGraphMap::new();
    let mut labels: Vec<String> = Vec::new();

    json["graph"]["nodes"]
        .as_array()
        .unwrap()
        .into_iter()
        .for_each(|node| {
            let label = node["id"].as_str().unwrap().to_string();
            graph.add_node(hash(&label));
            labels.push(label);
        });

    json["graph"]["edges"]
        .as_array()
        .unwrap()
        .into_iter()
        .for_each(|edge| {
            let a = hash(&edge["source"].as_str().unwrap().to_string());
            let b = hash(&edge["target"].as_str().unwrap().to_string());
            graph.add_edge(a, b, ());
        });

    Ok(Graph { graph, labels })
}

pub fn find_cycle(g: &Graph) {
    let graph = &g.graph;
    let labels = &g.labels;

    let start_node_label = labels[0].clone();

    let mut dp: HashMap<FixedBitSet, Vec<bool>> = HashMap::new();
    let mut processed: HashSet<FixedBitSet> = HashSet::new();
    let mut q: VecDeque<FixedBitSet> = VecDeque::new();

    // initial state
    let mut initial_state = FixedBitSet::with_capacity(graph.node_count());
    initial_state.set(0, true);
    dp.insert(initial_state.clone(), vec![false; graph.node_count()]);
    dp.get_mut(&initial_state).unwrap()[0] = true;
    processed.insert(initial_state.clone());


    q.push_back(initial_state);


    while !q.is_empty() {
        let state = q.pop_front().unwrap();

        println!("entered state {:?}", state);

        (0..graph.node_count())
            .filter(|x| !state.contains(*x))
            .map(|x| {
                let mut new_state = state.clone();
                new_state.set(x, true);
                new_state
            })
            .for_each(|state| {
                if !processed.contains(&state) {
                    q.push_back(state.clone());
                    processed.insert(state);
                }
            });

        if !dp.contains_key(&state) {
            dp.insert(state.clone(), vec![false; graph.node_count()]);
        }


        let current_dp = dp.get_mut(&state).unwrap();
        let ones: Vec<usize> = state.ones().collect();

        let computed: Vec<bool> = ones.par_iter().map(|&v| {
            let mut prev_state = state.clone();
            prev_state.set(v, false);

            let node_v = hash(&labels[v]);
            for u in prev_state.ones() {
                let node_u = hash(&labels[u]);

                if graph.is_adjacent(&graph.adjacency_matrix(), node_u, node_v) {
                    return true;
                }
            }
            return false;
        }).collect();

        for (&i, ans) in ones.iter().zip(computed) {
            current_dp[i] = ans;
        }
    }

    let mut cycle_exists = false;

    let mut final_state = FixedBitSet::with_capacity(graph.node_count());
    final_state.set_range(.., true);

    for v in labels {
        if graph.is_adjacent(&graph.adjacency_matrix(), hash(&v), hash(&start_node_label)) {
            cycle_exists = true;
            println!("found cycle!");
        }
    }

    println!("cycle: {}", cycle_exists);
}