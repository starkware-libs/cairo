use super::graph::FlowControlGraph;

pub fn format_graph(graph: &FlowControlGraph) -> String {
    let mut result = String::new();
    result.push_str(&format!("Root: {}\n", graph.root.0));
    for (i, node) in graph.nodes.iter().enumerate() {
        result.push_str(&format!("{i} {node:?}\n"));
    }
    result
}

// pub fn parse_graph(graph: &str) -> FlowControlGraph {
//     let mut graph = FlowControlGraph::default();
//     for line in graph.lines() {
//         let (i, node) = line.split_once(" ").unwrap();
//         let i = i.parse::<usize>().unwrap();
//         assert_eq!(i, graph.nodes.len(), "Unexpected node index. Expected {}, got {i}.",
// graph.nodes.len());         let node = node.parse::<FlowControlNode>().unwrap();
//         graph.nodes.push(node);
//     }
//     graph
// }
