use crate::hir_index::FunctionBody;

fn escape_dot_label(str: &str) -> String {
    format!("\"{}\"", str.replace('\"', "\"\""))
}

pub fn create_cfg_dot_visualization(func_body: &FunctionBody) -> String {
    let mut graph = "".to_string();
    graph.push_str(format!("// Control flow graph for {}\n", func_body.name.0).as_str());
    graph.push_str("// In VSCode, I recommend installing the `Graphviz Interactive Preview` extension to view this file.\n");
    graph.push_str("strict digraph D {\n");
    graph.push_str("  node [shape=box];\n");
    for bb in &func_body.basic_blocks {
        for i in &bb.instructions {
            if i.is_branching() && bb.label.is_some() {
                for target in &i.basic_block_refs {
                    let from_lbl = escape_dot_label(&bb.label.as_ref().unwrap().0);
                    let to_lbl = escape_dot_label(&target.0);
                    graph.push_str(format!("  {} -> {}\n", from_lbl, to_lbl).as_str());
                }
            }
        }
    }
    graph.push_str("}\n");
    graph.to_string()
}
