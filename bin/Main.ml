(* Generated by ocaml-tree-sitter. *)

open Tree_sitter_go

let () =
  Tree_sitter_run.Main.run
    ~lang:"go"
    ~parse_source_file:Parse.parse_source_file
    ~parse_input_tree:Parse.parse_input_tree
    ~dump_tree:Boilerplate.dump_tree
