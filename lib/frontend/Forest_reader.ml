open Forester_core

module T = Xml_tree2

type 'a env = 'a constraint 'a = <
    cwd : Eio.Fs.dir_ty Eio.Path.t;
    process_mgr : _ Eio.Process.mgr;
    stdout : _ Eio.Flow.sink;
    ..
  > as 'a

exception Todo

let read_trees ~(env : _ env) (trees : Code.tree list) : T.content T.article Addr_map.t =
  let module I = struct
    let latex_to_svg _ = "todo"
  end in

  let module Ev = Eval2.Make (I) in

  let add_tree addr tree trees =
    if Addr_map.mem addr trees then
      begin
        Reporter.emitf Duplicate_tree "skipping duplicate tree at address `%a`" pp_addr addr;
        trees
      end
    else
      Addr_map.add addr tree trees
  in

  let unexpanded_trees =
    let alg acc (tree : Code.tree) =
      match tree.addr with
      | Some addr -> add_tree (User_addr addr) tree acc
      | None -> acc
    in
    List.fold_left alg Addr_map.empty trees
  in

  let (_, trees) =
    let import_graph = Import_graph.build_import_graph trees in
    let task addr (units, trees) =
      let tree = Addr_map.find_opt addr unexpanded_trees in
      match tree with
      | None -> units, trees
      | Some tree ->
        let units, syn = Expand.expand_tree units tree in
        let tree, emitted_trees = Ev.eval_tree ~addr ~source_path:tree.source_path syn in
        let add trees (emitted : _ T.article)  =
          add_tree emitted.frontmatter.addr emitted trees
        in
        units, List.fold_left add trees @@ tree :: emitted_trees
    in
    Import_graph.Topo.fold task import_graph (Expand.UnitMap.empty, Addr_map.empty)
  in

  trees