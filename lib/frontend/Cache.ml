open Forester_core

module Store = Irmin_defs
open Store

module S = Algaeff.Sequencer.Make (struct
  type t = Eio.Fs.dir_ty Eio.Path.t
end)

type status = Changed | Unchanged [@@deriving ord]

module Path_sequencer = struct
  module S = Algaeff.Sequencer.Make (struct
    type t = Eio.Fs.dir_ty Eio.Path.t
  end)

  include S

  let rec process_file fp =
    if Eio.Path.is_directory fp then process_dir fp
    else
      Eio.Path.split fp
      |> Option.iter @@ fun (_, basename) ->
         if
           Filename.extension basename = ".tree"
           && (not @@ String.starts_with ~prefix:"." basename)
         then yield fp

  and process_dir dir =
    try
      Eio.Path.read_dir dir
      |> List.iter @@ fun fp -> process_file Eio.Path.(dir / fp)
    with Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

  let scan_directories dirs =
    S.run @@ fun () -> dirs |> List.iter @@ fun fp -> process_dir fp
end

module Oper = Graph.Oper.Make (Graph.Builder.I (Import_graph.Gph))

let rec process_file fp =
  if Eio.Path.is_directory fp then process_dir fp
  else
    Eio.Path.split fp
    |> Option.iter @@ fun (_, basename) ->
       if
         Filename.extension basename = ".tree"
         && (not @@ String.starts_with ~prefix:"." basename)
       then S.yield fp

and process_dir dir =
  try
    Eio.Path.read_dir dir
    |> List.iter @@ fun fp -> process_file Eio.Path.(dir / fp)
  with Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

let scan_directories dirs =
  S.run @@ fun () -> dirs |> List.iter @@ fun fp -> process_dir fp

let addr_of_fs_path : Eio.Fs.dir_ty Eio.Path.t -> addr =
 fun path ->
  match Eio.Path.split path with
  | None ->
      Reporter.fatalf Internal_error
        "could not make addres from path %a because splitting the path failed."
        Eio.Path.pp path
  | Some (_, base) -> User_addr (Filename.chop_extension base)

let store_path_of_content : forest_content -> Store.Path.t = function
  | Value tree -> (
      match tree.addr with
      | Some addr -> [ "values"; addr ]
      | None -> Reporter.fatalf Internal_error "tree has no address")
  | Artifact article ->
      match article with
      | { frontmatter = { addr ; _ }; _ } -> [ "artifacts"; Format.asprintf "%a" pp_addr addr ]

(* let store_path_of_path p = p |> addr_of_fs_path |> store_path_of_addr *)
let user_addr addr = User_addr addr

let pp_store_path ppf segments =
  let pp_sep ppf () = Format.fprintf ppf "/@" in
  Format.fprintf ppf "%a"
    Format.(pp_print_list ~pp_sep pp_print_string)
    segments

let pp_path = Eio.Path.pp

let has_tree_changed (tree : Code.tree) cache =
  let addr =
    match tree with
    | { source_path; addr = Some str; code } -> User_addr str
    | _ -> Reporter.fatalf Internal_error "tree has no addr"
  in
  let fresh_hash = Store.Backend.Contents.Hash.hash @@ Value tree in
  let store_path = store_path_of_content (Value tree) in
  match Store.hash cache store_path with
  | Some stored_hash -> if stored_hash = fresh_hash then Unchanged else Changed
  | None -> Changed

let compare_addr = Addr.compare

type stat_addr = status * addr [@@deriving ord]

let partition_by_status l =
  let rec part left right = function
    | [] -> (List.rev left, List.rev right)
    | x :: l -> (
        match x with
        | Changed, v -> part (v :: left) right l
        | Unchanged, v -> part left (v :: right) l)
  in
  part [] [] l

let get_status path cache =
  (* let store_path = path |> addr_of_fs_path |> store_path_of_addr in *)
  let code =
    match Parse.parse_file path with
    | Error _ ->
        Reporter.fatalf Parse_error "failed to parse tree at %a" pp_path path
    | Ok code -> code
  in
  let source_path = Option.map Unix.realpath @@ Eio.Path.native path in
  let addr =
    match Eio.Path.split path with
    | None -> None
    | Some (_, base) -> Some (Filename.chop_extension base)
  in
  let tree = Code.{ source_path; addr; code } in
  (has_tree_changed tree cache, tree)

let has_file_changed path cache = get_status path cache |> fst
(*
  let fresh_hash = Cache.Backend.Contents.Hash.hash @@ Value tree in
  match Cache.hash cache store_path with
  | Some stored_hash ->
      if stored_hash = fresh_hash then Unchanged tree else Changed tree
  | None -> Changed tree
*)

let set_value tree cache =
  let v = Value tree in
  let path = store_path_of_content v in
  Store.set ~info:(info "") cache path v

let get_value (addr : addr) cache =
  let path = [ "values"; Format.asprintf "%a" pp_addr addr ] in
  Store.get cache path |> function
  | Value v -> v
  | Artifact _ ->
      Reporter.fatalf Internal_error
        "got artifact when looking up store path %a. This should never happen! \
         There is a bug in the code which stores artifacts!"
        pp_store_path path

let get_value_opt (addr : addr) cache =
  let path = [ "values"; Format.asprintf "%a" pp_addr addr ] in
  Store.find cache path |> function Some (Value v) -> Some v | _ -> None

let update_value path cache =
  match get_status path cache with
  | (Unchanged, _) -> Ok ()
  | (Changed, tree) ->
      Reporter.tracef "when updating %a" pp_store_path (store_path_of_content (Value tree))
      @@ fun () ->
      let v = Value tree in
      let path = store_path_of_content v in
      Store.set ~info:(info "foo") cache path v

let get_artifact (addr : addr) cache =
  let path = [ "artifacts"; Format.asprintf "%a" pp_addr addr ] in
  Store.get cache path |> function
  | Artifact a -> a
  | Value _ ->
      Reporter.fatalf Internal_error
        "got value when looking up store path %a. This should never happen! \
         There is a bug in the code which stores values!"
        pp_store_path path

let get_artifact_opt (addr : addr) cache =
  let path = [ "artifacts"; Format.asprintf "%a" pp_addr addr ] in
  Store.find cache path |> function Some (Artifact a) -> Some a | _ -> None

let set_artifact (tree : Xml_tree2.content Xml_tree2.article) cache =
  match tree with
  | { frontmatter = { addr; _ }; _ } ->
      let path = [ "artifacts"; Format.asprintf "%a" pp_addr addr ] in
      Store.set
        ~info:(Store_info.v "Todo: commit info for store access")
        cache path (Artifact tree)

let iter cache =
  let repo = Store.repo cache in
  let main = Store.main repo in
  Store.Repo.iter repo ~min:[] ~max:[]

(*
let write_artifact_to_file (addr : addr) fmt =
  let module Serialize =
    Forester_render.Serialise_xml_tree.Make
      (struct
        let root = None
      end)
      ()
  in
  let tree = get_artifact addr in
  Serialize.pp fmt tree
*)

(* TODO: rename to update_inputs or something *)
let update_values cache =
  let to_either = function
    | Ok a -> Either.Right a
    | Error e -> Either.Left e
  in
  fun dirs ->
    Reporter.tracef "when updating cache" @@ fun () ->
    scan_directories dirs |> List.of_seq
    |> List.map (fun p -> update_value p cache)
    |> List.partition_map to_either
    |> fun (errs, _) -> match errs with [] -> Ok () | _ :: _ -> Error errs

let status dirs cache : (status * Code.tree) list =
  scan_directories dirs |> List.of_seq |> List.map (fun path -> get_status path cache)

let changed_trees trees cache =
  trees
  |> List.filter_map (fun tree ->
         let status = has_tree_changed tree cache in
         match status with Changed -> Some tree | Unchanged -> None)

let read_changed_trees dirs cache =
  status dirs cache
  |> List.filter_map (function
       | Changed, tree -> Some tree
       | Unchanged, _ -> None)

let trees_to_reevaluate dirs cache =
  let changed_trees, unchanged_trees = status dirs cache |> partition_by_status in
  let dependency_graph =
    Import_graph.build_import_graph (changed_trees @ unchanged_trees)
    |> Oper.transitive_closure
  in
  let dependents addr =
    Import_graph.Gph.fold_succ Addr_set.add dependency_graph addr Addr_set.empty
  in
  let changed_addrs =
    List.filter_map
      (fun tree -> Option.map user_addr Code.(tree.addr))
      changed_trees
  in
  List.fold_left
    (fun trees addr -> Addr_set.union (dependents addr) trees)
    Addr_set.empty changed_addrs

(* TODO: Here I would need to look up the set of trees with queries that match
   an addr in `reval`.
*)
let trees_to_rerender dirs =
  let reval = trees_to_reevaluate dirs in
  Addr_set.empty
