open Forester_core
open Eio
module Store = Irmin_defs
open Store

type status = Changed | Unchanged

val compare_status : status -> status -> Ppx_deriving_runtime.int

module Path_sequencer : sig
  module S : sig
    val yield : Fs.dir_ty Path.t -> unit
    val run : (unit -> unit) -> Fs.dir_ty Path.t Seq.t

    val register_printer :
      ([ `Yield of Fs.dir_ty Path.t ] -> string option) -> unit
  end

  val yield : Fs.dir_ty Path.t -> unit
  val run : (unit -> unit) -> Fs.dir_ty Path.t Seq.t

  val register_printer :
    ([ `Yield of Fs.dir_ty Path.t ] -> string option) -> unit

  val process_file : Fs.dir_ty Path.t -> unit
  val process_dir : Fs.dir_ty Path.t -> unit
  val scan_directories : Fs.dir_ty Path.t list -> Fs.dir_ty Path.t Seq.t
end

val process_file : Fs.dir_ty Path.t -> unit
val process_dir : Fs.dir_ty Path.t -> unit
val scan_directories : Fs.dir_ty Path.t list -> Fs.dir_ty Path.t Seq.t
val addr_of_fs_path : Fs.dir_ty Path.t -> addr
val store_path_of_content : forest_content -> Store.Path.t
val user_addr : string -> addr
val pp_store_path : Format.formatter -> string list -> unit
val pp_path : [> Eio__.Fs.dir_ty ] Eio.Path.t Fmt.t
val has_tree_changed : Code.tree -> Store.t -> status
val compare_addr : 'a -> 'a -> int
val partition_by_status : (status * 'a) list -> 'a list * 'a list
val get_status : Fs.dir_ty Path.t -> Store.t -> status * Code.tree
val has_file_changed : Fs.dir_ty Path.t -> Store.t -> status
val set_value : Code.tree -> Store.t -> (unit, Store.write_error) result
val get_value : addr -> Store.t -> Code.tree
val get_value_opt : addr -> Store.t -> Code.tree option

val update_value :
  Fs.dir_ty Path.t -> Store.t -> (unit, Store.write_error) result

val update_values :
  Store.t -> Fs.dir_ty Path.t list -> (unit, Store.write_error list) result

val get_artifact : addr -> Store.t -> Xml_tree2.content Xml_tree2.article

val get_artifacts :
  Irmin_defs.Store.t -> Xml_tree2.content Xml_tree2.article Addr_map.t

val get_artifact_opt :
  addr -> Store.t -> Xml_tree2.content Xml_tree2.article option

val set_artifact :
  Xml_tree2.content Xml_tree2.article ->
  Store.t ->
  (unit, Store.write_error) result

val iter : Store.t -> unit
val status : Fs.dir_ty Path.t list -> Store.t -> (status * Code.tree) list
val changed_trees : Code.tree list -> Store.t -> Code.tree list
val read_changed_trees : Fs.dir_ty Path.t list -> Store.t -> Code.tree list

val trees_to_reevaluate :
  Fs.dir_ty Path.t list -> Store.t -> Code.tree Addr_map.t

val trees_to_rerender : Fs.dir_ty Path.t list -> Addr_set.t

val set_artifact_map :
  Xml_tree2.content Xml_tree2.article Addr_map.t -> Irmin_defs.Store.t -> unit

val codes : Irmin_defs.Store.t -> Fs.dir_ty Path.t list -> Code.tree list
