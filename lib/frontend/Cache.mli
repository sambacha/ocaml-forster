module Store = Irmin_defs
open Store

type status = Changed | Unchanged
val compare_status : status -> status -> Ppx_deriving_runtime.int

module Path_sequencer :
  sig
    module S :
      sig
        val yield : Eio.Fs.dir_ty Eio.Path.t -> unit
        val run : (unit -> unit) -> Eio.Fs.dir_ty Eio.Path.t Seq.t
        val register_printer :
          ([ `Yield of Eio.Fs.dir_ty Eio.Path.t ] -> string option) -> unit
      end
    val yield : Eio.Fs.dir_ty Eio.Path.t -> unit
    val run : (unit -> unit) -> Eio.Fs.dir_ty Eio.Path.t Seq.t
    val register_printer :
      ([ `Yield of Eio.Fs.dir_ty Eio.Path.t ] -> string option) -> unit
    val process_file : Eio.Fs.dir_ty Eio.Path.t -> unit
    val process_dir : Eio.Fs.dir_ty Eio.Path.t -> unit
    val scan_directories :
      Eio.Fs.dir_ty Eio.Path.t list -> Eio.Fs.dir_ty Eio.Path.t Seq.t
  end

val process_file : Eio.Fs.dir_ty Eio.Path.t -> unit
val process_dir : Eio.Fs.dir_ty Eio.Path.t -> unit
val scan_directories :
  Eio.Fs.dir_ty Eio.Path.t list -> Eio.Fs.dir_ty Eio.Path.t Seq.t
val addr_of_fs_path : Eio.Fs.dir_ty Eio.Path.t -> Forester_core.addr
val store_path_of_content : forest_content -> Store.Path.t
val user_addr : string -> Forester_core.addr
val pp_store_path : Format.formatter -> string list -> unit
val pp_path : [> Eio__.Fs.dir_ty ] Eio.Path.t Fmt.t
val has_tree_changed : Forester_core.Code.tree -> Store.t -> status
val compare_addr : 'a -> 'a -> int
type stat_addr = status * Forester_core.addr
val compare_stat_addr : stat_addr -> stat_addr -> Ppx_deriving_runtime.int
val partition_by_status : (status * 'a) list -> 'a list * 'a list
val get_status :
  Eio.Fs.dir_ty Eio.Path.t -> Store.t -> status * Forester_core.Code.tree
val has_file_changed : Eio.Fs.dir_ty Eio.Path.t -> Store.t -> status
val set_value :
  Forester_core.Code.tree -> Store.t -> (unit, Store.write_error) result
val get_value : Forester_core.addr -> Store.t -> Forester_core.Code.tree
val get_value_opt :
  Forester_core.addr -> Store.t -> Forester_core.Code.tree option
val update_value :
  Eio.Fs.dir_ty Eio.Path.t -> Store.t -> (unit, Store.write_error) result
val get_artifact :
  Forester_core.addr ->
  Store.t -> Forester_core.Xml_tree2.content Forester_core.Xml_tree2.article
val get_artifact_opt :
  Forester_core.addr ->
  Store.t ->
  Forester_core.Xml_tree2.content Forester_core.Xml_tree2.article option
val set_artifact :
  Forester_core.Xml_tree2.content Forester_core.Xml_tree2.article ->
  Store.t -> (unit, Store.write_error) result
val iter : Store.t -> unit
val update_values :
  Store.t ->
  Eio.Fs.dir_ty Eio.Path.t list -> (unit, Store.write_error list) result
val status :
  Eio.Fs.dir_ty Eio.Path.t list ->
  Store.t -> (status * Forester_core.Code.tree) list
val changed_trees :
  Forester_core.Code.tree list -> Store.t -> Forester_core.Code.tree list
val read_changed_trees :
  Eio.Fs.dir_ty Eio.Path.t list -> Store.t -> Forester_core.Code.tree list
val trees_to_reevaluate :
  Eio.Fs.dir_ty Eio.Path.t list -> Store.t -> Forester_core.Addr_set.t
val trees_to_rerender :
  Eio.Fs.dir_ty Eio.Path.t list -> Forester_core.Addr_set.t

