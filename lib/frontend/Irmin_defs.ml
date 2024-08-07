open Forester_core

(* https://github.com/mirage/irmin/issues/909 *)
(* https://github.com/ocaml/dune/blob/a3a5bfcecae994509b2cb5e283e9569956244785/doc/dev/cache.md *)
type ('a, 'b) content = Artifact of 'a | Value of 'b

type forest_content =
  (Xml_tree2.content Xml_tree2.article, Code.tree) content

module Content : Irmin.Contents.S with type t = forest_content = struct
  type t = forest_content

  let t : t Repr.ty =
    let open Repr in
    variant "forest_content" (fun artifact value -> function
      | Artifact x -> artifact x | Value x -> value x)
    |~ case1 "Artifact" (Xml_tree2.article_t Xml_tree2.content_t) (fun x -> Artifact x)
    |~ case1 "Value" Reps.tree (fun x -> Value x)
    |> sealv

  let merge ~old:_ _a b = Irmin.Merge.ok b
  let merge = Irmin.Merge.(option (v t merge))
end


module Article : Irmin.Contents.S with type t = Xml_tree2.content Xml_tree2.article = struct
  type t = Xml_tree2.content Xml_tree2.article

  let t : t Repr.ty = Xml_tree2.article_t Xml_tree2.content_t
    (* let open Repr in *)
    (* variant "forest_content" (fun artifact value -> function *)
    (*   | Artifact x -> artifact x | Value x -> value x) *)
    (* |~ case1 "Artifact" (Xml_tree2.article_t Xml_tree2.content_t) (fun x -> Artifact x) *)
    (* |~ case1 "Value" Reps.tree (fun x -> Value x) *)
    (* |> sealv *)

  let merge ~old:_ _a b = Irmin.Merge.ok b
  let merge = Irmin.Merge.(option (v t merge))
end

(* To use article_t for remote forestry, apply Irmin_git_unix instead, as
   Irmin_fs_unix does not support syncing *)

module Store = Irmin_fs_unix.KV.Make (Content)
module Remote = Irmin_git_unix.FS.KV (Article)
module Store_info = Irmin_unix.Info (Store.Info)

let info message = Store_info.v "%s" message
