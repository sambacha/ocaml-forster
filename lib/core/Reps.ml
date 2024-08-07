open Repr
open Base

let string_source : Range.string_source t =
  let open Range in
  record "string_source" (fun title content -> { title; content })
  |+ field "title" (option string) (fun s -> s.title)
  |+ field "content" string (fun s -> s.content)
  |> sealr

let source_repr : Range.source ty =
  let open Range in
  variant "source" (fun file string -> function
    | `File s -> file s | `String s -> string s)
  |~ case1 "File" string (fun s -> `File s)
  |~ case1 "String" string_source (fun s -> `String s)
  |> sealv

let position : Range.position ty =
  let open Range in
  record "position" (fun source offset start_of_line line_num ->
      { source; offset; start_of_line; line_num })
  |+ field "source" source_repr (fun t -> t.source)
  |+ field "offset" int (fun t -> t.offset)
  |+ field "start_of_line" int (fun t -> t.offset)
  |+ field "line_num" int (fun t -> t.offset)
  |> sealr

let range : Range.t ty =
  (* NOTE:
     For the irmin-git backend, the functions we need are pp, of_string and
     equal. I've worked around the need for a full of_string implementation
     (parser), since in `located_sem_node` I am simply returning `None` for the
     value of `loc`. This means even though we can serialize ranges, we can't
     retrieve them. This is fine for now, since we don't need that info for
     rendering, which is our primary use case.
  *)
  let open Range in
  let pp = Range.dump in
  let pos =
    { source = `File "todo"; offset = 0; start_of_line = 0; line_num = 0 }
  in

  let of_string str =
    (* HACK: Should parse this kind of string (produced by Range.dump):

       Range
       ({source=(`File "todo"); offset=0; start_of_line=0; line_num=0},
        {source=(`File "todo"); offset=0; start_of_line=0; line_num=0})
    *)
    Ok (Range.make (pos, pos))
  in

  let r = Range.make (pos, pos) in
  let encode encoder range = () in
  let decode _ = Ok r in
  let encode_bin : _ encode_bin = fun _ _ -> () in
  let decode_bin _ _ = r in
  let size_of : _ size_of =
    (* NOTE: Named args of_value and of_encoding are optional.
       Precompute the size that will be used by `encode_bin`. `of_encoding`
       unused nowadays
    *)
    Size.custom_dynamic ()
  in

  let compare_pos p q =
    p.source = q.source && p.offset = q.offset
    && p.start_of_line = q.start_of_line
    && p.line_num = q.line_num
  in

  let equal r1 r2 =
    match (Range.view r1, Range.view r2) with
    | `End_of_file p, `End_of_file q -> compare_pos p q
    | `Range (p1, p2), `Range (q1, q2) -> compare_pos p1 q1 && compare_pos p2 q2
    | _ -> false
  in
  let compare r1 r2 =
    if equal r1 r2 then 0
    else
      (*  FIXME: Is this used by the git-backend? If not, remove it.
      *)
      match (Range.view r1, Range.view r2) with
      | `End_of_file p, `End_of_file q ->
          if p.source = q.source then
            match (p.source, q.source) with
            | `String s1, `String s2 -> String.compare s1.content s2.content
            | `File s1, `File s2 -> String.compare s1 s2
            | _ -> -1
          else -1
      | `Range (p1, p2), `Range (q1, q2) -> -1
      | _ -> -1
  in
  let short_hash ?seed a = 0 in
  let pre_hash _ _ = () in
  abstract ~pp ~of_string ~json:(encode, decode)
    ~bin:(encode_bin, decode_bin, size_of)
    ~equal ~compare ~short_hash ~pre_hash ()

let binding a : 'a binding ty = pair binding_strategy_t a

let _object (t : Code.t ty) : Code._object ty =
  let open Code in
  record "object" (fun self methods : _object -> { self; methods })
  |+ field "self" (option (list string)) (fun (s : _object) -> s.self)
  |+ field "methods" (list (pair string t)) (fun (s : _object) -> s.methods)
  |> sealr

let patch (t : Code.t ty) : Code.patch ty =
  let open Code in
  record "patch" (fun obj self methods : patch -> { obj; self; methods })
  |+ field "obj" t (fun s -> s.obj)
  |+ field "self" (option (list string)) (fun s -> s.self)
  |+ field "methods" (list (pair string t)) (fun s -> s.methods)
  |> sealr

let node_t (t : Code.t ty) : Code.node ty =
  let open Code in
  variant "node"
    (fun
      text
      verbatim
      group
      math
      ident
      xml_tag
      subtree
      _let
      _open
      scope
      put
      default
      get
      _fun
      _object
      patch
      call
      import
      def
      decl_xmlns
      alloc
      namespace
    -> function
    | Text s -> text s
    | Verbatim s -> verbatim s
    | Group (x, y) -> group (x, y)
    | Math (x, y) -> math (x, y)
    | Ident (x, y) -> ident (x, y)
    | Xml_tag (x, y, z) -> xml_tag (x, y, z)
    | Subtree (x, y) -> subtree (x, y)
    | Let (x, y, z) -> _let (x, y, z)
    | Open x -> _open x
    | Scope x -> scope x
    | Put (x, y) -> put (x, y)
    | Default (x, y) -> default (x, y)
    | Get x -> get x
    | Fun (x, y) -> _fun (x, y)
    | Object x -> _object x
    | Patch x -> patch x
    | Call (x, y) -> call (x, y)
    | Import (x, y) -> import (x, y)
    | Def (x, y, z) -> def (x, y, z)
    | Decl_xmlns (x, y) -> decl_xmlns (x, y)
    | Alloc x -> alloc x
    | Namespace (x, y) -> namespace (x, y))
  |~ case1 "Text" string (fun x -> Text x)
  |~ case1 "Verbatim" string (fun x -> Verbatim x)
  |~ case1 "Group" (pair delim_t t) (fun (x, y) -> Group (x, y))
  |~ case1 "Math" (pair math_mode_t t) (fun (x, y) -> Math (x, y))
  |~ case1 "Ident"
       (pair (list string) (list string))
       (fun (x, y) -> Ident (x, y))
  |~ case1 "Xml_tag"
       (triple
          (pair (option string) string)
          (list (pair (pair (option string) string) t))
          t)
       (fun (x, y, z) -> Xml_tag (x, y, z))
  |~ case1 "Subtree" (pair (option string) t) (fun (x, y) -> Subtree (x, y))
  |~ case1 "Let"
       (triple (list string) (list (binding (list string))) t)
       (fun (x, y, z) -> Let (x, y, z))
  |~ case1 "Open" (list string) (fun x -> Open x)
  |~ case1 "Scope" t (fun x -> Scope x)
  |~ case1 "Put" (pair (list string) t) (fun (x, y) -> Put (x, y))
  |~ case1 "Default" (pair (list string) t) (fun (x, y) -> Default (x, y))
  |~ case1 "Get" (list string) (fun x -> Get x)
  |~ case1 "Fun"
       (pair (list (binding (list string))) t)
       (fun (x, y) -> Fun (x, y))
  |~ case1 "Object" (_object t) (fun x -> Object x)
  |~ case1 "Patch" (patch t) (fun x -> Patch x)
  |~ case1 "Call" (pair t string) (fun (x, y) -> Call (x, y))
  |~ case1 "Import" (pair visibility_t string) (fun (x, y) -> Import (x, y))
  |~ case1 "Def"
       (triple (list string) (list (binding (list string))) t)
       (fun (x, y, z) -> Def (x, y, z))
  |~ case1 "Decl_xmlns" (pair string string) (fun (x, y) -> Decl_xmlns (x, y))
  |~ case1 "Alloc" (list string) (fun x -> Alloc x)
  |~ case1 "Namespace" (pair (list string) t) (fun (x, y) -> Namespace (x, y))
  |> sealv

let located_code_node (t : Code.t ty) : Code.node Range.located ty =
  let open Asai in
  let open Range in
  record "located_code_node" (fun loc value -> { loc; value })
  |+ field "loc" (option range) (fun t -> None)
  |+ field "value" (node_t t) (fun t -> t.value)
  |> sealr

let t : Code.t ty = mu (fun t -> list (located_code_node t))

let tree : Code.tree ty =
  let open Code in
  record "tree" (fun source_path addr code -> { source_path; addr; code })
  |+ field "source_path" (option string) (fun x -> x.source_path)
  |+ field "addr" (option string) (fun x -> x.addr)
  |+ field "code" t (fun x -> x.code)
  |> sealr
