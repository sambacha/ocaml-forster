open Base
open Forester_prelude

type node =
  | Text of string
  | Verbatim of string
  | Group of delim * t
  | Math of math_mode * t
  | Link of {dest : t; title : t option}
  | Subtree of string option * tree
  | Query of t Query.t
  | Embed_tex
  | Lam of Symbol.t list * t
  | Var of Symbol.t
  | Put of Symbol.t * t * t
  | Default of Symbol.t * t * t
  | Get of Symbol.t
  | If_tex of t * t
  | Xml_tag of xml_resolved_qname * (xml_resolved_qname * t) list * t
  | TeX_cs of TeX_cs.t
  | Prim of Prim.t
  | Object of {self : Symbol.t; methods: (string * t) list}
  | Patch of {obj : t; self : Symbol.t; super : Symbol.t; methods : (string * t) list}
  | Call of t * string

  | Ref
  | Transclude

  | Set_title
  | Set_parent
  | Set_taxon
  | Add_tag
  | Add_author
  | Add_contributor
  | Set_meta
  | Add_date
  | Set_number

[@@deriving show]

and t = node Range.located list
[@@deriving show]

and tree = t

