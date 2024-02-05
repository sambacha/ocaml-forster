open Base
open Prelude

type node =
  | Text of string
  | Group of delim * t
  | Math of math_mode * t
  | Link of {dest : t; title : t option}
  | Transclude of addr
  | Subtree of tree
  | Query of t Query.t
  | Embed_tex of {preamble : t; source : t}
  | Block of t * t
  | Lam of Symbol.t list * t
  | Var of Symbol.t
  | Put of Symbol.t * t * t
  | Default of Symbol.t * t * t
  | Get of Symbol.t
  | If_tex of t * t
  | Xml_tag of string * (string * t) list * t
  | Unresolved of string
  | Prim of Prim.t * t
  | Object of {self : Symbol.t; methods: (string * t) list}
  | Patch of {obj : t; self : Symbol.t; super : Symbol.t; methods : (string * t) list}
  | Call of t * string
  | Ref of t
[@@deriving show]

and t = node Range.located list
[@@deriving show]

and frontmatter =
  {title : t option;
   addr : addr option;
   taxon : string option;
   authors : addr list;
   contributors : addr list;
   tags : addr list;
   dates : Date.t list;
   metas : (string * t) list;
   source_path : string option}
[@@deriving show]

and tree = frontmatter * t

let empty_frontmatter =
  {addr = None;
   title = None;
   taxon = None;
   dates = [];
   authors = [];
   contributors = [];
   tags = [];
   metas = [];
   source_path = None}
