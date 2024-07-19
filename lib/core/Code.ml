open Forester_prelude
open Base

type node =
  | Text of string
  | Verbatim of string
  | Group of delim * t
  | Math of math_mode * t
  | Ident of Trie.path * string list
  | Xml_tag of (string option * string) * ((string option * string) * t) list * t
  | Subtree of string option * t
  | Let of Trie.path * Trie.path list * t
  | Open of Trie.path
  | Scope of t
  | Put of Trie.path * t
  | Default of Trie.path * t
  | Get of Trie.path

  | Fun of Trie.path list * t

  | Object of {self : Trie.path option; methods : (string * t) list}
  | Patch of {obj : t; self : Trie.path option; methods: (string * t) list}
  | Call of t * string

  | Import of visibility * string
  | Def of Trie.path * Trie.path list * t
  | Decl_xmlns of string * string
  | Alloc of Trie.path

  | Namespace of Trie.path * t
[@@deriving show]

and t = node Range.located list
[@@deriving show]

and tree =
  {source_path : string option;
   addr : string option;
   code : t}
[@@deriving show]

let import_private x = Import (Private, x)
let import_public x = Import (Public, x)

let inline_math e = Math (Inline, e)
let display_math e = Math (Display, e)
let parens e = Group (Parens, e)
let squares e = Group (Squares, e)
let braces e = Group (Braces, e)
