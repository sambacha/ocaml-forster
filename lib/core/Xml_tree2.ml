open Forester_prelude
open Base

type xml_qname = {
  prefix : string;
  (** The prefix to a qualified XML name; this prefix is expected to be rendered in the scope of a corresponding [xmlns] binding. *)

  uname : string;
  (** The unqualified part of the XML name. *)

  xmlns : string option
  (** The XML namespace bound by the current scope to [prefix]. This is not used when serialising to XML, but can be helpful for other analyses. *)
}
[@@deriving show]

type xml_attr = {key : xml_qname; value : string}
[@@deriving show]

type 'content xml_elt = {
  name : xml_qname;
  attrs : xml_attr list;
  content : 'content
}
[@@deriving show]

type attribution =
  | Author of string
  | Contributor of string
[@@deriving show]

type 'content frontmatter = {
  addr : addr;
  title : 'content;
  dates : Date.t list;
  attributions : attribution list;
  taxon : string option;
  number : string option;
  physical_parent : addr option;
  designated_parent : addr option;
  source_path : string option;
  tags : string list;
  metas : (string * 'content) list
}
[@@deriving show]

type 'content section = {
  frontmatter : 'content frontmatter;
  mainmatter : 'content
}
[@@deriving show]

type 'content article = {
  frontmatter: 'content frontmatter;
  mainmatter: 'content;
  backmatter: 'content;
}
[@@deriving show]

type content_target =
  | Full
  | Mainmatter
  | Title
  | Taxon
  | Number
  (** TODO: when we support automatic subtree numbering *)
[@@deriving show]

type transclusion = {
  addr : addr;
  target : content_target
}
[@@deriving show]

type 'content link = {
  href : string;
  content : 'content
}
[@@deriving show]

type inline_img = {
  format : string;
  base64 : string
}
[@@deriving show]

type img =
  | Inline of inline_img
  | Remote of string
[@@deriving show]

type content_node =
  | Text of string
  | CDATA of string
  | Xml_elt of content xml_elt
  | Transclude of transclusion
  | Results_of_query of Query.dbix Query.expr
  | Section of content section
  | Prim of Prim.t * content
  | KaTeX of math_mode * content
  | TeX_cs of TeX_cs.t
  | Link of content link
  | Img of img
[@@deriving show]

and content = content_node list
[@@deriving show]

let is_whitespace node =
  match node with
  | Text txt -> String.trim txt = ""
  | _ -> false

let strip_whitespace =
  List.filter @@ fun x ->
  not @@ is_whitespace x

let trim_whitespace xs =
  let rec trim_front xs =
    match xs with
    | x :: xs when is_whitespace x ->
      trim_front xs
    | xs -> xs
  and trim_back xs =
    List.rev @@ trim_front @@ List.rev xs
  in
  trim_back @@ trim_front xs


let empty_frontmatter ~addr = {
  addr;
  source_path = None;
  physical_parent = None;
  designated_parent = None;
  dates = [];
  attributions = [];
  taxon = None;
  number = None;
  metas = [];
  tags = [];
  title = [Text "Untitled"]
}

let default_backmatter ~addr : content =
  let a = Query.Addr addr in
  let make_section title query =
    let query = Query.distill_expr query in
    let section =
      let tmp_addr = Machine_addr (Oo.id object end) in
      let frontmatter = {(empty_frontmatter ~addr:tmp_addr) with title = [Text title]} in
      let mainmatter = [Results_of_query query] in
      {frontmatter; mainmatter}
    in
    Section section
  in
  [make_section "references" @@ Query.references a;
   make_section "context" @@ Query.context a;
   make_section "backlinks" @@ Query.backlinks a;
   make_section "related" @@ Query.related a;
   make_section "contributions" @@ Query.contributions a]

let article_to_section : 'a article -> 'a section =
  fun {frontmatter; mainmatter; _} ->
  {frontmatter; mainmatter}

module Comparators (I : sig val string_of_content : content -> string end) =
struct
  let compare_content =
    Compare.under I.string_of_content String.compare

  let compare_frontmatter =
    let latest_date (fm : content frontmatter) =
      let sorted_dates = fm.dates |> List.sort @@ Compare.invert Date.compare in
      List.nth_opt sorted_dates 0
    in
    let by_date =
      Fun.flip @@
      Compare.under latest_date @@
      Compare.option Date.compare
    in
    let by_title =
      compare_content |> Compare.under @@ fun fm ->
      fm.title
    in
    Compare.cascade by_date by_title

  let compare_article =
    compare_frontmatter |> Compare.under @@ fun x ->
    x.frontmatter
end


module TeX_like : sig
  val pp_content : Format.formatter -> content -> unit
end =
struct
  let pp_tex_cs fmt =
    function
    | TeX_cs.Symbol x -> Format.fprintf fmt "\\%c" x
    | TeX_cs.Word x -> Format.fprintf fmt "\\%s " x

  let rec pp_content fmt =
    Format.pp_print_list pp_content_node fmt

  and pp_content_node fmt =
    function
    | Text str -> Format.fprintf fmt "%s" str
    | CDATA str -> Format.fprintf fmt "%s" str
    | KaTeX (_, xs) -> pp_content fmt xs
    | TeX_cs cs -> pp_tex_cs fmt cs
    | Xml_elt _ | Transclude _ | Results_of_query _ | Section _ | Prim _ | Link _ | Img _ ->
      Reporter.fatalf Type_error "Cannot render this kind of content as TeX-like string"

end