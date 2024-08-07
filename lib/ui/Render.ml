open Forester_core
open Xml_tree2
open Lwd
open Nottui
open Notty
module Widget = Nottui_widgets

let render_xml_elt (elt : 'content xml_elt) =
  match elt with { name; attrs; content } -> Lwd.return Ui.empty

let render_transclusion (trans : transclusion) =
  match trans with { addr; target } -> Lwd.return Ui.empty

let render_section (section : 'content section) =
  match section with { frontmatter; mainmatter } -> Lwd.return Ui.empty

let render_tex ((mode, content) : math_mode * content) =
  match mode with Inline | Display -> Lwd.return Ui.empty

let render_section (section : 'content section) =
  match section with { frontmatter; mainmatter } -> Lwd.return Ui.empty

let render_cs (cs : TeX_cs.t) =
  match cs with TeX_cs.Word _ | TeX_cs.Symbol _ -> Lwd.return Ui.empty

let render_img (img : img) =
  match img with Inline _ | Remote _ -> Lwd.return Ui.empty

let render_link (link : 'content link) =
  match link with { href; content } -> Lwd.return Ui.empty

let rec render_node ?attr (n : content_node) =
  match n with
  | Text str -> Widget.string ?attr str |> Lwd.return
  | CDATA str -> Widget.string str |> Lwd.return
  | Xml_elt elt -> render_xml_elt elt
  | Transclude transclusion -> render_transclusion transclusion
  | Results_of_query _ -> Lwd.return Ui.empty
  | Section section -> render_section section
  | Prim (prim, content) -> render_prim (prim, content)
  | KaTeX (mode, content) -> render_tex (mode, content)
  | TeX_cs cs -> render_cs cs
  | Link link -> render_link link
  | Img img -> render_img img

and render_prim ((prim, content) : Prim.t * content) =
  let attr =
    match prim with
    | `Figure -> A.empty
    | `Em -> A.italic |> A.st
    | `Figcaption -> A.empty
    | `Strong -> A.bold |> A.st
    | `Ul -> A.empty
    | `Li -> A.empty
    | `Blockquote -> A.empty
    | `Code -> A.empty
    | `Ol -> A.empty
    | `Pre -> A.empty
    | `P -> A.empty
  in
  Widget.hbox (List.map (fun n -> render_node ~attr n) content)

let render_addr addr =
  let attr =
    let open A in
    bg green ++ fg black
  in
  let content =
    match addr with User_addr str -> str | Machine_addr i -> Int.to_string i
  in
  I.strf ~attr " %s " content

let render_frontmatter = function
  | {
      addr;
      title;
      dates;
      attributions;
      taxon;
      number;
      physical_parent;
      designated_parent;
      source_path;
      tags;
      metas;
    } ->
      Widget.hbox [ render_addr addr |> Ui.atom |> Lwd.return ]

let render_backmatter sections =
  Widget.hbox @@ List.map (fun section -> render_node section) sections

let render_mainmatter mainmatter = Widget.hbox (List.map render_node mainmatter)

let render_article = function
  | { frontmatter; mainmatter; backmatter } ->
      Widget.vbox
        [
          render_frontmatter frontmatter;
          render_mainmatter mainmatter;
          render_backmatter backmatter;
        ]
