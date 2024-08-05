open Forester_prelude
open Forester_core

module T = Xml_tree2

module Make (F : Forest2.S) =
struct
  let rec pp_content fmt =
    List.iter @@ pp_content_node fmt

  and pp_content_node fmt : T.content_node -> unit =
    function
    | Text txt | CDATA txt -> Format.pp_print_string fmt txt
    | KaTeX (_, content) -> pp_content fmt content
    | TeX_cs cs -> Format.fprintf fmt "\\%a" TeX_cs.pp cs
    | Xml_elt elt -> pp_content fmt elt.content
    | Transclude trn -> pp_transclusion fmt trn
    | Section section -> pp_section fmt section
    | Prim (p, content) -> pp_content fmt content
    | Link link -> pp_link fmt link
    | Results_of_query _ | Img _ -> ()

  and pp_transclusion fmt (transclusion : T.transclusion) =
    pp_content fmt @@ F.get_content_of_transclusion transclusion

  and pp_link fmt (link : T.content T.link) =
    pp_content fmt link.content

  and pp_section fmt (section : T.content T.section) =
    Format.fprintf fmt "<omitted content: %a>" pp_content section.frontmatter.title

  let string_of_content =
    Format.asprintf "%a" pp_content
end
