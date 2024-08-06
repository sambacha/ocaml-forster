open Forester_prelude
open Forester_core

module T = Xml_tree2
module P = Pure_html
module H = P.HTML

exception Todo

module type S = sig
  val route : addr -> string
  val render_article : T.content T.article -> P.node
end

module Make (F : Forest2.S) : S = struct

  module PT = Plain_text_client.Make (F)
  module C = T.Comparators (PT)

  let get_sorted_articles addrs =
    addrs
    |> Addr_set.to_seq
    |> Seq.filter_map F.get_article
    |> List.of_seq
    |> List.sort C.compare_article

  let route addr =
    Format.asprintf "%a.html" pp_addr addr

  let render_xml_qname =
    function
    | T.{prefix = ""; uname; _} -> uname
    | T.{prefix; uname; _} -> Format.sprintf "%s:%s" prefix uname

  let render_xml_attr T.{key; value} =
    P.string_attr (render_xml_qname key) "%s" value

  let tag_of_prim_node : Prim.t -> P.attr list -> P.node list -> P.node =
    function
    | `P -> H.p
    | `Em -> H.em
    | `Strong -> H.strong
    | `Figure -> H.figure
    | `Figcaption -> H.figcaption
    | `Ul -> H.ul
    | `Ol -> H.ol
    | `Li -> H.li
    | `Blockquote -> H.blockquote
    | `Code -> H.code
    | `Pre -> H.pre

  let render_prim_node p =
    tag_of_prim_node p []

  let render_img =
    function
    | T.Inline {format; base64} ->
      H.img [H.src "data:image/%s;base64,%s" format base64]
    | T.Remote url ->
      H.img [H.src "%s" url]

  let rec render_article (article : T.content T.article) : P.node =
    H.html [] [
      H.head [] [
        H.meta [H.charset "utf-8"];
        H.link [H.rel "stylesheet"; H.href "style.css"]
      ];
      H.body [] [
        H.article [] [
          render_frontmatter article.frontmatter;
          H.null @@ render_content article.mainmatter;
          H.section [H.class_ "backmatter"] @@ render_content article.backmatter
        ]
      ]
    ]

  and render_section (section : T.content T.section) : P.node =
    H.section [] [
      render_frontmatter section.frontmatter;
      H.null @@ render_content section.mainmatter
    ]

  and render_frontmatter (frontmatter : T.content T.frontmatter) : P.node =
    H.header [] [
      H.h1 [] @@ render_content frontmatter.title
    ]

  and render_content (content : T.content) : P.node list =
    List.concat_map render_content_node content

  and render_content_node : T.content_node -> P.node list =
    function
    | Text str ->
      [P.txt "%s" str]
    | CDATA str ->
      [P.txt ~raw:true "<![CDATA[%s]]>" str]
    | Xml_elt elt ->
      [P.std_tag
         (render_xml_qname elt.name)
         (List.map render_xml_attr elt.attrs)
         (render_content elt.content)]
    | Prim (p, content) ->
      [render_prim_node p @@ render_content content]
    | Transclude transclusion ->
      render_transclusion transclusion
    | Link link ->
      render_link link
    | Results_of_query q ->
      F.run_query q
      |> get_sorted_articles
      |> List.map (Fun.compose render_section T.article_to_section)
    | Section section ->
      [render_section section]
    | KaTeX (mode, content) ->
      let l, r =
        match mode with
        | Display -> {|\[|}, {|\]|}
        | Inline -> {|\(|}, {|\)|}
      in
      let body = PT.string_of_content content in
      [P.txt ~raw:true "%s%s%s" l body r]
    | TeX_cs cs ->
      [P.txt ~raw:true "\\%s" (TeX_cs.show cs)]
    | Img img ->
      [render_img img]


  and render_transclusion (transclusion : T.transclusion) : P.node list =
    render_content @@ F.get_content_of_transclusion transclusion

  (* TODO: links need to be flattened in order to produce valid HTML. *)
  and render_link (link : T.content T.link) : P.node list =
    let article_opt = F.get_article @@ User_addr link.href in
    let attrs =
      match article_opt with
      | None ->
        [H.href "%s" link.href]
      | Some article ->
        [H.href "%s" @@ route article.frontmatter.addr;
         H.title_ "%s" @@ PT.string_of_content article.frontmatter.title]
    in
    [ H.a attrs @@ render_content link.content ]

end
