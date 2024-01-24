open Prelude
open Bwd
open Core

module E = Render_effect.Perform

module Xml = Xml_printer
type printer = Xml.printer

module S = Set.Make (String)

type cfg = {base_url : string option}

let xml_id_of_addr addr =
  "tree." ^ addr

let rec render_node : cfg:cfg -> Sem.node Range.located -> printer =
  fun ~cfg located ->
  match located.value with
  | Sem.Text txt ->
    Xml.text txt

  | Sem.Link {dest; title; modifier} ->
    begin
      match E.get_doc dest with
      | Some _ ->
        render_internal_link ~cfg ~title ~modifier ~addr:dest
      | None ->
        render_external_link ~cfg ~title ~modifier ~url:dest
    end

  | Sem.Ref {addr} ->
    begin
      match E.get_doc addr with
      | None ->
        Reporter.fatalf ?loc:located.loc Tree_not_found "could not find tree at address `%s` for reference" addr
      | Some tree ->
        let uri = "#" ^ xml_id_of_addr addr in
        let attrs = [Xml.attr "target" uri] in
        Xml.tag "ref" attrs [
          begin
            match tree.taxon with
            | None -> Xml.text "§"
            | Some taxon -> Xml.text @@ String_util.sentence_case taxon
          end;
          Xml.text " ";
          Xml.text addr
        ]
    end

  | Sem.If_tex (_, x) -> render ~cfg x

  | Sem.Prim (p, body) ->
    let name =
      match p with
      | `P -> "p"
      | `Ul -> "list"
      | `Ol -> "list"
      | `Li -> "item"
      | `Em -> "emph"
      | `Strong -> "emph"
      | `Code -> "code"
      | `Blockquote -> "quote"
      | `Pre -> "pre"
    in
    let attrs =
      match p with
      | `Strong -> [Xml.attr "rend" "bold"]
      | `Ol -> [Xml.attr "rend" "numbered"]
      | `Ul -> [Xml.attr "rend" "bulletted"]
      | _ -> []
    in
    Xml.tag name attrs [render ~cfg body]

  | Sem.Math (mode, code) ->
    let module TP = Render_verbatim.Printer in
    let rend = match mode with Inline -> "inline" | Display -> "display" in
    Xml.tag "formula" [Xml.attr "notation" "tex"; Xml.attr "rend" rend] [
      Xml.text @@
      TP.contents @@
      Render_verbatim.render ~cfg:{tex = false} code
    ]

  | Sem.Xml_tag (name, attrs, xs) ->
    let attrs =
      attrs |> List.map @@ fun (k, v) ->
      let txt =
        Render_verbatim.Printer.contents @@
        Render_verbatim.render ~cfg:{tex = true} v
      in
      Xml.attr k txt
    in
    Xml.tag name attrs [render ~cfg xs]

  | Sem.Transclude (opts, addr) ->
    begin
      match E.get_doc addr with
      | None ->
        Reporter.fatalf ?loc:located.loc Tree_not_found "could not find tree at address `%s` for transclusion" addr
      | Some tree ->
        render_transclusion ~cfg ~opts tree
    end

  | Sem.Block (title, body) ->
    Xml.tag "floatingText" [] [
      Xml.tag "body" [] [
        Xml.tag "head" [] [
          Xml.tag "title" [] [
            render ~cfg title
          ]
        ];
        render ~cfg body
      ];
    ]

  | Sem.Embed_tex {packages; source} ->
    let code =
      Render_verbatim.Printer.contents @@
      Render_verbatim.render ~cfg:{tex = true} source
    in
    let hash = Digest.to_hex @@ Digest.string code in
    E.enqueue_latex ~name:hash ~packages ~source:code;
    let path = Format.sprintf "resources/%s-web.svg" hash in
    Xml.tag "figure" [] [
      Xml.tag "graphic" [Xml.attr "url" path] []
    ]

  | Sem.Query (opts, query) ->
    (* TODO: Don't run query if we're in the backmatter! *)
    let docs = E.run_query query in
    begin
      match docs with
      | [] -> Xml.nil
      | _ ->
        let body =
          docs |> List.filter_map @@ fun (doc : Sem.tree) ->
          doc.addr |> Option.map @@ fun addr ->
          let opts = Sem.{expanded = false; show_heading = true; title_override = None; taxon_override = None; toc = false; numbered = false; show_metadata = true} in
          Range.locate_opt None @@ Sem.Transclude (opts, addr)
        in
        let doc : Sem.tree =
          {addr = None;
           taxon = None;
           title = None;
           authors = [];
           contributors = [];
           dates = [];
           metas = [];
           tags = [];
           body = body;
           source_path = None}
        in
        render_transclusion ~cfg ~opts doc
    end

  | _ ->
    Reporter.fatalf ?loc:located.loc Unhandled_case "unhandled case"

and render_transclusion ~cfg ~opts (tree : Sem.tree) =
  Xml.tag "floatingText" [] [
    Xml.tag "body" [] [
      Xml.tag "head" [] [
        begin
          tree.addr |> Xml.option @@ fun addr ->
          Xml.tag "idno" [Xml.attr "type" "tree"] [Xml.text addr]
        end;
        Xml.tag "biblFull" [] [
          render_tree_metadata ~cfg tree
        ]
      ];
      render ~cfg tree.body
    ];
  ]

and render_internal_link ~cfg ~title ~modifier ~addr =
  let uri = "#" ^ xml_id_of_addr addr in
  let doc = E.get_doc addr in
  let doc_title = Option.bind doc @@ fun d -> d.title in
  let title = Option.fold title ~none:doc_title ~some:Option.some in
  let title = Option.map (Sem.apply_modifier modifier) title in
  let title = Option.value ~default:[Range.locate_opt None @@ Sem.Text addr] title in
  Xml.tag "ref"
    [Xml.attr "target" uri]
    [render ~cfg title]

and render_external_link ~cfg ~title ~modifier ~url =
  let title = Option.map (Sem.apply_modifier modifier) title in
  let title = Option.value ~default:[Range.locate_opt None @@ Sem.Text url] title in
  Xml.tag "ref"
    [Xml.attr "target" url]
    [render ~cfg title]

and render : cfg:cfg -> Sem.t -> printer =
  fun ~cfg ->
  Xml.iter (render_node ~cfg)


and render_tree : cfg:cfg -> opts:Sem.transclusion_opts -> Sem.tree -> printer =
  fun ~cfg ~opts tree ->
  let tei_attrs =
    Xml.attr ~ns:Xmlm.ns_xmlns "xmlns" "http://www.tei-c.org/ns/1.0" ::
    match tree.addr with
    | Some addr -> [
        Xml.attr ~ns:Xmlm.ns_xml "id" @@ xml_id_of_addr addr;
        Xml.attr "n" addr
      ]
    | _-> []
  in
  Xml.tag "TEI" tei_attrs [
    render_tei_header ~cfg tree;
    Xml.tag "text" [] [
      Xml.tag "body" [] [
        render ~cfg tree.body
      ]
    ]
  ]



and render_tree_metadata : cfg:cfg -> Sem.tree -> printer =
  fun ~cfg tree ->
  Xml.seq [
    Xml.tag "fileDesc" [] [
      Xml.tag "titleStmt" [] [
        Xml.tag "title" [] [
          tree.title |> Option.fold ~none:Xml.nil ~some:(render ~cfg)
        ]
      ];
      Xml.tag "publicationStmt" [] [
        Xml.tag "p" [] [
          Xml.text "Distributed as part of a forest (TODO, change)"
        ]
      ];
      Xml.tag "sourceDesc" [] [
        Xml.tag "p" [] [
          Xml.text "No print source exists: this is an original digital text"
        ]
      ]
    ];
    Xml.tag "profileDesc" [] []
  ]

and render_tei_header : cfg:cfg -> Sem.tree -> printer =
  fun ~cfg tree ->
  Xml.tag "teiHeader" [] [
    render_tree_metadata ~cfg tree
  ]


let render_tree_page : base_url:string option -> Sem.tree -> printer =
  fun ~base_url tree ->
  let cfg = {base_url} in
  let opts = Sem.{title_override = None; taxon_override = None; toc = false; show_heading = true; expanded = true; numbered = false; show_metadata = true} in
  let corpus_attrs =
    Xml.attr ~ns:Xmlm.ns_xmlns "xmlns" "http://www.tei-c.org/ns/1.0" ::
    match tree.addr with
    | Some addr -> [Xml.attr "n" addr]
    | None -> []
  in
  fun out ->
    Xmlm.output out @@ `Dtd (Some {|<?xml-model href="http://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng"?>|});
    render_tree ~cfg ~opts tree out
