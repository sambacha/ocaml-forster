open Prelude
open Bwd
open Core

module E = Render_effect.Perform

module Printer = Xml_printer
type printer = Printer.printer

type part =
  | Top
  | Frontmatter
  | Mainmatter
  | Backmatter

type cfg = {base_url : string option; part : part; trail : int bwd option; counter : int ref}


let (let*?) = Option.bind

let rec render_node ~cfg : Sem.node -> printer =
  function
  | Sem.Text txt ->
    Printer.text txt
  | Sem.Math (mode, bdy) ->
    let attrs =
      match mode with
      | Inline -> []
      | Display -> ["display", "block"]
    in
    let module TP = Render_math_mode.Printer in
    Printer.tag "tex" attrs [
      Printer.text @@
      TP.contents @@
      Render_math_mode.render bdy
    ]
  | Sem.Link {title; dest} ->
    begin
      match E.get_doc dest with
      | Some _ ->
        render_internal_link ~cfg ~title ~addr:dest
      | None ->
        render_external_link ~cfg ~title ~url:dest
    end
  | Sem.Tag (name, attrs, xs) ->
    Printer.tag name attrs [render ~cfg xs]
  | Sem.Transclude (opts, addr) ->
    begin
      match E.get_doc addr with
      | None ->
        failwith @@ Format.sprintf "Failed to transclude non-existent tree with address '%s'" addr
      | Some doc ->
        render_transclusion ~cfg ~opts doc
    end
  | Sem.Query (opts, query) ->
    let docs = E.run_query query in
    begin
      match docs with
      | [] -> Printer.nil
      | _ ->
        let body =
          docs |> List.filter_map @@ fun (doc : Sem.doc) ->
          doc.addr |> Option.map @@ fun addr ->
          let opts = Sem.{expanded = false; show_heading = true; title_override = None; toc = false; numbered = false; show_metadata = true} in
          Sem.Transclude (opts, addr)
        in
        let doc : Sem.doc =
          {addr = None;
           taxon = None;
           title = None;
           authors = [];
           date = None;
           metas = [];
           tags = [];
           body = body}
        in
        render_transclusion ~cfg ~opts doc
    end
  | Sem.Embed_TeX {packages; source} ->
    let code =
      Render_math_mode.Printer.contents @@
      Render_math_mode.render source
    in
    let hash = Digest.to_hex @@ Digest.string code in
    E.enqueue_latex ~name:hash ~packages ~source:code;
    let path = Format.sprintf "resources/%s-web.svg" hash in
    Printer.tag "center" []
      [Printer.tag "img" ["src", path] []]
  | Sem.Block (title, body) ->
    Printer.tag "block" ["open", "open"] @@
    [Printer.tag "headline" [] [render ~cfg title];
     render ~cfg body]

and render_transclusion ~cfg ~opts doc =
  let cfg =
    let ctr = cfg.counter in
    let ix = if opts.numbered then !ctr + 1 else !ctr in
    ctr := ix;
    let trail =
      match opts.numbered with
      | true -> cfg.trail |> Option.map @@ fun tr -> Snoc (tr, ix)
      | false -> None
    in
    let counter = ref 0 in
    {cfg with trail; counter}
  in
  render_doc ~cfg ~opts doc

and render_internal_link ~cfg ~title ~addr =
  let url = E.route Xml addr in
  let target_title_attr =
    match Option.bind (E.get_doc addr) Sem.Doc.title_as_string with
    | Some s -> ["title", s]
    | None -> []
  in
  Printer.tag "link"
    (["href", url; "type", "local"] @ target_title_attr)
    [render ~cfg title]

and render_external_link ~cfg ~title ~url =
  Printer.tag "link"
    ["href", url; "type", "external"]
    [render ~cfg title]

and render ~cfg : Sem.t -> printer =
  Printer.iter (render_node ~cfg)

and render_author (author : string) =
  let cfg = {base_url = None; part = Frontmatter; trail = Some Emp; counter = ref 0} in
  (* If the author string is an address to a biographical page, then link to it *)
  match E.get_doc author with
  | Some bio ->
    begin
      match bio.addr with
      | None ->
        Printer.text author
      | Some addr ->
        let url = E.route Xml addr in
        Printer.tag "link"
          ["href", url; "type", "local"]
          [match bio.title with
           | None -> Printer.text "Untitled"
           | Some title -> render ~cfg title]
    end
  | None ->
    Printer.text author

and render_date (doc : Sem.doc) =
  match doc.date with
  | None -> Printer.nil
  | Some date ->
    let str = Format.asprintf "%a" Date.pp_human date in
    Printer.tag "date" [] [
      Printer.tag "year" [] [Printer.text @@ string_of_int @@ Date.year date];
      Date.month date |> Printer.option begin fun m ->
        Printer.tag "month" [] [Printer.text @@ string_of_int m]
      end;
      Date.day date |> Printer.option begin fun d ->
        Printer.tag "day" [] [Printer.text @@ string_of_int d]
      end;
    ]

and render_authors (doc : Sem.doc) =
  let contributors =
    match doc.addr with
    | Some addr -> E.contributors addr
    | None -> []
  in
  match doc.authors, contributors with
  | [], [] -> Printer.nil
  | authors, contributors ->
    Printer.tag "authors" [] [
      begin
        authors |> Printer.iter @@ fun author ->
        Printer.tag "author" [] [render_author author]
      end;
      begin
        contributors |> Printer.iter @@ fun contributor ->
        Printer.tag "contributor" [] [render_author contributor]
      end
    ]

and with_addr (doc : Sem.doc) k =
  match doc.addr with
  | Some addr -> k addr
  | None -> Printer.nil

and render_rss_link ~cfg doc =
  (* Only link to RSS if there is a base url, because RSS cannot be generated in the first place without one. *)
  cfg.base_url |> Printer.option @@ fun _ ->
  with_addr doc @@ fun addr ->
  Printer.tag "rss" [] [Printer.text (E.route Rss addr)]

and render_frontmatter ~cfg ?(toc = true) (doc : Sem.doc) =
  let anchor = string_of_int @@ Oo.id (object end) in
  Printer.tag "frontmatter" [] [
    if toc then render_trail cfg.trail else Printer.nil;
    Printer.tag "anchor" [] [Printer.text anchor];
    render_rss_link ~cfg doc;
    doc.taxon |> Printer.option begin fun taxon ->
      Printer.tag "taxon" [] [Printer.text @@ String_util.sentence_case taxon]
    end;
    with_addr doc (fun addr -> Printer.tag "addr" [] [Printer.text addr]);
    with_addr doc begin fun addr ->
      match E.source_path addr with
      | Some source_path ->
        Printer.tag "source-path" [] [Printer.text source_path]
      | None ->
        Printer.nil
    end;
    with_addr doc (fun addr -> Printer.tag "route" [] [Printer.text @@ E.route Xml addr]);
    render_date doc;
    render_authors doc;
    begin
      doc.title |> Printer.option @@ fun title ->
      Printer.tag "title" [] [
        render ~cfg:{cfg with part = Frontmatter} @@
        Sem.sentence_case title
      ]
    end;
    begin
      doc.metas |> Printer.iter @@ fun (key, body) ->
      Printer.tag "meta" ["name", key] [render ~cfg:{cfg with part = Frontmatter} body]
    end
  ]

and render_mainmatter ~cfg (doc : Sem.doc) =
  Printer.tag "mainmatter" [] [
    render ~cfg:{cfg with part = Mainmatter} doc.body
  ]

and render_backmatter ~cfg (doc : Sem.doc) =
  let cfg = {cfg with part = Backmatter} in
  let opts = Sem.{title_override = None; toc = false; show_heading = true; expanded = false; numbered = false; show_metadata = true} in
  with_addr doc @@ fun addr ->
  Printer.tag "backmatter" [] [
    Printer.tag "contributions" [] [
      E.contributions addr |> Printer.iter @@
      render_doc ~cfg ~opts
    ];
    Printer.tag "context" [] [
      E.parents addr |> Printer.iter @@
      render_doc ~cfg ~opts
    ];
    Printer.tag "related" [] [
      E.related addr |> Printer.iter @@
      render_doc ~cfg ~opts
    ];
    Printer.tag "backlinks" [] [
      E.backlinks addr |> Printer.iter @@
      render_doc ~cfg ~opts
    ];
    Printer.tag "references" [] [
      E.bibliography addr |> Printer.iter @@
      render_doc ~cfg ~opts
    ];
  ]

and render_trail trail =
  match trail with
  | None -> Printer.nil
  | Some trail ->
    let render_crumb i = Printer.tag "crumb" [] [Printer.text @@ string_of_int i] in
    Printer.tag "trail" [] @@
    List.map render_crumb @@
    Bwd.to_list trail

and bool_to_string =
  function
  | true -> "true"
  | false -> "false"


and trail_to_string =
  function
  | Emp -> ""
  | Snoc (trail, i) ->
    Format.sprintf "%s.%i" (trail_to_string trail) i


and render_doc ~cfg ~opts (doc : Sem.doc) : printer =
  let doc =
    match opts.title_override with
    | Some _ as title -> {doc with title}
    | None -> doc
  in
  let attrs =
    ["expanded", string_of_bool opts.expanded;
     "show-heading", string_of_bool opts.show_heading;
     "show-metadata", string_of_bool opts.show_metadata;
     "toc", string_of_bool opts.toc;
     "root", string_of_bool @@ Option.fold doc.addr ~none:false ~some:(fun addr -> E.is_root addr)]
  in
  Printer.tag "tree" attrs
    [render_frontmatter ~cfg ~toc:opts.toc doc;
     render_mainmatter ~cfg doc;
     match cfg.part with
     | Top -> render_backmatter ~cfg doc
     | _ -> Printer.nil]

let render_doc_page ~base_url ~trail (doc : Sem.doc) : printer =
  let cfg = {base_url; trail; part = Top; counter = ref 0} in
  let opts = Sem.{title_override = None; toc = false; show_heading = true; expanded = true; numbered = true; show_metadata = true} in
  Printer.with_xsl "forest.xsl" @@ render_doc ~cfg ~opts doc