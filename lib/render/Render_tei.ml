open Prelude
open Bwd
open Core

module E = Render_effect.Perform

module Xml = Xml_printer
type printer = Xml.printer

type cfg = {base_url : string option}

let rec render_node : cfg:cfg -> Sem.node Range.located -> printer =
  fun ~cfg located ->
  match located.value with
  | _ ->
    Xml.nil

and render : cfg:cfg -> Sem.t -> printer =
  fun ~cfg ->
  Xml.iter (render_node ~cfg)


and render_tree : cfg:cfg -> opts:Sem.transclusion_opts -> Sem.tree -> printer =
  fun ~cfg ~opts tree ->
  Xml.nil

let render_tree_page : base_url:string option -> Sem.tree -> printer =
  fun ~base_url tree ->
  let cfg = {base_url} in
  let opts = Sem.{title_override = None; taxon_override = None; toc = false; show_heading = true; expanded = true; numbered = false; show_metadata = true} in
  Xml.document @@
  Xml.tag "TEI" [Xml.attr ~ns:Xmlm.ns_xmlns "xmlns" "http://www.tei-c.org/ns/1.0"] [
    render_tree ~cfg ~opts tree
  ]
