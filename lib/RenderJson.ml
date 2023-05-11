open Types

module Printer =
struct
  module P0 = 
  struct 
    type out = Format.formatter 
    let text txt fmt =
      Format.fprintf fmt "%s" txt
  end

  include PrinterKit.Kit (P0)

  let contents (printer : t) : string = 
    Format.asprintf "%a" (fun fmt _ -> printer fmt) ()  
end

let squares x = 
  Printer.seq ~sep:Printer.space 
    [Printer.text "["; x; Printer.text "]"]

let braces x = 
  Printer.seq ~sep:Printer.space 
    [Printer.text "{"; x; Printer.text "}"]

let comma = Printer.text ", "

let render_string_literal body = 
  Printer.seq [Printer.text "\""; body; Printer.text "\""]


let render_key k p = 
  Printer.seq ~sep:Printer.space
    [render_string_literal @@ Printer.text k;
     Printer.text ":";
     p]

let escape = 
  Str.global_substitute (Str.regexp {|"|}) @@ 
  fun _ -> {|\"|}

let rec render nodes : Printer.t = 
  Printer.iter render_node nodes

and render_node : Sem.node -> Printer.t = 
  function 
  | Sem.Text txt -> Printer.text @@ escape @@ StringUtil.title_case txt
  | Sem.Tag (_,_,body) -> Printer.iter ~sep:Printer.space render body
  | Sem.Link {title; _} -> render title
  | Sem.Transclude _ | Sem.EmbedTeX _ | Sem.Math _ | Sem.Block _ -> Printer.nil
  | Sem.Group (delim, body) ->
    let l, r = 
      match delim with 
      | Braces -> "{", "}"
      | Squares -> "[", "]"
      | Parens -> "(", ")"
    in
    Printer.seq 
      [Printer.text l;
       render body;
       Printer.text r]


let render_doc (env : RenderEnv.t) (doc : Sem.doc) : Printer.t = 
  render_key doc.addr @@ braces @@
  Printer.iter ~sep:comma (fun (k, x) -> render_key k x)
    ["title", render_string_literal @@ render doc.title;
     "taxon", 
     begin 
       match doc.taxon with 
       | None -> Printer.text "null" 
       | Some taxon -> render_string_literal @@ Printer.text @@ StringUtil.title_case taxon
     end;
     "route", render_string_literal @@ Printer.text @@ env#route doc.addr]

let render_docs (env : RenderEnv.t) (docs : Sem.doc list) : Printer.t =
  braces @@ Printer.iter ~sep:comma (render_doc env) docs