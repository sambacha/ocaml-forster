open Prelude
open Core

module Printer =
struct
  module P0 =
  struct
    type out = Format.formatter
    let text txt fmt =
      Format.fprintf fmt "%s" txt
  end

  include Printer_kit.Kit (P0)

  let contents (printer : t) : string =
    Format.asprintf "%a" (fun fmt _ -> printer fmt) ()
end

type cfg = {tex : bool}

let rec render_node ~cfg : Sem.node Range.located -> Printer.t =
  fun located ->
  match located.value with
  | Sem.Text txt | Sem.Verbatim txt ->
    Printer.text txt
  | Sem.Math (mode, xs) ->
    Printer.seq [
      Printer.text "{";
      render ~cfg xs;
      Printer.text "}"
    ]
  | Sem.Xml_tag (_, _, body) ->
    render ~cfg body
  | Sem.If_tex (x , y) ->
    if cfg.tex then render ~cfg x else render ~cfg y
  | Sem.Unresolved name ->
    Printer.seq
      [Printer.text "\\";
       Printer.text name]
  | node ->
    Reporter.fatalf ?loc:located.loc Type_error "Render_TeX_like: cannot render this kind of object"

and render ~cfg =
  Printer.iter (render_node ~cfg)
