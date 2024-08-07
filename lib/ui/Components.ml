open Forester_core
open Lwd_infix
open Notty
open Nottui
module Widget = Nottui_widgets

type history_item = string * ui
type command_history = history_item list

let prompt ~prompt_label =
  [
    I.strf "%s" prompt_label |> Ui.atom |> Lwd.return;
    I.string A.(fg green) "❯ " |> Ui.atom |> Lwd.return;
  ]
  |> Widget.hbox

let input ~last_command ~focus_handle ~f : ui Lwd.t =
  let edit_field = Lwd.var ("", 0) in
  let caret_color =
    match last_command with
    | Some (Ok _) -> A.green
    | Some (Error _) -> A.red
    | None -> A.green
  in
  Widget.hbox
    [
      prompt ~prompt_label:"Forest";
      Widget.hbox
      @@ [
           Widget.edit_field (Lwd.get edit_field) ~focus:focus_handle
             ~on_change:(fun (text, x) -> edit_field $= (text, x))
             ~on_submit:(fun _ ->
               let s = fst @@ Lwd.peek edit_field in
               if s <> "" then (
                 f s;
                 edit_field $= ("", 0)));
         ];
    ]

let render_cmd_history : command_history -> ui Lwd.t =
 fun items ->
  items
  |> List.map (fun (input, output) ->
         Widget.vbox
           [
             Widget.hbox
               [
                 prompt ~prompt_label:"Forest";
                 Lwd.return @@ Nottui_widgets.string input;
               ];
             output |> Lwd.return;
           ])
  |> List.rev |> Widget.vbox

let autocomplete (suggestions : (addr * string) list) f =
  let handler ~x:_ ~y:_ addr = function `Left -> f addr | _ -> `Handled in
  let item ((addr, str) : addr * string) : ui Lwd.t =
    let i =
      match addr with
      | User_addr id ->
          Ui.atom
          @@ I.hcat [ I.string A.(st underline) id; I.string A.(empty) str ]
      | Machine_addr _ -> Ui.empty
    in
    Ui.mouse_area (handler addr) i |> Lwd.return
  in
  Widget.hbox @@ List.map item suggestions
