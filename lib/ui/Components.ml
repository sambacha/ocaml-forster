open Forester_core
open Forester_render
open Forester_frontend
open Lwd_infix
open Notty
open Nottui
module Widget = Nottui_widgets

type history_item = string * ui Lwd.t
type command_history = history_item list

let prompt ~prompt_label =
  [
    I.strf "%s" prompt_label |> Ui.atom |> Lwd.return;
    I.string A.(fg green) "❯ " |> Ui.atom |> Lwd.return;
  ]
  |> Widget.hbox

let empty = Widget.empty_lwd

let help =
  let commands =
    let open Command in
    [ Quit; Edit (User_addr "<addr>"); View (User_addr "<addr>"); Help; Ls ]
  in
  let render_command_help cmd =
    match (cmd : Command.t) with
    | Command.Quit -> I.strf "quit %s" "" |> Ui.atom
    | Command.Edit addr -> I.strf "edit %a" pp_addr addr |> Ui.atom
    | Command.View addr -> I.strf "view %a" pp_addr addr |> Ui.atom
    | Command.Help -> I.strf "help %s" "" |> Ui.atom
    | Command.Ls -> I.strf "ls %s" "" |> Ui.atom
    | Command.Nop -> I.strf "nop %s" "" |> Ui.atom
  in
  Ui.vcat
    (Ui.atom (I.strf "Available commands")
    :: (commands |> List.map render_command_help))

let render_error (e : Command.error) : ui =
  match e with
  | Command.Unknown_command str ->
      Ui.hcat [ I.strf "unknown command %s" str |> Ui.atom ]
  | Command.Tree_not_found addr ->
      Ui.hcat
        [
          I.strf "tree at address %a could not be found" pp_addr addr |> Ui.atom;
        ]
  | Command.Custom string -> Ui.hcat [ I.string A.(empty) string |> Ui.atom ]

let render_source (t : Code.tree) =
  Formatter.format_code t.code
  |> Formatter.P.pretty_format |> Widget.string |> Lwd.return

let forest_summary trees =
  trees
  |> List.map (fun tree ->
         match tree with
         | Xml_tree2.{ frontmatter; mainmatter; backmatter } ->
             Render_tui.render_frontmatter frontmatter)
  |> Widget.vbox

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

let cmd_history : command_history -> ui Lwd.t =
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
             output;
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

let root ~quit ~handle =
  let history = Lwd.var ([] : command_history) in

  let update_history (item : history_item) =
    let h = Lwd.peek history in
    history $= item :: h
  in
  let focus_handle = Nottui.Focus.make () in
  let focus_handler state = function `Tab -> `Handled | _ -> `Unhandled in
  (* TODO: Figure out this Nottui gravity thing so that we can see the prompt
     even when the rendered output is long*)
  Nottui_widgets.scroll_area
  @@ Lwd_utils.pack Ui.pack_y
       [
         Lwd.bind ~f:cmd_history (Lwd.get history);
         input ~last_command:None ~focus_handle ~f:(fun string ->
             handle ~input:string ~update_history ~quit);
       ]
