open Forester_core
open Nottui

type command_error = string
type command = Quit | Edit of addr | Help | Nop
type input = string
type command_result = (command * string, command_error) Result.t
type history_item = input * command_result
type command_history = history_item list

let prompt ~prompt_label =
  let open Notty in
  [
    Notty.I.strf "%s" prompt_label |> Nottui.Ui.atom |> Lwd.return;
    Notty.I.string A.(fg green) "❯ " |> Nottui.Ui.atom |> Lwd.return;
  ]
  |> Nottui_widgets.hbox

let input ~last_command ~prompt_label ~focus_handle ~f : Nottui.ui Lwd.t =
  let open Notty in
  let edit_field = Lwd.var ("", 0) in
  let caret_color =
    match last_command with
    | Some (Ok _) -> A.green
    | Some (Error _) -> A.red
    | None -> A.green
  in
  Nottui_widgets.hbox
    [
      prompt ~prompt_label;
      Nottui_widgets.hbox
      @@ [
           Nottui_widgets.edit_field (Lwd.get edit_field) ~focus:focus_handle
             ~on_change:(fun (text, x) -> Lwd.set edit_field (text, x))
             ~on_submit:(fun _ ->
               let s = fst @@ Lwd.peek edit_field in
               if s <> "" then (
                 f s;
                 Lwd.set edit_field ("", 0)));
         ];
    ]

let render_cmd_history : command_history -> Nottui.ui Lwd.t =
 fun items ->
  items
  |> List.map (fun item ->
         Nottui_widgets.vbox
           [
             Nottui_widgets.hbox
               [
                 prompt ~prompt_label:"Forest";
                 Lwd.return @@ Nottui_widgets.string (item |> fst);
               ];
             Nottui_widgets.string
               (item |> snd |> function
                | Ok (command, output) -> output
                | Error _ -> Format.sprintf "unrecognized command %s" (fst item))
             |> Lwd.return;
           ])
  |> List.rev |> Nottui_widgets.vbox

let parse_input (i : string) : (command, string) Result.t =
  if i = "quit" then Ok Quit
  else if i = "help" then Ok Help
  else if i = "error" then Error "error"
  else Error "unhandled"

let run () =
  let open Nottui in
  let quit = Lwd.var false in

  let history = Lwd.var ([] : command_history) in

  let focus_handle = Nottui.Focus.make () in
  let prompt_label = "Forest" in
  let update_history (item : history_item) =
    let h = Lwd.peek history in
    Lwd.set history (item :: h)
  in
  let on_receive_input i =
    let res = parse_input i in
    let handle cmd =
      match cmd with
      | Edit _ ->
          let o = "edit cmd is unimplemented" in
          update_history (i, Ok (cmd, o))
      | Help ->
          let o = "help cmd is unimplemented. CTRL-Q to quit" in
          update_history (i, Ok (cmd, o))
      | Nop ->
          let o = "" in
          update_history (i, Ok (cmd, o))
      | Quit -> Lwd.set quit true
    in
    match res with
    | Ok cmd -> handle cmd
    | Error e -> update_history (i, Error e)
  in
  let root =
    Lwd_utils.pack Ui.pack_y
      [
        Lwd.bind ~f:render_cmd_history (Lwd.get history);
        input ~last_command:None ~prompt_label ~focus_handle ~f:on_receive_input;
      ]
  in
  Ui_loop.run ~tick_period:0.2 ~quit ~quit_on_ctrl_q:true ~quit_on_escape:false
    root
