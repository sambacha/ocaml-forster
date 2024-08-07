open Forester_core
open Forester_frontend
open Nottui
open Lwd_infix
module Widget = Nottui_widgets

module Cache_handle = Algaeff.State.Make (struct
  type t = Irmin_defs.Store.t
end)

let make_dir ~env dir = Eio.Path.(Eio.Stdenv.fs env / dir)
let make_dirs ~env = List.map (make_dir ~env)

let parse str =
  let lexbuf = Lexing.from_string str in
  try Ok (Parser.main Cli_lexer.token lexbuf) with
  | Cli_lexer.Syntax_error tok ->
      Reporter.emitf ~loc:(Range.of_lexbuf lexbuf) Parse_error
        "unrecognized token %S" tok;
      Error ""
  | Parser.Error ->
      Reporter.emitf ~loc:(Range.of_lexbuf lexbuf) Parse_error "failed to parse";
      Error ""

let setup_cache ~env =
  let cache_path = Eio.Path.(env#cwd / "build") in
  let clock = Eio.Stdenv.clock env in
  let cache_config = Irmin_fs_unix.conf ~path:cache_path ~clock in
  let repo = Irmin_defs.Store.Backend.Repo.v cache_config in
  Irmin_defs.Store.main repo

let handle ~input ~update_history ~quit =
  let cache = Cache_handle.get () in
  let cmd = parse input in
  match cmd with
  | Ok cmd -> (
      match cmd with
      | Edit addr ->
          let tree = Cache.get_value addr cache in
          let o = Code.show tree.code in
          update_history (input, Widget.string o)
      | View addr ->
          let tree = Cache.get_value addr cache in
          let o = Widget.string @@ Code.show tree.code in
          update_history (input, o)
      | Help ->
          let o =
            Widget.string @@ "help cmd is unimplemented. CTRL-Q to quit"
          in
          update_history (input, o)
      | Nop ->
          let o = Widget.string "" in
          update_history (input, o)
      | Quit -> quit $= true)
  | Error _ -> ()

let root_component ~quit =
  let open Components in
  let history = Lwd.var ([] : command_history) in

  let update_history (item : history_item) =
    let h = Lwd.peek history in
    history $= item :: h
  in
  let focus_handle = Nottui.Focus.make () in
  let focus_handler state = function `Tab -> `Handled | _ -> `Unhandled in
  let pressed_tab = Lwd.var false in
  Lwd_utils.pack Ui.pack_y
    [
      Lwd.bind ~f:render_cmd_history (Lwd.get history);
      input ~last_command:None ~focus_handle ~f:(fun string ->
          handle ~input:string ~update_history ~quit);
    ]

let run ~env ~config_file =
  let config = Forester_frontend.Config.parse_forest_config_file config_file in
  let tree_dirs = make_dirs ~env Config.Forest_config.(config.trees) in
  let cache = setup_cache ~env in
  let quit = Lwd.var false in
  Cache_handle.run ~init:cache @@ fun () ->
  let _ = Cache.update_values cache tree_dirs in
  Ui_loop.run ~quit_on_ctrl_q:true ~quit_on_escape:false ~quit
    (root_component ~quit)
