open Forester_core
open Forester_render
open Forester_frontend
open Lwd_infix

module Cache_handle = Algaeff.State.Make (struct
  type t = Irmin_defs.Store.t
end)

let make_dir ~env dir = Eio.Path.(Eio.Stdenv.fs env / dir)
let make_dirs ~env = List.map (make_dir ~env)

let parse str : (Command.t, Command.error) Result.t =
  let lexbuf = Lexing.from_string str in
  try Ok (Parser.main Cli_lexer.token lexbuf) with
  | Cli_lexer.Syntax_error tok ->
      (* I don't know of an easy way to integrate diagnostics reporting with Nottui, see
         https://github.com/RedPRL/asai/issues/150
      *)
      Reporter.emitf ~loc:(Range.of_lexbuf lexbuf) Parse_error
        "unrecognized token %S" tok;
      Error (Unknown_command tok)
  | Parser.Error ->
      Reporter.emitf ~loc:(Range.of_lexbuf lexbuf) Parse_error "failed to parse";
      Error (Unknown_command str)

let setup_cache ~env =
  let cache_path = Eio.Path.(env#cwd / "build") in
  let clock = Eio.Stdenv.clock env in
  let cache_config = Irmin_fs_unix.conf ~path:cache_path ~clock in
  let repo = Irmin_defs.Store.Backend.Repo.v cache_config in
  Irmin_defs.Store.main repo

let handle ~input ~quit =
  let cache = Cache_handle.get () in
  let cmd = parse input in
  match cmd with
  | Ok cmd -> (
      match cmd with
      | Edit addr -> (
          let tree = Cache.get_value_opt addr cache in
          match tree with
          | Some tree ->
              (* let o = Components.render_source tree in *)
              (* Code.show tree.code |> Widget.string |> Lwd.return in *)
              let o = Code.show tree.code in
              print_endline o;
              (input, o)
          | None ->
              let err =
                Command.Tree_not_found addr |> Command.show_error
                (* Command.Tree_not_found addr |> Components.render_error *)
                (* |> Lwd.return *)
              in
              (input, err))
      | View addr -> (
          let tree = Cache.get_artifact_opt addr cache in
          match tree with
          | Some tree ->
              let o =
                (* Render_tui.render_article tree *)
                (* Widget.string *)
                Format.asprintf "%a" Xml_tree2.(pp_article pp_content) tree
              in
              (input, o)
          | None ->
              let err =
                Command.Tree_not_found addr |> Command.show_error
                (* |> Components.render_error |> Lwd.return *)
              in
              (input, err))
      | Help -> (input, "TODO: implement help")
      | Ls ->
          let map = Cache.get_artifacts cache in

          let trees =
            Addr_map.fold (fun addr content acc -> content :: acc) map []
          in
          (* let rows = Components.forest_summary trees in *)
          (input, "TODO: implement ls")
      | Nop -> (input, "")
      | Quit ->
          quit $= true;
          (input, ""))
  | Error err -> (input, Command.show_error err)

let rec user_input prompt cb =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
      cb v;
      user_input prompt cb

let banner = {| |}

let run ~env ~config_file =
  let config = Config.parse_forest_config_file config_file in
  let tree_dirs = make_dirs ~env Config.Forest_config.(config.trees) in
  let cache = setup_cache ~env in
  let quit = Lwd.var false in
  Cache_handle.run ~init:cache @@ fun () ->
  let _ = Cache.update_values cache tree_dirs in
  let forest = Forest_reader.read_trees ~env @@ Cache.codes cache tree_dirs in
  let _ = Cache.persist_addr_map forest cache in

  LNoise.set_hints_callback (fun line ->
      if line <> "git remote add " then None
      else
        Some
          ( " <this is the remote name> <this is the remote URL>",
            LNoise.Yellow,
            true ));
  (* LNoise.history_load ~filename:"history.txt" |> ignore; *)
  (* LNoise.history_set ~max_length:100 |> ignore; *)
  LNoise.set_completion_callback (fun line_so_far ln_completions ->
      if line_so_far <> "" && line_so_far.[0] = 'h' then
        [ "Hey"; "Howard"; "Hughes"; "Hocus" ]
        |> List.iter (LNoise.add_completion ln_completions));
  [
    "   __                    _            ";
    "  / _| ___  _ __ ___ ___| |_ ___ _ __ ";
    " | |_ / _ \\| '__/ _ / __| __/ _ | '__|";
    " |  _| (_) | | |  __\\__ | ||  __| |   ";
    " |_|  \\___/|_|  \\___|___/\\__\\___|_|   ";
    "";
  ]
  |> List.iter print_endline;
  (fun input ->
    let update_history _ = () in
    let res = handle ~input ~quit in
    Printf.sprintf "%s" (snd res) |> print_endline
    (* if from_user = "quit" then exit 0; *)
    (* LNoise.history_add from_user |> ignore; *)
    (* LNoise.history_save ~filename:"history.txt" |> ignore; *)
    (* Printf.sprintf "Got: %s" from_user |> print_endline *))
  |> user_input "forest❯ "

(*
  Nottui.Ui_loop.run ~quit_on_ctrl_q:true ~quit_on_escape:false ~quit
    (Components.root ~quit ~handle)
    *)
