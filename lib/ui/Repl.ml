open Forester_core
open Forester_render
open Forester_frontend

module Cache_handle = Algaeff.Reader.Make (struct
  type t = Irmin_defs.Store.t
end)

module Config_handle = Algaeff.State.Make (struct
  type t = Config.Forest_config.t
end)

let make_dir ~env dir = Eio.Path.(Eio.Stdenv.fs env / dir)
let make_dirs ~env = List.map (make_dir ~env)

let parse str : (Command.t, Command.error) Result.t =
  let lexbuf = Lexing.from_string str in
  try Ok (Parser.main Cli_lexer.token lexbuf) with
  | Cli_lexer.Syntax_error tok ->
      (* With the current code structure, the reporter is not able to open
         stdout, so displaying diagnostics fails:
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

let handle ~input ~env ~renderer =
  let cache = Cache_handle.read () in
  let cmd = parse input in
  match cmd with
  | Ok cmd -> (
      match cmd with
      | Edit addr -> (
          let tree = Cache.get_value_opt addr cache in
          match tree with
          | Some tree ->
              let o =
                Formatter.P.pretty_format @@ Formatter.format_code tree.code
              in
              o
          | None ->
              let err = Command.Tree_not_found addr |> Command.show_error in
              Format.sprintf "%s" err)
      | View addr -> (
          let tree = Cache.get_artifact_opt addr cache in
          match tree with
          | Some { frontmatter; mainmatter; backmatter } ->
              Format.sprintf "%s" (renderer mainmatter)
          | None ->
              let err = Command.Tree_not_found addr |> Command.show_error in
              Format.sprintf "%s" err)
      | Help -> Format.sprintf "TODO"
      | Update ->
          let config = Config_handle.get () in
          let cache = Cache_handle.read () in
          let dirs = make_dirs ~env config.trees in
          let reval =
            Cache.trees_to_reevaluate dirs cache
            |> Addr_map.to_list |> List.map snd
            |> Forest_reader.read_trees ~env
          in
          let _ = Cache.set_artifact_map reval cache in

          if Addr_map.cardinal reval = 0 then "all trees are up to date."
          else
            Format.asprintf "The following trees were updated:\n%a"
              (Format.pp_print_list Format.pp_print_string)
              (reval |> Addr_map.to_list
              |> List.map (fun (addr, _) -> Format.asprintf "%a" pp_addr addr))
      | Ls ->
          let map = Cache.get_artifacts cache in

          let trees =
            Addr_map.fold (fun addr content acc -> content :: acc) map []
          in
          let o =
            trees
            |> List.map
                 (fun
                   Xml_tree2.
                     {
                       frontmatter =
                         {
                           addr;
                           title;
                           dates;
                           attributions;
                           taxon;
                           number;
                           physical_parent;
                           designated_parent;
                           source_path;
                           tags;
                           metas;
                         };
                       mainmatter;
                       backmatter;
                     }
                 -> Format.asprintf "[%a] %s" pp_addr addr (renderer title))
          in
          Format.asprintf "%a" (Format.pp_print_list Format.pp_print_string) o
      | Nop -> ""
      | Quit -> exit 0)
  | Error err -> Format.sprintf "%s" (Command.show_error err)

let rec user_input prompt cb =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
      cb v;
      user_input prompt cb

let termcode n = "\x1B[" ^ string_of_int n ^ "m"
let bold str = termcode 1 ^ str
let dim str = termcode 2 ^ str
let darkgray str = termcode 90 ^ str
let red str = termcode 91 ^ str
let green str = termcode 92 ^ str
let yellow str = termcode 93 ^ str
let blue str = termcode 94 ^ str
let magenta str = termcode 95 ^ str
let cyan str = termcode 96 ^ str
let red_bg str = termcode 41 ^ str
let clear str = str ^ termcode 0

let forester_logo =
  let banner str = str |> bold |> green |> clear in
  let logo =
    [
      {| .-,--'                .           |};
      {|  \|__ ,-. ,-. ,-. ,-. |- ,-. ,-.  |};
      {|   |   | | |   |-' `-. |  |-' |    |};
      {|  `'   `-' '   `-' `-' `' `-' '    |};
    ]
  in
  List.map banner logo

let run ~env ~config_file =
  let config = Config.parse_forest_config_file config_file in
  let tree_dirs = make_dirs ~env Config.Forest_config.(config.trees) in
  let cache = setup_cache ~env in
  Cache_handle.run ~env:cache @@ fun () ->
  Config_handle.run ~init:config @@ fun () ->
  let _ = Cache.update_values cache tree_dirs in
  let forest = Forest_reader.read_trees ~env @@ Cache.codes cache tree_dirs in
  let _ = Cache.set_artifact_map forest cache in
  let module G = Forester_graphs.Make () in
  let module F = Forest2.Make (G) in
  let module Client = Plain_text_client.Make (F) in
  LNoise.catch_break true;
  forester_logo |> List.iter print_endline;
  try
    (fun from_user ->
      if from_user = "quit" then exit 0;
      LNoise.history_add from_user |> ignore;
      LNoise.history_save ~filename:".foresterHistory" |> ignore;
      handle ~env ~input:from_user ~renderer:Client.string_of_content
      |> print_endline)
    (* LNoise can't handle non-ascii chars in the prompt...*)
    |> user_input "forest> "
  with Sys.Break ->
    print_endline "bye!";
    exit 0
