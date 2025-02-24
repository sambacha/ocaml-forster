open Eio.Std
open Forester_core
open Forester_frontend
open Cmdliner

module Tty = Asai.Tty.Make (Forester_core.Reporter.Message)

let make_dir ~env dir =
  Eio.Path.(Eio.Stdenv.fs env / dir)

let make_dirs ~env =
  List.map (make_dir ~env)

let internal_config_from_config ~env (config : Forester_frontend.Config.Forest_config.t) =
  Forest.
    {env;
     root = config.root;
     assets_dirs = make_dirs ~env config.assets;
     theme_dir = make_dir ~env config.theme;
     stylesheet = config.stylesheet;
     ignore_tex_cache = false;
     no_assets = false;
     no_theme = false}

let version =
  Format.asprintf "%s" @@
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let build ~env config_filename dev render_only ignore_tex_cache no_assets no_theme  =
  let config, internal_cfg =
    Reporter.profile "read configuration" @@ fun () ->
    let config = Forester_frontend.Config.parse_forest_config_file config_filename in
    config, internal_config_from_config ~env config
  in
  let parsed_trees =
    Reporter.profile "parse trees" @@ fun () ->
    Process.read_trees_in_dirs ~dev @@ make_dirs ~env config.trees
  in
  let forest =
    Reporter.profile "expand, evaluate, and analyse forest" @@ fun () ->
    Forest.plant_forest parsed_trees
  in
  Reporter.profile "render forest" @@ fun () ->
  let render_only =
    render_only |>
    Option.map @@
    List.map @@ fun addr ->
    User_addr addr
  in
  Forest.render_trees ~cfg:internal_cfg ~forest ~render_only

let new_tree ~env config_filename dest_dir prefix template random =
  let config = Forester_frontend.Config.parse_forest_config_file config_filename in
  let internal_config =
    {(internal_config_from_config ~env config) with
     no_assets = true;
     no_theme = true}
  in
  let input_dirs = make_dirs ~env config.trees in
  let forest =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev:true ~ignore_malformed:true input_dirs
  in
  let addrs =
    Addr_map.bindings forest.trees
    |> List.to_seq
    |> Seq.map fst
    |> Seq.filter_map Addr.to_user_addr
  in
  let mode = if random then `Random else `Sequential in
  let addr = Forest.create_tree ~cfg:internal_config ~dest:(make_dir ~env dest_dir) ~prefix ~template ~addrs ~mode in
  Format.printf "%s/%s.tree\n" dest_dir addr

let complete ~env config_filename title =
  let config = Forester_frontend.Config.parse_forest_config_file config_filename in
  let forest =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev:true ~ignore_malformed:true @@
    make_dirs ~env config.trees
  in
  let completions = Forest.complete ~forest title in
  completions |> Seq.iter @@ fun (addr, title) ->
  Format.printf "%s, %s\n" addr title

let query_taxon ~env filter_taxa config_filename =
  let config = Forester_frontend.Config.parse_forest_config_file config_filename in
  let forest =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev:true ~ignore_malformed:true @@
    make_dirs ~env config.trees
  in
  let taxa = Forest.taxa ~forest in
  let filtered_taxa =
    taxa |> Seq.filter @@ fun (_, taxon) ->
    List.mem taxon filter_taxa
  in
  filtered_taxa
  |> Seq.iter @@ fun (addr, taxon) ->
  Format.printf "%s, %s\n" addr taxon

let query_tag ~env filter_tags config_filename =
  let config = Forester_frontend.Config.parse_forest_config_file config_filename in
  let forest =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev:true ~ignore_malformed:true @@
    make_dirs ~env config.trees
  in
  let tags = Forest.tags ~forest in
  let filtered =
    tags |> Seq.filter @@ fun (_, ts) ->
    List.for_all (fun t -> List.mem t ts) ts
  in
  filtered
  |> Seq.iter @@ fun (addr, tags) ->
  let rec tag_string = function
    | [] -> ""
    | hd :: tl -> ", " ^ hd ^ tag_string tl
  in
  Format.printf "%s%s\n" addr (tag_string tags)

let query_all ~env config_filename =
  let config = Forester_frontend.Config.parse_forest_config_file config_filename in
  let (forest : Forest.forest) =
    Forest.plant_forest @@
    Process.read_trees_in_dirs ~dev:true ~ignore_malformed:true @@
    make_dirs ~env config.trees
  in
  forest.trees
  |> Forester_render.Render_json.render_trees ~root:config.root ~dev:true
  |> Yojson.Basic.to_string
  |> Format.printf "%s"


let init ~env () =
  let default_theme_url =
    "https://git.sr.ht/~jonsterling/forester-base-theme"
  in
  let theme_version = "4.3-dev" in
  let (/) = Eio.Path.(/) in
  let fs = Eio.Stdenv.fs env in
  let try_create_dir name =
    try Eio.Path.mkdir ~perm:0o700 (fs / name)
    with _ ->
      Reporter.emitf Initialization_warning "Directory `%s` already exists" name
  in

  let default_config_str =
    (* More convenient to just write this string instead of constructing it with the toml library*)
    {|[forest]
trees = ["trees" ]                   # The directories in which your trees are stored
assets = ["assets"]                  # The directories in which your assets are stored
theme = "theme"                      # The directory in which your theme is stored
|}
  in

  let index_tree_str =
    {|
\title{Hello, World!}
\p{
  Welcome to your first tree! This tree is the root of your forest.
  \ul{
    \li{[Build and view your forest for the first time](http://www.jonmsterling.com/jms-007D.xml)}
    \li{[Overview of the Forester markup language](http://www.jonmsterling.com/jms-007N.xml)}
    \li{[Creating new trees](http://www.jonmsterling.com/jms-007H.xml)}
    \li{[Creating your personal biographical tree](http://www.jonmsterling.com/jms-007K.xml)}
  }
}
|} in
  let gitignore = {|output/|} in
  begin
    if Eio.Path.is_file (Eio.Stdenv.fs env / "forest.toml") then
      Reporter.emitf Initialization_warning "forest.toml already exists"
    else
      Eio.Path.(save ~create:(`Exclusive 0o600) (fs / "forest.toml") default_config_str)
  end;

  Eio.Path.(save ~create:(`Exclusive 0o600) (fs / ".gitignore") gitignore);

  let proc_mgr = Eio.Stdenv.process_mgr env in
  begin
    try
      let shut_up = Stdlib.Buffer.create 1 |> Eio.Flow.buffer_sink in
      let quietly_run cmd =
        Eio.Process.run proc_mgr ~stdout:shut_up ~stderr:shut_up cmd
      in
      [
        [ "git"; "init" ];
        [ "git"; "branch"; "-m"; "main" ];
        [ "git"; "submodule"; "add"; default_theme_url; "theme" ];
        [ "git"; "-C"; "theme"; "checkout"; theme_version ];
      ]
      |> List.iter quietly_run
    with _ ->
      Reporter.fatalf Configuration_error
        {|Failed to set up theme. To perform this step manually, run the commands

   git init
   git submodule add %s
   git -C theme checkout %s|}
        default_theme_url theme_version
  end;

  [ "trees"; "assets" ] |> List.iter try_create_dir;

  begin
    try
      Eio.Path.(save ~create:(`Exclusive 0o600) (fs / "trees" / "index.tree") index_tree_str)
    with _ ->
      Reporter.with_backtrace Emp @@ fun () ->
      Reporter.emitf Initialization_warning "`index.tree` already exists"
  end;

  build ~env "forest.toml" true None false false false ;
  Format.printf "%s" "Initialized forest, try editing `trees/index.tree` and running `forester build`. Afterwards, you can open `output/index.xml` in your browser to view your forest.\n"

let arg_config =
  let doc = "A TOML file like $(i,forest.toml)" in
  Arg.(value & pos 0 file "forest.toml" & info [] ~docv:"FOREST" ~doc)

let build_cmd ~env =
  let arg_dev =
    let doc = "Run forester in development mode; this will attach source file locations to the generated json." in
    Arg.value @@ Arg.flag @@ Arg.info ["dev"] ~doc
  in

  let arg_ignore_tex_cache =
    let doc = "Ignore the SVG cache when building LaTeX assets." in
    Arg.value @@ Arg.flag @@ Arg.info ["ignore-tex-cache"] ~doc
  in

  let arg_no_assets =
    let doc = "Build without copying the asset directory" in
    Arg.value @@ Arg.flag @@ Arg.info ["no-assets"] ~doc
  in

  let arg_no_theme =
    let doc = "Build without copying the theme directory" in
    Arg.value @@ Arg.flag @@ Arg.info ["no-theme"] ~doc
  in

  let arg_render_only =
    let doc = "Builds the entire forest but renders only the specified comma-separated list of trees" in
    Arg.value @@ Arg.opt (Arg.some' (Arg.list Arg.string)) None @@ Arg.info ["render-only"] ~docv:"TREES" ~doc
  in

  let doc = "Build the forest" in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command builds a hypertext $(b,forest) from trees stored in each $(i,INPUT_DIR) or any of its subdirectories; tree files are expected to be of the form $(i,addr.tree) where $(i,addr) is the global address of the tree. Note that the physical location of a tree is not taken into account, and two trees with the same address are not permitted.";
  ]
  in
  let info = Cmd.info "build" ~version ~doc ~man in
  Cmd.v info
    Term
    .(const (build ~env)
      $ arg_config
      $ arg_dev
      $ arg_render_only
      $ arg_ignore_tex_cache
      $ arg_no_assets
      $ arg_no_theme)

let new_tree_cmd ~env =
  let arg_prefix =
    let doc = "The namespace prefix for the created tree." in
    Arg.required @@ Arg.opt (Arg.some Arg.string) None @@
    Arg.info ["prefix"] ~docv:"XXX" ~doc
  in
  let arg_template =
    let doc = "The tree to use as a template" in
    Arg.value @@ Arg.opt (Arg.some Arg.string) None @@
    Arg.info ["template"] ~docv:"XXX" ~doc
  in
  let arg_dest_dir =
    let doc = "The directory in which to deposit created tree." in
    Arg.required @@ Arg.opt (Arg.some Arg.dir) None @@
    Arg.info ["dest"] ~docv:"DEST" ~doc
  in
  let arg_random =
    let doc = "True if the new tree should have id assigned randomly rather than sequentially" in
    Arg.value @@ Arg.flag @@ Arg.info ["random"] ~doc
  in
  let doc = "Create a new tree." in
  let info = Cmd.info "new" ~version ~doc in
  Cmd.v info
    Term
    .(const (new_tree ~env)
      $ arg_config
      $ arg_dest_dir
      $ arg_prefix
      $ arg_template
      $ arg_random)

let complete_cmd ~env =
  let arg_title =
    let doc = "The tree title prefix to complete." in
    Arg.value @@ Arg.opt Arg.string "" @@
    Arg.info ["title"] ~docv:"title" ~doc
  in
  let doc = "Complete a tree title." in
  let info = Cmd.info "complete" ~version ~doc in
  Cmd.v info Term.(const (complete ~env) $ arg_config $ arg_title)

let query_taxon_cmd ~env =
  let arg_taxa =
    Arg.(value @@ pos_all string [] @@ info [] ~docv:"TAXON")
  in
  let doc = "List all trees of taxon TAXON" in
  let info = Cmd.info "taxon" ~version ~doc in
  Cmd.v info Term.(const (query_taxon ~env) $ arg_taxa $ arg_config)

let query_tag_cmd ~env =
  let arg_tags =
    Arg.(value @@ pos_all string [] @@ info [] ~docv:"TAG")
  in
  let doc = "List all trees with tag TAG" in
  let info = Cmd.info "tag" ~version ~doc in
  Cmd.v info Term.(const (query_tag ~env) $ arg_tags $ arg_config)

let query_all_cmd ~env =
  let doc = "List all trees in JSON format" in
  let info = Cmd.info "all" ~version ~doc in
  Cmd.v info Term.(const (query_all ~env) $ arg_config)

let query_cmd ~env =
  let doc = "Query your forest" in
  let info = Cmd.info "query" ~version ~doc in
  Cmd.group info [query_taxon_cmd ~env; query_tag_cmd ~env; query_all_cmd ~env]

let init_cmd ~env =
  let doc = "Initialize a new forest" in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command initializes a $(b,forest) in the current directory. This involves initialising a git repository, setting up a git submodule for the theme, creating an assets and trees directory, as well as a config file."
  ]
  in
  let info = Cmd.info "init" ~version ~doc ~man in
  Cmd.v info Term.(const (init ~env) $ const ())

let cmd ~env =
  let doc = "a tool for tending mathematical forests" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <~jonsterling/forester-discuss@lists.sr.ht>." ;
    `S Manpage.s_authors;
    `P "Jonathan Sterling"
  ]
  in

  let info = Cmd.info "forester" ~version ~doc ~man in
  Cmd.group info [build_cmd ~env; new_tree_cmd ~env; complete_cmd ~env; query_cmd ~env; init_cmd ~env;]


let () =
  Random.self_init ();
  Printexc.record_backtrace true;
  Eio_main.run @@ fun env ->
  Forester_core.Reporter.easy_run @@ fun () ->
  exit @@ Cmd.eval ~catch:false @@ cmd ~env
