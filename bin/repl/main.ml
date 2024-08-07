open Forester_core
open Forester_frontend
open Forester_ui
module Tty = Asai.Tty.Make (Reporter.Message)

let easy_run k =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Reporter.run ~emit:(Fun.const ()) ~fatal k

let () =
  Printexc.record_backtrace true;
  Eio_main.run @@ fun env ->
  easy_run @@ fun () -> Repl.run ~env ~config_file:"forest.toml"
