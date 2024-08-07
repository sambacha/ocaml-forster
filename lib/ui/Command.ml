open Forester_core

type t = Quit | Edit of addr | View of addr | Help | Nop [@@deriving show]

type command_error =
  | Unknown_command
  | Tree_not_found of string
  | Custom of string

type command_result = (t * string, command_error) Result.t
