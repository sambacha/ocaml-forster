open Forester_core

type t = Quit | Edit of addr | View of addr | Ls | Help | Nop
[@@deriving show]

type error =
  | Unknown_command of string
  | Tree_not_found of addr
  | Custom of string
[@@deriving show]
