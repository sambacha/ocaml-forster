type t = Trie.path * int

let counter = ref 0

let fresh path =
  counter := !counter + 1;
  path, !counter

let clone (path, _) = fresh path

let pp fmt (sym, ix) =
  Format.fprintf fmt "%a%i" Trie.pp_path sym ix

let show x = Format.asprintf "%a" pp x

let compare = compare

let repr : t Repr.t =
  Repr.pair (Repr.list Repr.string) Repr.int
