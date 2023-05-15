module P =
struct
  type data = Syn.t
  type tag = unit

  type hook = unit (* for modifier hooks; unused here *)
  type context = unit (* for advanced printing and reporting; unused here *)
end

module Mod = Yuujinchou.Modifier.Make (P)
module Scope = Yuujinchou.Scope.Make (P) (Mod)