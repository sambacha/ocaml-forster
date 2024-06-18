open Forester_core

val route : root:string option -> addr -> string

module type I = sig val root : string option end

module Make (_ : I) () :
sig
  val pp : ?stylesheet:string -> Format.formatter -> Xml_tree.tree_ -> unit
end