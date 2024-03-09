open Peano
open Tokenize

(* open Language.Rutalg *)

let string_to_sexpr str =
  list_string_to_exp (tokenize str);;
 
