
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]

open Peano
open Tokenize

exception WrongVariable of string
exception WrongOperation of string


let get_number: sexpr -> int = function
  | Int x -> x
  | _ -> raise (WrongVariable "Parameter should be of type Int a ");;


let rec eval: sexpr -> sexpr = function
  | Nil ->raise (WrongVariable "Variable cannot be Nil")
  | Int x-> Int x
  | String _ -> raise (WrongVariable "Variable cannot be Nil")
  | S_expr( (String "+"),S_expr ((Int x), (S_expr (Int y, Nil)))) ->
      Int (get_number (eval (Int x)) + get_number (eval (Int y)))
  | _ -> raise (WrongOperation "Error wrong exp");;

(* car, cdr, list, quote, if, cond, cons, null *)
