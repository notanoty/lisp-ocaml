[@@@ocaml.warning "-8"]
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
  | Nil -> Nil
  | Int x-> Int x
  | String _ -> raise (WrongVariable "Variable cannot be string")
  | S_expr( (String "+"),S_expr ( x, (S_expr (y, Nil)))) ->
      Int (get_number (eval  x) + get_number (eval  y))
  | S_expr( (String "-"),S_expr ( x, (S_expr (y, Nil)))) ->
      Int (get_number (eval  x) - get_number (eval  y))
  | S_expr( (String "list"), x ) ->  x (*Fix это и квоат и лист*)  
  | S_expr( (String "car"), S_expr ( x , _) ) ->
    let S_expr( x , _) = eval x in x

  | S_expr( (String "cdr"), S_expr ( x , _) ) ->
    let S_expr( _ , x) = eval x in x
  | S_expr( (String "null"), S_expr (x, _ )) ->
    ( 
    match eval x with
    | Nil -> String "t"
    | _ -> String "f"
    )

  | S_expr( (String "if"), S_expr (cond, S_expr (output1, S_expr( output2, Nil) ) )) ->
   (
    match eval cond with
    | String "t" -> eval output1
    | String "f" -> eval output2
    | _ ->  raise (WrongOperation "Problem with conditiion");
    ) 
  | S_expr( (String "cons"),S_expr ( x, (S_expr (y, Nil)))) -> S_expr (eval x, eval y )
  
  | S_expr( (String "quote"), S_expr (cond, Nil )) -> cond
  | _ -> raise (WrongOperation "Error wrong exp");;

(* car, cdr, list, quote, if, cond, cons, null *)









