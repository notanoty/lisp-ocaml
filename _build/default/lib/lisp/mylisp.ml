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


let eval exp = 
  let rec evaluate exp = 
  print_exsprassion exp;
  match exp with
  | Nil -> Nil
  | Int x-> Int x
  | String x -> String x 
  | S_expr( (String "list"), x ) ->  eval_list x (*Fix это и квоат и лист*)  
  | S_expr( (String "car"), S_expr ( x , _) ) ->
    let S_expr( x , _) = evaluate x in x

  | S_expr( (String "cdr"), S_expr ( x , _) ) ->
    let S_expr( _ , x) = evaluate x in x
  | S_expr( (String "null"), S_expr (x, _ )) ->
    ( 
    match evaluate x with
    | Nil -> String "t"
    | _ -> String "f"
    )

  | S_expr( (String "if"), S_expr (cond, S_expr (output1, S_expr( output2, Nil) ) )) ->
   (
    match evaluate cond with
   | String "t" -> evaluate output1
    | String "f" -> evaluate output2
    | _ ->  raise (WrongOperation "Problem with conditiion");
    ) 
  | S_expr( (String "cond"), x )-> 
      eval_cond x 
  | S_expr( (String "cons"),S_expr ( x, (S_expr (y, Nil)))) -> S_expr (evaluate x, evaluate y )
  
  | S_expr( (String "quote"), S_expr (cond, Nil )) -> cond
  | S_expr((String operation ), x) -> apply operation x
    | _ -> raise (WrongOperation "Error wrong exp")
  


  and eval_list exp =
  match exp with
  | Nil -> Nil
  | S_expr( x , y) -> S_expr (evaluate x, eval_list y)
  | _ -> raise (WrongVariable "List can not be anything but S_expr") 
  


  and eval_cond exp = 
  match exp with
  | Nil -> Nil
  | S_expr( S_expr (cond, S_expr (res, Nil)), next) -> 
    (
    Printf.printf "cond -> ";
    print_exsprassion_full cond;
    Printf.printf "cond -> ";
    print_exsprassion cond;
    
        match (evaluate cond) with
    | String "t" -> evaluate res
    | String "f" -> eval_cond next
    | _ -> raise (WrongOperation "Conditiion is not true or false")
    )
  | _ -> raise (WrongOperation "Cond should have S_expr inside")

  and apply func exp = 
  print_exsprassion exp;
    match (func, exp) with
    | ("+", S_expr ( x, (S_expr (y, Nil)))) -> 
      Int (get_number (evaluate x ) + get_number (evaluate y))
    | ("-", S_expr ( x, (S_expr (y, Nil)))) -> 
      Int (get_number (evaluate x ) - get_number (evaluate y))
    | ("*", S_expr ( x, (S_expr (y, Nil)))) -> 
      Int (get_number (evaluate x ) * get_number (evaluate y))
    | ("/", S_expr ( x, (S_expr (y, Nil)))) -> 
      Int (get_number (evaluate x ) / get_number (evaluate y))
    | _ -> raise (WrongOperation "Error wrong exp")

  in
  evaluate exp 
(* car, cdr, list, quote, if, cond, cons, null *)

(* (list 1 2 3) *)
(* (1 2 3) *)
(* (cond (x y) (z q) ... (f g )) *)

    (* (+ ( 2 3) 4)   (apply + ( /6 4)) *)




