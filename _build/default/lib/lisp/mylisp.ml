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

(* ((lambda (x y)(+ x y)) 1 2) *)
(* ((x . 1) (y . 2)) *)
(* (x y) (1 2) *)
(* (z x y) *)
(* (3 1 2) *)
(*   S_expr (S_expr(S_expr("Z", Nil), S_expr(1, Nil) ),  *)
(*          *)
(*         (S_expr ( S_expr( "X", S_expr ("Y",  Nil))), *)
(*          S_expr(1, S_expr( 2,  Nil)  ) ) ) *)
(* (  ((z) . (3))  ((x y) . (1 2))  ) *)


let rec pair_lis parameters values =
  match (parameters, values)  with
  | (Nil, Nil) -> Nil
  | (Nil, _) | (_, Nil) -> raise (WrongVariable "lists should be equale")
  | ( S_expr(name, next_name), S_expr (value, next_value)  ) -> 
   S_expr( S_expr(name, value),(pair_lis next_name next_value ) )
  
let rec look_up assoscation_list name = 
  match assoscation_list with
  | Nil -> Nil
  | S_expr( S_expr((String name_check), value ), next ) ->
    if  (String.equal  name name_check) then S_expr((String name), value )
    else look_up next name
  | _ -> raise (WrongVariable "Something went wrong")
    

let eval exp = 
  let rec evaluate exp = 
  print_exsprassion exp;
  match exp with
  | Nil -> Nil
  | Int x-> Int x
  | String x -> String x 
  | S_expr( (String "if"), S_expr (conditiion, S_expr (output1, S_expr( output2, Nil) ) )) ->
   (
    match evaluate conditiion with
    | String "t" -> evaluate output1
    | String "f" -> evaluate output2
    | _ ->  raise (WrongOperation "Conditiion didn't retune boolean");
    ) 
  | S_expr( (String "cond"), x )-> 
      eval_cond x 
  
  | S_expr( (String "quote"), S_expr (cond, Nil )) -> cond
  | S_expr((String operation ), x) -> apply operation (eval_list x)
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
    (* Printf.printf "cond -> "; *)
    (* print_exsprassion_full cond; *)
    (* Printf.printf "cond -> ";  *)
    (* print_exsprassion cond; *)
    
    match (evaluate cond) with
    | String "t" -> evaluate res
    | String "f" -> eval_cond next
    | _ -> raise (WrongOperation "Conditiion is not true or false")
    )
  | _ -> raise (WrongOperation "Cond should have S_expr inside")

  and apply func exp = 
  print_exsprassion exp;
    match (func, exp) with
    | ("+", S_expr ((Int x), (S_expr ((Int y), Nil)))) -> 
        Int ( x  +  y)
    | ("-", S_expr ((Int x), (S_expr ((Int y), Nil)))) -> 
        Int ( x  -  y)
    | ("*", S_expr ((Int x), (S_expr ((Int y), Nil)))) -> 
        Int ( x  *  y)
    | ("/", S_expr ((Int x), (S_expr ((Int y), Nil)))) -> 
        Int ( x  /  y)

    | ("cons", S_expr ( x, (S_expr (y, Nil)))) ->
        S_expr ( x,  y )
    | ("car", S_expr ( S_expr (res, _ ) , Nil) ) -> res
    | ("cdr", S_expr ( S_expr (_, res ) , Nil) ) -> res
    | ("list", x ) -> x
    | ( "null", S_expr (Nil, Nil )) -> String "t"
    | ( "null", S_expr (_, Nil )) -> String "f"

   | _ -> raise (WrongOperation "Error wrong exp")

  in
  evaluate exp 

(* (+  ( + 1 2 ) 1) *)
    (* apply(+,[3,1]) *)


(* car, cdr, list, quote, if, cond, cons, null *)

(* (list 1 2 3) *)
(* (1 2 3) *)
(* (cond (x y) (z q) ... (f g )) *)

    (* (+ ( 2 3) 4)   (apply + ( /6 4)) *)




