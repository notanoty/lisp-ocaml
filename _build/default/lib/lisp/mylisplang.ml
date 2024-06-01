[@@@ocaml.warning "-8"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]

open Peano
open Tokenize
open Parsing

exception WrongVariable of string
exception WrongOperation of string
exception ParsingError of string

let get_number : sexpr -> int = function
  | Int x -> x
  | _ -> raise (WrongVariable "Parameter should be of type Int a ")

(* ((lambda (x y)(+ x y)) 1 2) *)
(* ((x . 1) (y . 2)) *)
(* (x y) (1 2) *)
(* (z x y) *)
(* (3 1 2) *)
(*   S_expr ( S_expr(S_expr("Z", Nil), S_expr(1, Nil) ),  *)
(*          *)
(*         (S_expr ( S_expr( "X", S_expr ("Y",  Nil))), *)
(*          S_expr(1, S_expr( 2,  Nil)  ) ) ) *)
(* (  ((z) . (3))  ((x y) . (1 2))  ) *)

let rec pair_lis parameters values =
  match (parameters, values) with
  | Nil, Nil -> Nil
  | Nil, _ | _, Nil -> raise (WrongVariable "lists should be equale")
  | S_expr (name, next_name), S_expr (value, next_value) ->
      S_expr (S_expr (name, value), pair_lis next_name next_value)

let rec look_up assoscation_list name =
  match assoscation_list with
  | Nil -> Nil
  | S_expr (S_expr (String name_check, value), next) ->
      if String.equal name name_check then S_expr (String name, value)
      else look_up next name
  | _ -> raise (WrongVariable "Something went wrong")

let pair_lis_frames parameters values =
  if get_exspression_length parameters == get_exspression_length values then
    S_expr (parameters, values)
  else raise (WrongVariable "Parameters and Values should be the same length")

let look_up_frames_col assoscation_list name =
  let (S_expr (parameters, values)) = assoscation_list in

  let rec look_up_frames_rec parameters values =
    match (parameters, values) with
    | Nil, Nil -> Nil
    | Nil, _ | _, Nil -> raise (WrongVariable "lists should be equale")
    | S_expr (String parameter, next_parameter), S_expr (value, next_value) ->
        if String.equal parameter name then S_expr (String parameter, value)
        else look_up_frames_rec next_parameter next_value
  in
  look_up_frames_rec parameters values

(* (define foo (lambda (x) (+ 1 x))) *)

let look_up_frames assoscation_list name =
  let rec look_up_frames_rec assoscation_list name =
    match assoscation_list with
    | Nil -> Nil (* raise (WrongVariable "This parameter does not exist") *)
    | S_expr (column, next_column) ->
        let res = look_up_frames_col column name in
        if res == Nil then look_up_frames_rec next_column name else res
    | _ -> raise (WrongVariable "something went wrong is look_up_frames_rec")
  in
  look_up_frames_rec assoscation_list name

let eval exp assoscations =
  (* print_exsprassion exp; *)
  let rec evaluate exp assoscation_list =
    Printf.printf "evaluate: ";
    print_exsprassion exp;
    match exp with
    | Nil -> Nil
    | Int x -> Int x
    | String name -> (
        match look_up_frames assoscation_list name with
        | Nil -> raise (WrongVariable "association wasnt found")
        | S_expr (_, value) ->
            Printf.printf "look up result: ";
            print_exsprassion value;
            value
        | _ -> String name)
    | S_expr
        ( String "if",
          S_expr (conditiion, S_expr (output1, S_expr (output2, Nil))) ) -> (
        match evaluate conditiion assoscation_list with
        | String "t" -> evaluate output1 assoscation_list
        | String "f" -> evaluate output2 assoscation_list
        | _ -> raise (WrongOperation "Conditiion didn't retune boolean"))
    | S_expr (String "cond", x) -> eval_cond x assoscation_list
    | S_expr (String "quote", S_expr (cond, Nil)) -> cond
    | S_expr (String "lambda", S_expr (_, S_expr (_, Nil))) as
      lambda_exspression ->
        Closure (lambda_exspression, assoscation_list)
    | S_expr (String operation, x) -> (
        match look_up_frames assoscation_list operation with
        | Nil -> apply (String operation) (eval_list x assoscation_list)
        | S_expr (_, value) ->
            Printf.printf "Value: ";
            print_exsprassion value;
            evaluate (S_expr (value, x)) assoscation_list)
    | S_expr (operation, x) ->
        Printf.printf "Operation: ";
        print_exsprassion operation;
        Printf.printf "exspression: ";
        print_exsprassion x;

        apply operation (eval_list x assoscation_list)
  (* apply operation (eval_list x assoscation_list) assoscation_list *)
  (* ((lambda (x y)(+ x y)) 1 2) *)
  and eval_list exp assoscation_list =
    (* Printf.printf "eval_list: "; *)
    (* print_exsprassion exp; *)
    match exp with
    | Nil -> Nil
    | S_expr (x, y) ->
        S_expr (evaluate x assoscation_list, eval_list y assoscation_list)
    | _ -> raise (WrongVariable "List can not be anything but S_expr")
  and eval_cond exp assoscation_list =
    (* Printf.printf "eval_cond: "; *)
    (* print_exsprassion exp; *)
    match exp with
    | Nil -> Nil
    | S_expr (S_expr (cond, S_expr (res, Nil)), next) -> (
        (* Printf.printf "cond -> "; *)
        (* print_exsprassion_full cond; *)
        (* Printf.printf "cond -> ";  *)
        (* print_exsprassion cond; *)
        match evaluate cond assoscation_list with
        | String "t" -> evaluate res assoscation_list
        | String "f" -> eval_cond next assoscation_list
        | _ -> raise (WrongOperation "Conditiion is not true or false"))
    | _ -> raise (WrongOperation "Cond should have S_expr inside")
  and apply func exp =
    Printf.printf "func: ";
    print_exsprassion func;
    Printf.printf "apply: ";
    print_exsprassion exp;
    (* print_exsprassion exp; *)
    match (func, exp) with
    | String "+", S_expr (Int x, S_expr (Int y, Nil)) -> Int (x + y)
    | String "-", S_expr (Int x, S_expr (Int y, Nil)) -> Int (x - y)
    | String "*", S_expr (Int x, S_expr (Int y, Nil)) -> Int (x * y)
    | String "/", S_expr (Int x, S_expr (Int y, Nil)) -> Int (x / y)
    | String "cons", S_expr (x, S_expr (y, Nil)) -> S_expr (x, y)
    | String "car", S_expr (S_expr (res, _), Nil) -> res
    | String "cdr", S_expr (S_expr (_, res), Nil) -> res
    | String "list", x -> x
    | String "null", S_expr (Nil, Nil) -> String "t"
    | String "null", S_expr (_, Nil) -> String "f"
    | ( Closure
          ( S_expr
              (String "lambda", S_expr (parameters, S_expr (exspression, Nil))),
            new_association_list ),
        values ) -> (
        Printf.printf "exspression: ";
        print_exsprassion exspression;
        match parameters with
        | S_expr _ ->
            evaluate exspression
              (S_expr (pair_lis_frames parameters values, new_association_list))
        | _ ->
            evaluate exspression
              (S_expr
                 ( pair_lis_frames (S_expr (parameters, Nil)) values,
                   new_association_list ))
        (* Printf.printf "this ->"; *)
        (* print_exsprassion (S_expr ((pair_lis_frames parameters values), assoscation_list )); *)
        )
    | _ -> raise (WrongOperation "Error wrong exp")
  in

  evaluate exp assoscations

(* (+  ( + 1 2 ) 1) *)
(* apply(+,[3,1]) *)

(* car, cdr, list, quote, if, cond, cons, null *)

(* (list 1 2 3) *)
(* (1 2 3) *)
(* (cond (x y) (z q) ... (f g )) *)

(* (+ ( 2 3) 4)   (apply + ( /6 4)) *)

let get_first_string expression =
  match expression with
  | String x -> x
  | _ -> raise (WrongVariable "Exspression should be String")

let rec driver_loop exspression assoscation_list =
  match exspression with
  | Nil -> Printf.printf "The end =)\n"
  | S_expr (S_expr (String "define", S_expr (name, S_expr (lambda, Nil))), next)
    ->
      Printf.printf "Defined %s " (get_first_string name);
      print_exsprassion lambda;
      driver_loop next
        (S_expr
           ( pair_lis_frames
               (S_expr (name, Nil))
               (S_expr (eval lambda assoscation_list, Nil)),
             assoscation_list ))
  | S_expr (exp, next) ->
      print_exsprassion (eval exp assoscation_list);
      driver_loop next assoscation_list
  | _ -> raise (WrongVariable "IDK acttualy")

let rec driver_loop exspression assoscation_list =
  match exspression with
  | Nil -> Printf.printf "The end =)\n"
  | S_expr (S_expr (String "define", S_expr (name, S_expr (lambda, Nil))), next)
    ->
      Printf.printf "Defined %s " (get_first_string name);
      print_exsprassion lambda;
      driver_loop next
        (S_expr
           ( pair_lis_frames
               (S_expr (name, Nil))
               (S_expr (eval lambda assoscation_list, Nil)),
             assoscation_list ))
  | S_expr (exp, next) ->
      print_exsprassion (eval exp assoscation_list);
      driver_loop next assoscation_list
  | _ -> raise (WrongVariable "IDK acttualy")

let rec driver_loop_terminal assoscation_list =
  Printf.printf ">> ";
  let expression =
    try parsing (read_line ()) with
    | Failure msg ->
        Printf.printf "Error happened: %s\n" msg;
        driver_loop_terminal assoscation_list
    | _ ->
        Printf.printf "Unknown parsing error\n";
        driver_loop_terminal assoscation_list
  in
  try
    match expression with
    | Nil ->
        Printf.printf "The end =)\n";
        Nil
    | S_expr (String "define", S_expr (name, S_expr (lambda, Nil))) ->
        Printf.printf "Defined %s " (get_first_string name);
        print_exsprassion lambda;
        driver_loop_terminal
          (S_expr
             ( pair_lis_frames
                 (S_expr (name, Nil))
                 (S_expr (eval lambda assoscation_list, Nil)),
               assoscation_list ))
    | exp ->
        print_exsprassion (eval exp assoscation_list);
        driver_loop_terminal assoscation_list
  with
  | WrongVariable msg ->
      Printf.printf "Error: %s\n" msg;
      driver_loop_terminal assoscation_list
  | Failure msg ->
      Printf.printf "Error during evaluation: %s\n" msg;
      driver_loop_terminal assoscation_list
  | _ ->
      Printf.printf "Unknown error occurred\n";
      driver_loop_terminal assoscation_list
(* | _ -> raise (WrongVariable "IDK acttualy");; *)
