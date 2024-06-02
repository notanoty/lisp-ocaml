(* [@@@ocaml.warning "-8"] *)
type peano_number = Zero | Successor of peano_number

type sexpr =
  | Nil
  | Int of int
  | String of string
  | S_expr of sexpr * sexpr
  | Closure of sexpr * sexpr

type s_expression =
  | Nil
  | SInt of { mutable value : int }
  | SString of { mutable value : string }
  | S_expr of { mutable current : s_expression; mutable next : s_expression }
  | Closure of { mutable env : s_expression; mutable code : s_expression }

let example = S_expr { current = SString { value = "aa" }; next = Nil }
(*
let exp =
  ref (S_expr
    ( ref (S_expr ( ref (Int 10), ref (S_expr ( ref (String "test"), ref( Int 1))))),
      ref (S_expr ( ref (String "aaaa"), ref (Int 44))) )) *)

(* let test = S_expr (String "+", S_expr (Int 1, Int 1)) *)

(* поддержка под выражений  *)

(* (1 2 3 4) (1 . (2 . (3 . (4 . nil)))) *)
let print_exsprassion (exsprassion : s_expression) =
  let rec print_exp_par = function
    | Nil -> Printf.printf "Nil"
    | SInt { value } -> Printf.printf "%d " value
    | SString { value } -> Printf.printf "'%s' " value
    | S_expr { current; next = S_expr { current = y; next = z } } ->
        Printf.printf "( ";
        print_exp_par current;
        print_exp (S_expr { current = y; next = z });
        Printf.printf ") "
    | S_expr { current; next = Nil } ->
        Printf.printf "( ";
        print_exp_par current;
        Printf.printf ") "
    | S_expr { current; next } ->
        Printf.printf "( ";
        print_exp_par current;
        Printf.printf ". ";
        print_exp next;
        Printf.printf ") "
    | Closure _ -> Printf.printf "Closure"
  and print_exp = function
    | Nil -> Printf.printf "Nil"
    | SInt { value } -> Printf.printf "%d " value
    | SString { value } -> Printf.printf "'%s' " value
    | S_expr { current; next = S_expr { current = y; next = z } } ->
        print_exp_par current;
        print_exp (S_expr { current = y; next = z })
    | S_expr { current; next = Nil } -> print_exp_par current
    | S_expr { current; next } ->
        print_exp_par current;
        Printf.printf ". ";
        print_exp next
    | Closure _ -> Printf.printf "Closure"
  in

  print_exp_par exsprassion;
  Printf.printf "\n"

let print_exsprassion_full (exsprassion : s_expression) =
  let rec print_exp_par = function
    | Nil -> Printf.printf "Nil"
    | SInt { value } -> Printf.printf "Int %d " value
    | SString { value } -> Printf.printf "String \"%s\" " value
    | S_expr { current; next = S_expr { current = y; next = z } } ->
        Printf.printf "S_expr (";
        Printf.printf "(";
        print_exp_par current;
        Printf.printf "), (";
        print_exp_par (S_expr { current = y; next = z });
        Printf.printf ")) "
    | S_expr { current; next } ->
        Printf.printf "S_expr (";
        print_exp_par current;
        Printf.printf ", ";
        print_exp_par next;
        Printf.printf ") "
    | Closure _ -> Printf.printf "(Closure)"
  in
  print_exp_par exsprassion;
  Printf.printf "\n"

let rec exp_to_string = function
  | Nil -> "Nil "
  | SInt { value } -> Printf.sprintf "%d " value
  | SString { value } -> Printf.sprintf "\"%s\" " value
  | S_expr { current; next = S_expr { current = y; next = z } } ->
      "( " ^ exp_to_string current
      ^ exp_to_string_no_par (S_expr { current = y; next = z })
      ^ ") "
  | S_expr { current; next = Nil } -> "( " ^ exp_to_string current ^ ") "
  | S_expr { current; next } ->
      "( " ^ exp_to_string current ^ ". " ^ exp_to_string_no_par next ^ ") "
  | Closure _ -> "(Closure)"

and exp_to_string_no_par = function
  | Nil -> "Nil "
  | SInt { value } -> Printf.sprintf "%d " value
  | SString { value } -> Printf.sprintf "\"%s\" " value
  | S_expr { current; next = S_expr { current = y; next = z } } ->
      (* | S_expr (x, S_expr (y, z)) -> *)
      exp_to_string current
      ^ exp_to_string_no_par (S_expr { current = y; next = z })
  | S_expr { current; next = Nil } -> exp_to_string current
  | S_expr { current; next } ->
      exp_to_string current ^ ". " ^ exp_to_string_no_par next
  | Closure _ -> "Closure"

let rec get_exspression_length (exsprassion : s_expression) : int =
  match exsprassion with
  | Nil -> 0
  | SInt _ -> 1
  | SString _ -> 1
  | S_expr { current = _; next } -> 1 + get_exspression_length next
  | Closure _ -> 1

let rec peano_to_int = function Zero -> 0 | Successor x -> 1 + peano_to_int x

let rec int_to_peano = function
  | 0 -> Zero
  | x -> Successor (int_to_peano (x - 1))

let rec add x y =
  match (x, y) with x, Zero -> x | x, Successor y -> add (Successor x) y

let rec sub x y =
  match (x, y) with
  | x, Zero -> x
  | Zero, Successor _ -> Zero
  | Successor x, Successor y -> sub x y

let rec greater x y =
  match (x, y) with
  | Zero, Zero -> false
  | Successor _, Zero -> true
  | Zero, Successor _ -> false
  | Successor x, Successor y -> greater x y

let rec less x y =
  match (x, y) with
  | Zero, Zero -> false
  | Successor _, Zero -> false
  | Zero, Successor _ -> true
  | Successor x, Successor y -> less x y

let rec eq x y =
  match (x, y) with
  | Zero, Zero -> true
  | Successor _, Zero -> false
  | Zero, Successor _ -> false
  | Successor x, Successor y -> eq x y

let rec list_string_to_exp list_args : s_expression =
  match list_args with
  | [] -> Nil
  | head :: [] -> S_expr { current = SString { value = head }; next = Nil }
  | head :: tail ->
      S_expr
        { current = SString { value = head }; next = list_string_to_exp tail }

(*  ("aasdas ", "bbbb". "cccc") *)

(* S_expr (String "aasdas", (S_expr (String "bbbb",  Nil *)
(* (S_expr  (String "cccc", nil))) *)
