[@@@ocaml.warning "-8"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]

open Peano
open Tokenize
open Parsing
open Mylisplang

(* let exmple = ref (SS_expr (SInt (ref 1), SNil)) *)
let int_expr = SInt { value = 42 }
let string_expr = SString { value = "hello" }
let s_expr = S_expr { current = int_expr; next = string_expr }
let closure_expr = Closure { env = Nil; code = s_expr }

let () =
  match s_expr with S_expr v -> v.current <- SInt { value = 50 } | _ -> ()

(* s_expr.current <- string_expr;; *)

(* let modify_current s_expr = *)
(*   match s_expr with *)
(*   | S_expr { current; next = _ } -> current := SInt { value = 22 } *)
(*   | _ -> () (* Modifying mutable fields *) *)

(* let () = match int_expr with SInt v -> v.value <- 43 | _ -> ();; *)

(* print_exsprassion s_expr;; *)
(* print_exsprassion_full s_expr;; *)
(* Printf.printf "(%s)\n" (exp_to_string s_expr);; *)
(* Printf.printf "len %d\n" (get_exspression_length s_expr);; *)
(* print_exsprassion (parsing "( define a 10)") *)
let () =
  if Array.length Sys.argv == 2 then (
    let read_file file = In_channel.with_open_bin file In_channel.input_all in
    let contents = "(" ^ read_file Sys.argv.(1) ^ ")" in
    let parsing_result = parsing contents in
    print_exsprassion parsing_result;
    driver_loop parsing_result Nil)
  else ignore (driver_loop_terminal Nil)
