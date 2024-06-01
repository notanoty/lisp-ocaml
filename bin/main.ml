[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]

open Peano
open Tokenize
open Parsing
open Mylisplang

let rec conc ls1 ls2 =
  match (ls1, ls2) with
  | [], ls2 -> ls2
  | head :: tail, ls2 -> head :: conc tail ls2

let () =
  if Array.length Sys.argv == 2 then (
    let read_file file = In_channel.with_open_bin file In_channel.input_all in
    let contents = "(" ^ read_file Sys.argv.(1) ^ ")" in
    let parsing_result = parsing contents in
    print_exsprassion parsing_result;
    driver_loop parsing_result Nil)
  else ignore (driver_loop_terminal Nil)
