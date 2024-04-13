[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]

open Peano
open Tokenize 
open Parsing 
open Mylisplang

let dir_location = "codefolder/"

let file_standard = "testcode.ls"
(* open Language.Rutalg *)
(* exception Error of string *)

let rec conc ls1 ls2 = 
  match (ls1, ls2) with
  | ([], ls2) -> ls2
  | ( (hd :: tail), ls2 ) -> hd ::(  conc tail  (ls2) )

let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let test = S_expr ((String "+"), (S_expr ((Int 7), (S_expr (Int 1, Nil) )) ));;  

let nil_test = S_expr ( Nil, Nil);; 

let array = [1;2;3;4];;
let example_small_cond = "(cond ( (null (list)) (+ 1 2)) ( t (list 2 3 4 5)) ) " ;;  
let example_big_cond = "( cond ( (null (list  1 2 3 )) (+ 1 1) ) ( (null (list 2 3 )) 5  )  (t (+ 1 0) ) ) ";;  
let example_null = "(null (list ) ) " ;; 
 

let get_contents_from_file = 
  Printf.printf "Number of arguments: %d\n" (Array.length Sys.argv - 1);
  read_file ( dir_location ^ if Array.length Sys.argv - 1 == 0 then file_standard else Sys.argv.(1))

let () =  
  let contents = get_contents_from_file in
  Printf.printf  "%s\n" contents 

(* let example = "( (lambda (x y) ( + ((lambda (x y) (+ x y) ) 1 2 ) (+ x y) ) ) (+ 3 1) 4)" in *)
(*    let  a = parsing example in *)
(*     print_exsprassion (eval a); *)
