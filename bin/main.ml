[@@@ocaml.warning "-32"]

open Language.Peano
open Language.Tokenize
open Language.Rutalg


let () =
  
  Printf.printf "Testing Peano Numbers:\n";

  let five_peano = int_to_peano 5 in
  let three_peano = int_to_peano 3 in

  let sum = add five_peano three_peano in
  Printf.printf "5 + 3 = %d\n" (peano_to_int sum);

  let diff = sub five_peano three_peano in
  Printf.printf "5 - 3 = %d\n" (peano_to_int diff);

  Printf.printf "5 > 3 = %b\n" (greater five_peano three_peano);
  Printf.printf "5 < 3 = %b\n" (less five_peano three_peano);
  Printf.printf "5 = 3 = %b\n\n" (eq five_peano three_peano);
  
  let example = "( A + ( B * C) )" in
  let tokens = tokenize example in
  let rutaltest = rutal tokens in
  List.iter (Printf.printf "'%d' ") rutaltest; print_newline ()
    





(* "(abc (de 1) 2)" -> ["(","abc","(","de","1",")","2",")"] *)

        (* | S_expr (x, String y) ->   *)
        (*                     Printf.printf "( "; *)
        (*                     print_exp_par x; *)
        (*                     Printf.printf ". "; *)
        (*                     print_exp (String y ); *)
        (*                     Printf.printf ") " *)
        (**)
 
        (* | S_expr (x, String y) ->   *)
        (*                     print_exp_par x; *)
        (*                     Printf.printf ". "; *)
        (*                     print_exp (String y ); *)
        (**)



(* let three = Successor (Successor (Successor Zero)) *)

(* let (&&) x y = *)
(*   match (x, y) with *)
(*   | true, true -> true *)
(*   | _, _ -> false;; *)




(* [@@@ocaml.warning "-27"] *)
(* 
   ( (1 2) 3 4 (4))
->
 (( 1 . (2 . nil)) . (3. (4. (( 4 . nill) . nil ) 

(1 . 3) -> (1 . 3)

(1 . (3 . nil)) -> (1 3)

(1 . (2 . (3 . nil)))  
   == (1 2 3) 
*)
(*          *)

(* v(s (d) a) == (s . ((d .nil) . (a . nil))) *)
(**)
(*     (list  1 2 3 4 5) = (list (1 . (2 . (3 . ...)))) *)
(**)
(* (1 . (2 . 3)) == (1 2 . 3) *)
(**)
(* (1 . (2 . nil)) == (1 2) *)
(**)
(* (a . b) *)

(* let () = *)
(*   print_endline "Hello, world!"; *)
(*   (* Here you can use your library's functionality, e.g., Peano and Tokenizer modules *) *)
(*   let tokenized = Your_lib_name.Tokenizer.tokenize "This is an example." in *)
(*   List.iter print_endline tokenized *)
(**)
(* let exp = S_expr (S_expr( (Int 10), (S_expr ((String "test") , (Int 1) ) ) ), S_expr ((String "aaaa") , (Int 44)) ) ;; *)
(* let test = S_expr ((String "+"), (S_expr ((Int 1), (Int 1)) ));;   *)
(*  (* поддержка под выражений  *) *)
(**)
(**)
(**)
(**)
(*  Printf.printf "res = '%s'\n" (exp_to_string exp) ;; *)


