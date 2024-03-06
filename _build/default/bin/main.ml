[@@@ocaml.warning "-32"]

(* [@@@ocaml.warning "-27"] *)
(* 
   ( (1 2) 3 4 (4))
->
 (( 1 . (2 . nil)) . (3. (4. (( 4 . nill) . nil ) ))



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



type peano_number = Zero | Successor of peano_number;;




type sexpr = 
    | Int of int
    | String of string
    | S_expr of sexpr * sexpr;;


let exp = S_expr (S_expr( (Int 10), (S_expr ((String "test") , (Int 1) ) ) ), S_expr ((String "aaaa") , (Int 44)) ) ;;
let test = S_expr ((String "+"), (S_expr ((Int 1), (Int 1)) ));;  
 (* поддержка под выражений  *)

let rec print_exp_par =  function


        | Int x -> Printf.printf "%d " x 
        | String x ->  Printf.printf "%s " x        | S_expr (x, (S_expr (y,z ) )) ->   
                            Printf.printf "( ";
                            print_exp_par x;
                            print_exp (S_expr (y,z));
                            Printf.printf ") "
        | S_expr (x,  y) ->  
                            Printf.printf "( ";
                            print_exp_par x;
                            Printf.printf ". ";
                            print_exp y ;
                            Printf.printf ") "

   and print_exp = function
        | Int x -> Printf.printf "%d " x 
        | String x ->  Printf.printf "%s " x
        | S_expr (x, (S_expr (y, z)) ) ->  
                            print_exp_par x;
                            print_exp (S_expr (y,z))
        | S_expr (x, y) ->  
                            print_exp_par x;
                            Printf.printf ". ";
                            print_exp  y ;;


let rec exp_to_string = function
        | Int x -> Printf.sprintf "%d " x 
        | String x ->  Printf.sprintf "%s " x
        | S_expr (x, (S_expr (y,z ) )) ->   "( " ^ exp_to_string x ^ exp_to_string_no_par (S_expr (y,z)) ^ ") "
        | S_expr (x,  y) ->  "( " ^ exp_to_string x ^ ". " ^ exp_to_string_no_par y ^ ") "
        and  exp_to_string_no_par = function
        | Int x -> Printf.sprintf "%d " x 
        | String x ->  Printf.sprintf "%s " x
        | S_expr (x, (S_expr (y,z ) )) ->  exp_to_string x ^ exp_to_string_no_par (S_expr (y,z)) 
        | S_expr (x,  y) ->  exp_to_string x ^ ". " ^ exp_to_string_no_par y ;;      


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


 Printf.printf "res = '%s'\n" (exp_to_string exp) ;;
(* Printf.printf "\n";; *)
(* let a = Printf.sprintf "%d <- res" 10;; *)

  (* Printf.printf "%s -< res" a;; *)
(* (1 aaa bbb) *)
(* (10 . aaaa) *)

let three = Successor (Successor (Successor Zero))

let (&&) x y =
  match (x, y) with
  | true, true -> true
  | _, _ -> false;;



let rec peano_to_int = function
  | Zero -> 0
  | Successor x -> 1 + (peano_to_int x);;

let rec int_to_peano = function
  | 0 -> Zero
  | x -> (Successor (int_to_peano (x - 1)  ));;

let rec add x y = 
    match (x, y) with
    | x, Zero -> x
    | x,  Successor y -> add (Successor x) y;;

let rec sub x y = 
    match (x, y) with
    | x, Zero -> x
    | Zero, Successor _ -> Zero
    | Successor x,  Successor y -> sub x y;;

let rec greater x y = 
    match (x, y) with
    | Zero, Zero -> false
    | Successor _, Zero -> true
    | Zero, Successor _ -> false
    | Successor x, Successor y -> greater x y;;


let rec less x y = 
    match (x, y) with
    | Zero, Zero -> false
    | Successor _, Zero -> false
    | Zero, Successor _ -> true
    | Successor x, Successor y -> less x y;;

let rec eq x y = 
    match (x, y) with
    | Zero, Zero -> true
    | Successor _, Zero -> false
    | Zero, Successor _ -> false
    | Successor x, Successor y -> eq x y;;


