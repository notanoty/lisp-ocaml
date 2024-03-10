type peano_number = Zero | Successor of peano_number;;


type sexpr = 
    | Nil 
    | Int of int
    | String of string
    | S_expr of sexpr * sexpr;;


let exp = S_expr (S_expr( (Int 10), (S_expr ((String "test") , (Int 1) ) ) ), S_expr ((String "aaaa") , (Int 44)) ) ;;
let test = S_expr ((String "+"), (S_expr ((Int 1), (Int 1)) ));;  
 (* поддержка под выражений  *)


(* (1 2 3 4) (1 . (2 . (3 . (4 . nil)))) *)
let print_exsprassion exsprassion =
  let rec print_exp_par =  function
          | Nil -> Printf.printf "Nil"
          | Int x -> Printf.printf "%d " x 
          | String x ->  Printf.printf "%s " x        
          | S_expr (x, (S_expr (y,z ) )) ->   
                              Printf.printf "( ";
                              print_exp_par x;
                              print_exp (S_expr (y,z));
                              Printf.printf ") "
          | S_expr (x,  Nil) ->  
                              Printf.printf "( ";
                              print_exp_par x;
                              Printf.printf ") "

          | S_expr (x,  y) ->  
                              Printf.printf "( ";
                              print_exp_par x;
                              Printf.printf ". ";
                              print_exp y ;
                              Printf.printf ") "

     and print_exp = function
          | Nil -> Printf.printf "test "
          | Int x -> Printf.printf "%d " x 
          | String x ->  Printf.printf "%s " x
          | S_expr (x, (S_expr (y, z)) ) ->  
                              print_exp_par x;
                              print_exp (S_expr (y,z))
          | S_expr (x,  Nil) ->  
                              print_exp_par x;
          | S_expr (x, y) ->  
                              print_exp_par x;
                              Printf.printf ". ";
                              print_exp  y 
      in
  print_exp_par exsprassion ;
  Printf.printf "\n";;

let print_exsprassion_full exsprassion =
  let rec print_exp_par =  function
          | Nil -> Printf.printf "Nil"
          | Int x -> Printf.printf "(Int %d )" x 
          | String x ->  Printf.printf "(String '%s') " x        
          | S_expr (x, (S_expr (y,z ) )) ->   
                              Printf.printf "(S_expr ";
                              print_exp_par x;
                              print_exp_par (S_expr (y,z));
                              Printf.printf ") "
          | S_expr (x,  Nil) ->  
                              Printf.printf "(S_expr ";
                              print_exp_par x;
                              Printf.printf ") "

          | S_expr (x,  y) ->  
                              Printf.printf "(S_expr ";
                              print_exp_par x;
                              Printf.printf ". ";
                              print_exp_par y ;
                              Printf.printf ") ";
      in
  print_exp_par exsprassion ;
  Printf.printf "\n";;


let rec exp_to_string = function
        | Nil -> "Nil "
        | Int x -> Printf.sprintf "%d " x 
        | String x ->  Printf.sprintf "%s " x
        | S_expr (x, (S_expr (y,z ) )) ->   "( " ^ exp_to_string x ^ exp_to_string_no_par (S_expr (y,z)) ^ ") "
        | S_expr (x,  Nil) ->  "( " ^ exp_to_string x ^ ") "
        | S_expr (x,  y) ->  "( " ^ exp_to_string x ^ ". " ^ exp_to_string_no_par y ^ ") "
        
        and  exp_to_string_no_par = function
        | Nil -> "Nil "
        | Int x -> Printf.sprintf "%d " x 
        | String x ->  Printf.sprintf "%s " x
        | S_expr (x, (S_expr (y,z ) )) ->  exp_to_string x ^ exp_to_string_no_par (S_expr (y,z)) 
        | S_expr (x,  Nil) ->  exp_to_string x 
        | S_expr (x,  y) ->  exp_to_string x ^ ". " ^ exp_to_string_no_par y ;;      

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


let rec list_string_to_exp list = 
    match list with
    | [] -> Nil
    | hd::[] -> S_expr ((String hd),  Nil)
    | hd::tail -> S_expr ((String hd),  list_string_to_exp tail)

(*  ("aasdas ", "bbbb". "cccc") *)

 (* S_expr (String "aasdas", (S_expr (String "bbbb",  Nil *)
  (* (S_expr  (String "cccc", nil))) *)
