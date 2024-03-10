open Peano
open Tokenize

(* open Language.Rutalg *)
let find_max_int ls =  
  let rec find_max list max = 
      match (list) with
      | [] -> 0
      | (hd::[]) -> if hd > max then hd else max
      | (hd::tail) -> find_max tail (if hd > max then hd else max)
  in
  find_max ls 0;;

let string_to_sexpr str =
  list_string_to_exp (tokenize str);;

let rutal_original list = 
  let len = List.length list in 
  let rec rutishauser_algorithm i depth = 
    if i >= len then []
    else
      match (List.nth list i) with
        | ")" | "+" | "-" | "*" | "/" ->  ((depth - 1) :: ( rutishauser_algorithm  (i + 1) (depth - 1)) )
        | _ ->  ((depth + 1) :: ( rutishauser_algorithm  (i + 1) (depth + 1)) )    in 
  rutishauser_algorithm 0 0;; 


let rutal list = 
  let len = List.length list in 
  let rec rutishauser_algorithm i depth = 
    if i >= len then []
    else
      match (List.nth list i) with
        | ")"  ->  (depth :: ( rutishauser_algorithm  (i + 1) (depth - 1)) )
        | "(" ->  ((depth + 1) :: ( rutishauser_algorithm  (i + 1) (depth + 1)) )
        | _ -> (-1 :: ( rutishauser_algorithm  (i + 1) depth) )
    in 
  rutishauser_algorithm 0 0;; 

(* [Int] *)
(* List.hd x + List.hd (List.tl x) *)

(* let get_number number =  *)
(*   match number with *)
(*   | Int x -> x *)
(*   | _ -> -10000 *)
(**)
(* let rec eval exp =  *)
(*   match exp with *)
(*   | Nil -> String "Error Nil" *)
(*   | Int x-> Int x *)
(*   | String _ -> String "Error string" *)
(*   | S_expr( (String "+"),S_expr ((Int x), (S_expr (Int y, Nil)))) -> *)
(*       Int (get_number (eval (Int x)) + get_number (eval (Int y))) *)
(*   | _ -> (String "Error wrong exp");; *)





(* (+ 1 2 . 4) *)

(* car, cdr, list, quote, if, cond, cons, null *)





(* let parsing list list_depth =  *)
    (* let max = find_max_int list_depth in  *)
    (* let make_list ls ls_depth = *)
      (* match (ls ls_depth) with *)
      (* |( (hd_ls:: tail_ls) (max :: tail_ls_d)) ->   *)

 (* let parsing exp list_depth = *)
 (*  let max_depth = find_max_int list_depth in *)
 (*  let rec make_list ls ls_depth exp_biuld ls_d_build = match (ls, ls_depth) with *)
 (*    | (_, []) -> []  *)
 (*    | ((S_expr (hd, tail) ), (max_depth::tail_ls_d)) -> [] *)
 (*    | ((S_expr (hd, tail) ) , (hd_ls_d::tail_ls_d)) ->  *)
 (*        ( hd ::(make_list tail_ls_d))  *)
 (*  and create_list ls ls_depth  = match (ls, ls_depth) with  *)
 (*    | ((S_expr (hd, tail) ), max_depth::tail_ls_d) ) -> [] *)
 (*    | (S_expr (hd, tail) ) , hd_ls_d::tail_ls_d ) -> *)
 (*        ( hd ::(make_list tail_ls_d))  *)
 (*  in *)
 (*  make_list exp list_depth [] [] *)


 
(**)
(* let parsing list list_depth = *)
(*   let max_depth = find_max_int list_depth in *)
(*   let rec make_list ls ls_depth = match (ls, ls_depth) with *)
(*     | ([], _) | (_, []) -> []  *)
(*     | (hd_ls::tail_ls, max_depth::tail_ls_d) -> [] *)
(*     | (hd_ls::tail_ls, hd_ls_d::tail_ls_d) -> *)
(*         ( hd_ls ::(make_list tail_ls tail_ls_d))  *)
(*   and create_list ls ls_depth  = match (ls, ls_depth) with  *)
(*     | ([], _) | (_, []) -> [] *)
(*     | (hd_ls::tail_ls, max_depth::tail_ls_d) -> [] *)
(*     | (hd_ls::tail_ls, _ ::tail_ls_d) ->  *)
(*   in *)
(*    make_list list list_depth *)














