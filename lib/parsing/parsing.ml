open Peano
open Tokenize

exception WrongVariable of string

exception LispError of string

(* open Language.Rutalg *)
let find_max_int current_expr =  
  let rec find_max list max = 
      match (list) with
      | [] -> 0
      | (hd::[]) -> if hd > max then hd else max
      | (hd::tail) -> find_max tail (if hd > max then hd else max)
  in
  find_max current_expr 0;;

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


let strint_to_expr_int = function
  | String s -> (
      match int_of_string_opt s with
      | Some i -> Int i
      | None -> String s
    )
  | x -> x

let reverse_expression2 exp  = 
  
  let rec rev exspression  new_expr = 
    match exspression  with
    | Nil -> new_expr
    
    | String x -> String x
    | Int x  -> Int x
    | S_expr (head_expr, tail_expr) ->  
      rev tail_expr ( S_expr ( (strint_to_expr_int head_expr), new_expr) ) 
  in
   rev exp Nil  

let rec build_list current_expr current_depths expr_accumulator depth_accumulator new_expr max_depth= 
    (* Printf.printf "build_list - "; *)
    (* print_exsprassion_full current_expr; *)
    match (current_expr, current_depths) with  
      | (_, []) -> raise (LispError "Parenthesis are not closed")
      | (S_expr (head_expr, tail_expr), (current_depth :: tail_depths)) ->
          if current_depth = max_depth then
             ( tail_expr,  reverse_expression2 new_expr , -1::tail_depths)
          else
            build_list tail_expr tail_depths expr_accumulator depth_accumulator ( S_expr (head_expr, new_expr)) max_depth  
      | _ -> raise (WrongVariable "This expression cannot be supported")
 




 let parsing_par expression_list list_depth =
  let max_depth = find_max_int list_depth in
  let rec build_expression current_expr current_depths expr_accumulator depth_accumulator = 
    (* Printf.printf "build_expression - "; *)
    (* print_exsprassion_full current_expr; *)
    match (current_expr, current_depths) with
    | (_, []) -> (expr_accumulator, depth_accumulator)
    | (Nil, _) -> (expr_accumulator, depth_accumulator)
    | (S_expr (head_expr, tail_expr), (current_depth :: tail_depths)) ->
        if current_depth = max_depth then
            let (new_tail, new_expr_accumulator, new_depth_accumulator) = build_list tail_expr tail_depths expr_accumulator depth_accumulator Nil max_depth in
            build_expression Nil new_depth_accumulator (S_expr (new_expr_accumulator, new_tail)) new_depth_accumulator
        else
            let (new_expr_accumulator, new_depth_accumulator) = build_expression tail_expr tail_depths expr_accumulator depth_accumulator  in
            (S_expr (head_expr, new_expr_accumulator), current_depth :: new_depth_accumulator )
    | _ -> raise (WrongVariable "This expression cannot be supported")

 in
  build_expression expression_list list_depth Nil []


let parsing exspression = 
    let depth_list  = (rutal ( tokenize exspression) ) in 
    let rec parsing_rec expression_list list_depth  =
      match list_depth with
      | [-1] -> expression_list
      | _ ->
      let (new_exspression, new_depth) = parsing_par expression_list list_depth in
        parsing_rec new_exspression new_depth
    in
  let res_expression = parsing_rec (string_to_sexpr exspression) depth_list  in
  match res_expression with
  | S_expr (x , _) -> x
  | x -> x

(* (+ 1 2 . 4) *)

(* car, cdr, list, quote, if, cond, cons, null *)





(* let parsing list list_depth =  *)
    (* let max = find_max_int list_depth in  *)
    (* let build_expression current_expr current_depths = *)
      (* match (current_expr current_depths) with *)
      (* |( (head_expr:: tail_expr) (max :: tail_depths)) ->   *)


 
(**)
(* let parsing list list_depth = *)
(*   let max_depth = find_max_int list_depth in *)
(*   let rec build_expression current_expr current_depths = match (current_expr, current_depths) with *)
(*     | ([], _) | (_, []) -> []  *)
(*     | (head_expr::tail_expr, max_depth::tail_depths) -> [] *)
(*     | (head_expr::tail_expr, hd_ls_d::tail_depths) -> *)
(*         ( head_expr ::(build_expression tail_expr tail_depths))  *)
(*   and build_list current_expr current_depths  = match (current_expr, current_depths) with  *)
(*     | ([], _) | (_, []) -> [] *)
(*     | (head_expr::tail_expr, max_depth::tail_depths) -> [] *)
(*     | (head_expr::tail_expr, _ ::tail_depths) ->  *)
(*   in *)
(*    build_expression list list_depth *)













