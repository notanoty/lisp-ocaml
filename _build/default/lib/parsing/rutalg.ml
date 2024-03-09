let rutal list = 
  let len = List.length list in 
  let rec rutishauser_algorithm i depth = 
    if i >= len then []
    else
      match (List.nth list i) with
        | ")" | "+" | "-" | "*" | "/" ->  ((depth - 1) :: ( rutishauser_algorithm  (i + 1) (depth - 1)) )
        | _ ->  ((depth + 1) :: ( rutishauser_algorithm  (i + 1) (depth + 1)) )
    in 
  rutishauser_algorithm 0 0;; 

let rutal2 list = 
  let len = List.length list in 
  let rec rutishauser_algorithm i depth = 
    if i >= len then []
    else
      match (List.nth list i) with
        | ")"  ->  (depth :: ( rutishauser_algorithm  (i + 1) (depth - 1)) )
        | "(" ->  ((depth + 1) :: ( rutishauser_algorithm  (i + 1) (depth + 1)) )
        | _ -> (depth :: ( rutishauser_algorithm  (i + 1) depth) )

    in 
  rutishauser_algorithm 0 0;; 

(* ( + 1 ( + 1 1 )  )  *)
(* 1 1 1 2 2 2 2 2 1 *)
