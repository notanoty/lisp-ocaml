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

