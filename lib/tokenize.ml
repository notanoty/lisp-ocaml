
let cut_out str start_ind end_ind =
  if start_ind < 0 || end_ind >= String.length str || start_ind > end_ind then
    invalid_arg "Invalid indices"
  else
    String.sub str start_ind (end_ind - start_ind + 1);;

let char_to_string c = String.make 1 c

let tokenize str = 
  let len = String.length str in
  let rec token start_ind i res =
    if i >= len then
      if start_ind < len then cut_out str start_ind (len - 1) :: res
      else res
    else
      match str.[i] with
      | '(' | ')' as par->
        if start_ind < i then
          token (i + 1) (i + 1) ( char_to_string par :: (cut_out str start_ind (i - 1)  :: res ) )
        else
          token (i + 1) (i + 1) ( char_to_string par :: res)
      | ' ' ->
        if start_ind < i then
          token (i + 1) (i + 1) (cut_out str start_ind (i - 1) :: res)
        else
          token (i + 1) (i + 1) res
      | _ -> token start_ind (i + 1) res
  in
  List.rev (token 0 0 [])

