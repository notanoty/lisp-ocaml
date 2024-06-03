[@@@ocaml.warning "-8"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]

open Peano
open Tokenize
open Parsing

exception WrongVariable of string
exception WrongOperation of string
exception ParsingError of string

let get_number : s_expression -> int = function
  | SInt { value } -> value
  | _ -> raise (WrongVariable "Parameter should be of type Int a ")

let rec pair_lis (parameters : s_expression) (values : s_expression) :
    s_expression =
  match (parameters, values) with
  | Nil, Nil -> Nil
  | Nil, _ | _, Nil -> raise (WrongVariable "lists should be equale")
  | ( S_expr { current = name; next = next_name },
      S_expr { current = value; next = next_value } ) ->
      S_expr
        {
          current = S_expr { current = name; next = value };
          next = pair_lis next_name next_value;
        }

(* let rec look_up (assoscation_list : s_expression) (name : string) : s_expression *)
(*     = *)
(*   match assoscation_list with *)
(*   | Nil -> Nil *)
(*   (* | S_expr { current = (S_expr { SString {value = name_check}; next = value}); next } -> *) *)
(*   | S_expr { current = S_expr { current = (SString { value = name_check }); next = value }; next } *)
(*      if String.equal name name_check then S_expr { current = (SString {value = name}); next = value} *)
(*       else look_up next name *)
(*   | _ -> raise (WrongVariable "Something went wrong") *)

let rec look_up (association_list : s_expression) (name : string) : s_expression
    =
  match association_list with
  | Nil -> Nil
  | S_expr
      {
        current =
          S_expr { current = SString { value = name_check }; next = value };
        next;
      } ->
      if String.equal name name_check then
        S_expr { current = SString { value = name }; next = value }
      else look_up next name
  | _ -> raise (Failure "Something went wrong")

let pair_lis_frames (parameters : s_expression) (values : s_expression) :
    s_expression =
  if get_exspression_length parameters == get_exspression_length values then
    S_expr { current = parameters; next = values }
  else raise (WrongVariable "Parameters and Values should be the same length")

let look_up_frames_col (assoscation_list : s_expression) (name : string) :
    s_expression =
  let (S_expr { current = parameters; next = values }) = assoscation_list in

  let rec look_up_frames_rec parameters values =
    match (parameters, values) with
    | Nil, Nil -> Nil
    | Nil, _ | _, Nil -> raise (WrongVariable "lists should be equale")
    | ( S_expr { current = SString { value = parameter }; next = next_parameter },
        S_expr { current = value; next = next_value } ) ->
        if String.equal parameter name then
          S_expr { current = SString { value = parameter }; next = value }
        else look_up_frames_rec next_parameter next_value
  in
  look_up_frames_rec parameters values

let look_up_frames (assoscation_list : s_expression) (name : string) :
    s_expression =
  let rec look_up_frames_rec assoscation_list name =
    match assoscation_list with
    | Nil -> Nil (* raise (WrongVariable "This parameter does not exist") *)
    | S_expr { current = column; next = next_column } ->
        let res = look_up_frames_col column name in
        if res == Nil then look_up_frames_rec next_column name else res
    | _ -> raise (WrongVariable "something went wrong is look_up_frames_rec")
  in
  look_up_frames_rec assoscation_list name

let eval (exp : s_expression) (assoscations : s_expression) : s_expression =
  (* print_exsprassion exp; *)
  let rec evaluate (exp : s_expression) (association_list : s_expression) :
      s_expression =
    Printf.printf "evaluate: ";
    print_exsprassion exp;
    match exp with
    | Nil -> Nil
    | SInt { value = x } -> SInt { value = x }
    | SString { value = name } -> (
        match look_up_frames association_list name with
        | Nil -> raise (Failure "association wasn't found")
        | S_expr { current = _; next = value } ->
            Printf.printf "look up result: ";
            print_exsprassion value;
            value
        | _ -> SString { value = name })
    | S_expr
        {
          current = SString { value = "if" };
          next =
            S_expr
              {
                current = condition;
                next =
                  S_expr
                    {
                      current = output1;
                      next = S_expr { current = output2; next = Nil };
                    };
              };
        } -> (
        match evaluate condition association_list with
        | SString { value = "t" } -> evaluate output1 association_list
        | SString { value = "f" } -> evaluate output2 association_list
        | _ -> raise (Failure "Condition didn't return boolean"))
    | S_expr { current = SString { value = "cond" }; next = x } ->
        eval_cond x association_list
    | S_expr
        {
          current = SString { value = "quote" };
          next = S_expr { current = cond; next = Nil };
        } ->
        cond
    | S_expr
        {
          current = SString { value = "lambda" };
          next =
            S_expr { current = _; next = S_expr { current = _; next = Nil } };
        } as lambda_expression ->
        Closure { env = lambda_expression; code = association_list }
    | S_expr { current = SString { value = operation }; next = x } -> (
        match look_up_frames association_list operation with
        | Nil ->
            apply (SString { value = operation }) (eval_list x association_list)
        | S_expr { current = _; next = value } ->
            Printf.printf "Value: ";
            print_exsprassion value;
            evaluate (S_expr { current = value; next = x }) association_list)
    | S_expr { current = operation; next = x } ->
        Printf.printf "Operation: ";
        print_exsprassion operation;
        Printf.printf "expression: ";
        print_exsprassion x;
        apply operation (eval_list x association_list)
  and eval_list (exp : s_expression) (association_list : s_expression) :
      s_expression =
    match exp with
    | Nil -> Nil
    | S_expr { current = x; next = y } ->
        S_expr
          {
            current = evaluate x association_list;
            next = eval_list y association_list;
          }
    | _ -> raise (Failure "List can not be anything but S_expr")
  and eval_cond (exp : s_expression) (association_list : s_expression) :
      s_expression =
    match exp with
    | Nil -> Nil
    | S_expr
        {
          current =
            S_expr
              { current = cond; next = S_expr { current = res; next = Nil } };
          next;
        } -> (
        match evaluate cond association_list with
        | SString { value = "t" } -> evaluate res association_list
        | SString { value = "f" } -> eval_cond next association_list
        | _ -> raise (Failure "Condition is not true or false"))
    | _ -> raise (Failure "Cond should have S_expr inside")
  and apply (func : s_expression) (exp : s_expression) : s_expression =
    Printf.printf "func: ";
    print_exsprassion func;
    Printf.printf "apply: ";
    print_exsprassion exp;
    match (func, exp) with
    | ( SString { value = "+" },
        S_expr
          {
            current = SInt { value = x };
            next = S_expr { current = SInt { value = y }; next = Nil };
          } ) ->
        SInt { value = x + y }
    | ( SString { value = "-" },
        S_expr
          {
            current = SInt { value = x };
            next = S_expr { current = SInt { value = y }; next = Nil };
          } ) ->
        SInt { value = x - y }
    | ( SString { value = "*" },
        S_expr
          {
            current = SInt { value = x };
            next = S_expr { current = SInt { value = y }; next = Nil };
          } ) ->
        SInt { value = x * y }
    | ( SString { value = "/" },
        S_expr
          {
            current = SInt { value = x };
            next = S_expr { current = SInt { value = y }; next = Nil };
          } ) ->
        SInt { value = x / y }
    | ( SString { value = "cons" },
        S_expr { current = x; next = S_expr { current = y; next = Nil } } ) ->
        S_expr { current = x; next = y }
    | ( SString { value = "car" },
        S_expr { current = S_expr { current = res; next = _ }; next = Nil } ) ->
        res
    | ( SString { value = "cdr" },
        S_expr { current = S_expr { current = _; next = res }; next = Nil } ) ->
        res
    | SString { value = "list" }, x -> x
    | SString { value = "null" }, S_expr { current = Nil; next = Nil } ->
        SString { value = "t" }
    | SString { value = "null" }, S_expr { current = _; next = Nil } ->
        SString { value = "f" }
    | ( Closure
          {
            env =
              S_expr
                {
                  current = SString { value = "lambda" };
                  next =
                    S_expr
                      {
                        current = parameters;
                        next = S_expr { current = exspression; next = Nil };
                      };
                };
            code = new_association_list;
          },
        values ) -> (
        Printf.printf "exspression: ";
        print_exsprassion exspression;
        match parameters with
        | S_expr _ ->
            evaluate exspression
              (S_expr
                 {
                   current = pair_lis_frames parameters values;
                   next = new_association_list;
                 })
        | _ ->
            evaluate exspression
              (S_expr
                 {
                   current =
                     pair_lis_frames
                       (S_expr { current = parameters; next = Nil })
                       values;
                   next = new_association_list;
                 }))
    | _ -> raise (Failure "Error wrong exp")
  in

  evaluate exp assoscations

let get_first_string (expression : s_expression) : string =
  match expression with
  | SString { value } -> value
  | _ -> raise (WrongVariable "Exspression should be String")

let handle_expression (expression : s_expression)
    (association_list : s_expression) : s_expression =
  match expression with
  | Nil ->
      Printf.printf "The end =)\n";
      association_list
  | S_expr
      {
        current = SString { value = "define" };
        next =
          S_expr
            { current = name; next = S_expr { current = lambda; next = Nil } };
      } ->
      Printf.printf "Defined %s " (get_first_string name);
      print_exsprassion lambda;

      S_expr
        {
          current =
            pair_lis_frames
              (S_expr { current = name; next = Nil })
              (S_expr { current = eval lambda association_list; next = Nil });
          next = association_list;
        }
  | exp ->
      print_exsprassion (eval exp association_list);
      association_list
(* | _ -> raise (WrongVariable "IDK actually") *)

let rec driver_loop (expression : s_expression)
    (association_list : s_expression) : unit =
  match expression with
  | Nil -> Printf.printf "The end =)\n"
  | S_expr { current = expression; next } ->
      let updated_association_list =
        handle_expression expression association_list
      in
      driver_loop next updated_association_list

let rec driver_loop_terminal (association_list : s_expression) : s_expression =
  Printf.printf ">> ";
  let expression =
    let input_expression_line = read_line () in
    if String.equal input_expression_line "exit" then Nil
    else
      try parsing input_expression_line with
      | Failure msg ->
          Printf.printf "Error happened: %s\n" msg;
          driver_loop_terminal association_list
      | _ ->
          Printf.printf "Unknown parsing error\n";
          driver_loop_terminal association_list
  in
  match expression with
  | Nil ->
      Printf.printf "The end\n";
      Nil
  | _ -> (
      try
        let updated_association_list =
          handle_expression expression association_list
        in
        driver_loop_terminal updated_association_list
      with
      | WrongVariable msg ->
          Printf.printf "Error: %s\n" msg;
          driver_loop_terminal association_list
      | Failure msg ->
          Printf.printf "Error during evaluation: %s\n" msg;
          driver_loop_terminal association_list
      | _ ->
          Printf.printf "Unknown error occurred\n";
          driver_loop_terminal association_list)
(* | _ -> raise (WrongVariable "IDK acttualy");; *)
