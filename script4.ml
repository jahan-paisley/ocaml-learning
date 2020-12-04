type operation =
    Op of string * operation * operation
  | Value of int

type env = (string * (int -> int -> int)) list

let rec lookup_function n = function env ->
  match env with
    [] -> invalid_arg "lookup_function"
  | (name, func)::xs when name = n -> func
  | (name, func)::xs when name != n -> lookup_function n xs
  | _::_ -> invalid_arg "lookup_function"
;;

let add_function name op env =
  (name, op)::env
;;

let my_env=
  [("min", fun a b -> if a < b then a else b);
   ("mul", fun a b -> a * b);
   ("sub", fun a b -> a - b);
   ("div", fun a b -> a / b);
   ("add", fun a b -> a + b)];; 


let rec compute env op = match op with
  | Op(name, op1, op2) -> 
      let func = lookup_function name my_env
      and eval1 = compute env op1
      and eval2 = compute env op2 in
      func eval1 eval2
  | Value(num) -> num
;;

let rec compute_eff env = function _ ->
  "Replace this string with your implementation"
