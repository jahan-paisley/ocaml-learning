let simplify = function
  | EMul (EInt 1, e) | EMul (e, EInt 1) | EAdd (EInt 0, e) | EAdd (e, EInt 0) -> e 
  | EMul (EInt 0, _) | EMul (_, EInt 0)  -> zero
  | e -> e

let only_small_lists ls = match ls with 
  | [] | [_] | [_;_] -> ls
  | _ as anything -> [];;

let rec no_consecutive_repetition = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: ys 
    when x = y ->
      no_consecutive_repetition (y :: ys) 
  |  x :: y :: ys ->
      x::no_consecutive_repetition (y :: ys)
