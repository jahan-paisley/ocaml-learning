let id = function x -> x;;

let rec compose ls x = 
  match ls with 
    [] -> id x
  | hd::tl -> hd (compose tl x) 
;;


let rec fixedpoint f start delta = 
  let diff = abs_float(f(start) -. start) in
  if diff < delta then start
  else 
    fixedpoint f (f start) delta 
;;
