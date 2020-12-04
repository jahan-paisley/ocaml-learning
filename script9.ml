let for_all p l = List.fold_left (fun acc x -> p x && acc) true l
;;

let exists p l = List.fold_left (fun acc x -> p x || acc) false l
;;

let sorted cmp l = match l with 
  | [] -> true
  | [_] -> true
  | x::y::zs -> List.fold_left (fun acc x-> if (cmp x y) < 0 then false else acc) true (y::zs)
;;


