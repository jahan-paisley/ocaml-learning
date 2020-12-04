let rec filter p l = match l with
  | [] -> []
  | x::xs -> if p x = true then x::(filter p xs) else filter p xs
;;

let partition p l =
  let negatedp x = if p x then false else true in
  (filter p l, filter negatedp l)
;;

let rec sort l= match l with 
  | [] -> []
  | [x] -> [x]
  | x::xs -> let (left, right) = partition (fun a -> a <= x) xs in
      let sorted_left = sort left in 
      let sorted_right = sort right in
      List.append sorted_left (x::sorted_right)
;;



partition (fun x -> x > 3) [2;3;6;7;8;1] ;;
sort [-3; 1; 4; -2; 0; -2; -2; 1] ;;