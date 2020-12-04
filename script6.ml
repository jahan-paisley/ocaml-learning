
open List

let rec generate l = match l with
  | [] -> [[]]
  | x::xs -> [x]::[xs]
;;

let rec gen_sub_size l 1= match l with
  | [] -> []
  | x::xs -> [x]::(gen_sub_size xs 1)
;;

let rec gen_sub_size l1 l2 2= match l1,l2 with
  | [],[] -> []
  | x::xs, y::ys -> if x = y then gen_sub_size l1 ys 2 else [x,y]::(gen_sub_size xs ys 2)
  | _, _ -> []
;;

(gen_sub_size [1;2;3] [1;2;3] 2);;

gen_sub_size [1;2;3] 