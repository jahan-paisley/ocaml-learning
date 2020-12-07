type 'a clist =
  | CSingle of 'a
  | CApp of 'a clist * 'a clist
  | CEmpty

let example =
  CApp (CApp (CSingle 1,
              CSingle 2),
        CApp (CSingle 3,
              CApp (CSingle 4, CEmpty)))
;;

let rec append l1 l2 = match l1, l2 with 
  | x::xs, l2 -> x::(append xs l2)
  | [], l2 -> l2
;;

let result = append [1;2;3] [4;5;6];;
let rec print_ls l= match l with 
  x::xs -> Printf.printf "%i" x; print_ls xs;
  | [] -> ()
;;

print_ls result;; Printf.printf "\n";;
(* Write a function to_list : 'a clist -> 'a list 
   which computes the 'a list that contains the same elements as the input 'a clist, in the same order. *)
let rec to_list l = match l with 
  | CApp(a , b) -> append (to_list a) (to_list b)
  | CSingle(el) -> [el]
  | CEmpty -> []
;;


(* Write a function of_list : 'a list -> 'a clist which computes 
   the 'a clist that contains the same elements as the input 'a list, in the same order. *)
let rec of_list l =
  match l with 
    | x::xs -> CApp(CSingle(x), of_list xs)
    | [] -> CEmpty
;;

let rec print_capp capp = match capp with 
  | CApp(a, b) -> Printf.printf "CApp(";print_capp a;Printf.printf ";";print_capp b;Printf.printf ")";
  | CSingle(x) -> Printf.printf "CSingle(";Printf.printf "%d" x;Printf.printf ")";
  | CEmpty -> ()
;;

let res1 = of_list [1;2;3;4];;
print_capp res1;;
Printf.printf "\n"
let res2 = of_list [1;2;3;4;5];;
print_capp res2;;

let append l1 l2 =
  match l1,l2 with
  | CApp(a, b), CEmpty | CEmpty, CApp(a,b) -> CApp(a, b)
  | _,_ -> CApp(l1,l2)
;;

let rec hd l = match l with 
  CApp(a,b) -> let h1= (hd a) in if h1 != None then h1 else hd b
  | CSingle(x) -> Some(x)
  |CEmpty -> None
 ;;

 let rec tl_simple l = match l with 
  CApp(a,b) -> 
    if a = CEmpty && b = CEmpty then CEmpty
    else 
      let h1= (hd a) in 
      if h1 != None then CApp(tl_simple a, b)
      else 
        let tlb = tl_simple b in
        if tlb != CEmpty then CApp(CEmpty, tl_simple b)
        else CEmpty
 | CSingle(_) -> CApp(CEmpty, CEmpty)
 | CEmpty -> CEmpty


 let rec tl l = match tl_simple l with | CEmpty -> None | a -> Some(a);; 
 