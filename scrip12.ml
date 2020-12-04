type trie = Trie of int option * char_to_children and 
            char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let rec children_from_char m c = match m with  
	| (a, t)::rest -> if a = c then Some(t) else children_from_char rest c
	| [] -> None	
;;

let rec 
  update_children1 m c t =
  match m with
  | (chr, trie1)::rest -> if chr = c then (chr, t)::rest
      else (chr, trie1)::(update_children1 rest c t )
  | [] -> [] 
and 
  update_children m c t =
  if children_from_char m c = None then (c,t)::m 
  else update_children1 m c t
;;

let rec explode ch = match ch with
  | "" -> []
  | ch -> (String.get ch 0 ) :: (explode (String.sub ch 1 ( (String.length ch)-1) ) )  
;;

let rec to_str char_ls = match char_ls with
    [] -> ""
  | x::xs -> Char.escaped x ^ (to_str xs)
;;
  
let rec lookup_ls trie w = 
  match trie with 
  | Trie(someint, (ch, strie)::restl) -> 
      (match w with 
       | chr::restc -> 
           if ch = chr then lookup_ls strie restc else lookup_ls (Trie(someint, restl)) restc
       | [] -> someint)
  | Trie(someint, []) -> if List.length w = 0 then someint else None
;;

let lookup trie w = 
  let chars= explode w in
  lookup_ls trie chars
;;


let insert trie w v =
"Replace this string with your implementation." ;;
  