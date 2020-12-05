type trie = Trie of int option * char_to_children and 
            char_to_children = (char * trie) list

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

let print opti = match opti with 
| None -> Printf.printf "None"; None 
| Some(i) -> Printf.printf "i = %d" i;opti
;;

let rec tri_from_char trie c = match trie with  
  | Trie(someint, (a, strie)::rest) -> 
    if a = c then strie
    else tri_from_char (Trie(someint, rest)) c
	| Trie(_, []) -> Trie(None, [])
;;

let rec lookup_ls trie ws = match ws with 
  chr::restc -> 
    let foundtrie = tri_from_char trie chr in
    (match foundtrie with
      | Trie(someint, []) -> if List.length restc = 0 then someint else None
      | Trie(someint, ls) -> lookup_ls (Trie(someint, ls)) restc)
  | [] -> match trie with Trie(someint, _)-> someint
;;

let lookup trie w = 
  let chars= explode w in
  let someint = lookup_ls trie chars
  in print someint
;;

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf
;;

let print_space i = Printf.printf "\n%s" (String.make i ' ');;

let 
rec print_children ls i= match ls with 
  | (ch, trie)::restt -> print_space i; Printf.printf "('%c', " ch; print_tri trie (i+1); Printf.printf ");"; print_children restt i;
  | [] -> ();
and
print_tri tri i= match tri with
| Trie(someint, ls) -> 
    match someint with 
      | None ->    Printf.printf "Trie (None, [" ; print_children ls (i+1); Printf.printf "])";
      | Some(z) -> Printf.printf "Trie (%d, [" z;  print_children ls (i+1); Printf.printf "])";
;;

let rec upsert ls chrs k = 
  Printf.printf "chrs= %s\n" (string_of_chars chrs); print_children ls 0;
  match ls, chrs with  
  | (ch, trie)::restl, chr::restc -> 
      if ch = chr then (
        (* if List.length restc = 0 then
          match trie with Trie(_, grand) -> (ch, Trie(Some(k), grand))::restl
        else *)
        (ch, insert_trie trie restc k)::restl)
      else (ch, trie)::(upsert restl chrs k)
   | [], chr::restc -> (*if children_from_char ls chr = None then *)
                         (chr, Trie((if List.length restc = 0 then Some(k) else None) , upsert [] restc k))::[]
                       (* else ls *)
  | (ch, trie)::restl, [] -> (ch, trie)::restl
  | [], [] -> []
and
insert_trie tri ws k =
  Printf.printf "\nws= %s\n" (string_of_chars ws);
  print_tri tri 0; Printf.printf "\n\n";
  match tri with Trie(someint, children) -> 
    match ws with
    [] -> Trie(Some(k), children)
    | _ -> Trie(someint, upsert children ws k)
;; 

let insert trie w k =
  let ws = explode w in
  insert_trie trie ws k
;;

let input=   
  Trie (Some (-5),
    [('s', Trie (None, [('a', Trie (Some (-5), []))]));
     ('m',
      Trie (None,
       [('m', Trie (None, [('j', Trie (Some 1, []))]));
        ('j', Trie (Some 4, []))]))])
;;
let result = insert input "mmj" 4;;
print_tri input 1;;
Printf.printf "\nmmj 4\n\n";;
print_tri result 1;;

