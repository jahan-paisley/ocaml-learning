type trie = Trie of int option * char_to_children and 
            char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
		[('i', Trie (Some 11, [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
		 ('t', Trie (None,[('e', Trie (None,[('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));('a', Trie (Some 3, []))])); ('o', Trie (Some 7, []))]));
		 ('A', Trie (Some 15, []))])
;;   

(*
Write a function children_from_char : char_to_children -> char -> trie option such that
children_from_char m c = Some t if (c, t) is the first pair in m with c as a first component ;
children_from_char m c = None if no such pair exists in m.
*)
let rec children_from_char m c = match m with  
	| (a, t)::rest -> if a = c then Some(t) else children_from_char rest c
	| [] -> None	
;;

(*
Write a function update_children : char_to_children -> char -> trie -> char_to_children such that
	1. children_from_char (update_children m c t) c = Some t ;
	2. children_from_char (update_children m c t) c' = children_from_char m c' for c <> c';
	3. If children_from_char m c = Some t then List.length (update_children m c t') = List.length m.
*)
let rec 
find_in_trie m c = match m with
	| Trie(someint, (chr, strie)::rest) -> 
			if chr = c then true else find_in_trie strie c || find_in_trie (Trie(None, rest)) c
	| Trie(someint, []) -> false
and
update_trie tri c t= match tri with
  | Trie(someint, (chr, strie)::rest) -> 
      if chr = c then Trie((someint, (chr, t)::(update_children1 rest c t))) else Trie(someint, (chr, strie)::(update_children1 rest c t))
  | Trie(someint, []) -> Trie(someint, [])
and
update_children1 m c t =
	match m with
	| (chr, trie1)::rest -> if chr = c then (chr, t)::(update_children1 rest c t )
			else let newtrie = update_trie trie1 c t
				in (chr, newtrie)::(update_children1 rest c t )
	| [] -> [] 
and 
update_children m c t =
	if find_in_trie (Trie(None, m)) c = false then (c,t)::m 
	else update_children1 m c t
;;


update_children
  [('s', Trie (None, [('d', Trie (Some 3, []))]));
   ('m',
    Trie (Some (-4), [('g', Trie (None, [('a', Trie (Some (-1), []))]))]))]
  'g'
  (Trie (Some (-5),
    [('s', Trie (None, [('d', Trie (Some (-1), []))]));
     ('d', Trie (None, [('j', Trie (Some (-4), []))]));
     ('a',
      Trie (None,
       [('j', Trie (Some 3, []));
        ('d', Trie (None, [('g', Trie (Some 4, []))]))]));
     ('m', Trie (None, [('a', Trie (Some (-1), []))]));
     ('j',
      Trie (Some 2, [('s', Trie (None, [('p', Trie (Some (-1), []))]))]))]))

let lookup trie w =
"Replace this string with your implementation." ;;

let insert trie w v =
"Replace this string with your implementation." ;;
  