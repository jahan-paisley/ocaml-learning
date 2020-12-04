type 'a tree =
    Node of 'a tree * 'a * 'a tree
  | Leaf of 'a;;

let wrap l =
  List.map (function x -> [x]) l;;

let rec tree_map f = function t ->
  match t with
  | Node(left, root, right) -> Node(tree_map f left, f root, tree_map f right)
  | Leaf(v) -> Leaf(f v)
;;
  