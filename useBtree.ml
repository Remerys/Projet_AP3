open BtreeS;;
let rec size(bt_tree : 'a btreeS) : int =
  if(btreeS_isempty(bt_tree))
  then 0
  else 1 + size(btreeS_subleft(bt_tree)) + size(btreeS_subright(bt_tree))
;;

let max(a, b : int * int) : int =
  if (a > b)
  then a
  else b
;;

let rec height(bt_tree : 'a btreeS) : int =
  if (btreeS_isempty(bt_tree))
  then -1 (*not defined*)
  else 1 + max( height(btreeS_subleft(bt_tree)), height(btreeS_subright(bt_tree)))
;;

let rec btree_to_string(bt_tree : int btreeS) : string =
  if btreeS_isempty(bt_tree)
  then "empty"
  else
  "(" ^ string_of_int(btreeS_root(bt_tree)) ^ "," ^ btree_to_string(btreeS_subleft(bt_tree)) ^ "," ^ btree_to_string(btreeS_subright(bt_tree)) ^ ")"
;;
