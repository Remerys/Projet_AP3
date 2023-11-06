#load "btreeS.cmo";;

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

let rec btree_to_string(bt_tree : string btreeS) : string =
  "(" ^ btreeS_root(bt_tree) ^ "," ^ btree_to_string(btreeS_subleft(bt_tree)) ^ "," ^ btree_to_string(btree_subright(bt_tree)) ^ ")"
;;


let tEmpty : int btreeS = btreeS_empty();;

let tLeaf : int btreeS = btreeS_rooting(1, tEmpty, tEmpty);;

let tTree : int btreeS = btreeS_rooting(2, tLeaf, tEmpty);;

let tTree2 : int btreeS = btreeS_rooting(3, tTree, tLeaf);;

size(tEmpty);;
size(tLeaf);;
size(tTree);;
size(tTree2);;

height(tEmpty);;
height(tLeaf);;
height(tTree);;
height(tTree2);;

btree_to_string(tLeaf);;

