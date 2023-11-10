(*#load "btreeS.cmo";;*)
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

let min(a, b : 'a * 'a) : 'a =
  if a < b
  then a
  else b
;;

let rec btreeS_min(tree : 'a btreeS) : 'a =
  if btreeS_isempty(tree)
  then failwith("btreeS_min : btree is empty")
  else
    let root : 'a = btreeS_root(tree)
    and (g, d) : 'a btreeS * 'a btreeS = (btreeS_subleft(tree), btreeS_subright(tree))
    in
    if not(btreeS_isempty(g)) && not(btreeS_isempty(d))
    then
      let min_left : 'a = btreeS_min(g)
      and min_right : 'a = btreeS_min(d) in
      min(root, min(min_left, min_right))
    else
      if not(btreeS_isempty(g))
      then min(root, btreeS_min(g))
      else
        if not(btreeS_isempty(d))
        then min(root, btreeS_min(d))
        else root
;;

let rec btreeS_max(tree : 'a btreeS) : 'a =
  if btreeS_isempty(tree)
  then failwith("btreeS_max : btree is empty")
  else
    let root : 'a = btreeS_root(tree)
    and (g, d) : 'a btreeS * 'a btreeS = (btreeS_subleft(tree), btreeS_subright(tree))
    in
    if not(btreeS_isempty(g)) && not(btreeS_isempty(d))
    then
      let max_left : 'a = btreeS_max(g)
      and max_right : 'a = btreeS_max(d) in
      max(root, max(max_left, max_right))
    else
      if not(btreeS_isempty(g))
      then max(root, btreeS_max(g))
      else
        if not(btreeS_isempty(d))
        then max(root, btreeS_max(d))
        else root
;;

let rec height(bt_tree : 'a btreeS) : int =
  if (btreeS_isempty(bt_tree))
  then -1 (*not defined*)
  else 1 + max( height(btreeS_subleft(bt_tree)), height(btreeS_subright(bt_tree)))
;;

let rec btree_to_string_int(bt_tree : int btreeS) : string =
  if btreeS_isempty(bt_tree)
  then "EMPTY"
  else
  "(" ^ string_of_int(btreeS_root(bt_tree)) ^ "," ^ btree_to_string_int(btreeS_subleft(bt_tree)) ^ "," ^ int_btree_to_string_int(btreeS_subright(bt_tree)) ^ ")"
;;

let rec btree_to_string(bt_tree : string btreeS) : string =
  if btreeS_isempty(bt_tree)
  then "EMPTY"
  else
  "(" ^ btreeS_root(bt_tree) ^ "," ^ btree_to_string(btreeS_subleft(bt_tree)) ^ "," ^ btree_to_string(btreeS_subright(bt_tree)) ^ ")"
;;

