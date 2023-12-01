(*#load "btreeS.cmo";;*)
open BtreeS;;
let rec size(bt_tree : 'a t_bt) : int =
  if(bt_isempty(bt_tree))
  then 0
  else 1 + size(bt_subleft(bt_tree)) + size(bt_subright(bt_tree))
;;

let max(a, b : 'a * 'a) : 'a =
  if (a > b)
  then a
  else b
;;

let min(a, b : 'a * 'a) : 'a =
  if a < b
  then a
  else b
;;

let rec bt_min(tree : 'a t_bt) : 'a =
  if bt_isempty(tree)
  then failwith("bt_min : btree is empty")
  else
    let root : 'a = bt_root(tree)
    and (g, d) : 'a t_bt * 'a t_bt = (bt_subleft(tree), bt_subright(tree))
    in
    if not(bt_isempty(g)) && not(bt_isempty(d))
    then
      let min_left : 'a = bt_min(g)
      and min_right : 'a = bt_min(d) in
      min(root, min(min_left, min_right))
    else
      if not(bt_isempty(g))
      then min(root, bt_min(g))
      else
        if not(bt_isempty(d))
        then min(root, bt_min(d))
        else root
;;

let rec bt_max(tree : 'a t_bt) : 'a =
  if bt_isempty(tree)
  then failwith("bt_max : btree is empty")
  else
    let root : 'a = bt_root(tree)
    and (g, d) : 'a t_bt * 'a t_bt = (bt_subleft(tree), bt_subright(tree))
    in
    if not(bt_isempty(g)) && not(bt_isempty(d))
    then
      let max_left : 'a = bt_max(g)
      and max_right : 'a = bt_max(d) in
      max(root, max(max_left, max_right))
    else
      if not(bt_isempty(g))
      then max(root, bt_max(g))
      else
        if not(bt_isempty(d))
        then max(root, bt_max(d))
        else root
;;

let rec height(bt_tree : 'a t_bt) : int =
  if (bt_isempty(bt_tree))
  then -1 (*not defined*)
  else 1 + max( height(bt_subleft(bt_tree)), height(bt_subright(bt_tree)))
;;

let rec btree_to_string_int(bt_tree : int t_bt) : string =
  if bt_isempty(bt_tree)
  then "EMPTY"
  else
  "(" ^ string_of_int(bt_root(bt_tree)) ^ "," ^ btree_to_string_int(bt_subleft(bt_tree)) ^ "," ^ btree_to_string_int(bt_subright(bt_tree)) ^ ")"
;;

let rec btree_to_string(bt_tree : string t_bt) : string =
  if bt_isempty(bt_tree)
  then "EMPTY"
  else
  "(" ^ bt_root(bt_tree) ^ "," ^ btree_to_string(bt_subleft(bt_tree)) ^ "," ^ btree_to_string(bt_subright(bt_tree)) ^ ")"
;;

